#lang racket/base

;main issue right now: how do i elegantly handle arrays like 0 0 $ 2?
;until that problem is solved, we can never have a true implementation of j
;maybe implement arrays as structs?

(require racket/list)
(require racket/vector)

(define (scanl op id ls)
  (if (empty? ls)
      (list id)
      (let [(recurse (scanl op id (cdr ls)))]
        (cons (op (car ls) (car recurse))
              recurse))))
;future work, for when i try to implement j structs as arrays
#|
(require (only-in srfi/1 every))
;is every element in vec of the same type?
(define (homogenous? vec)
  (let [(ls (vector->list vec))]
    (or (every integer? ls)
        (every char? ls)
        (and (every vector? ls)
             (every homogenous? ls)))))

(struct array (shape storage)
  #:mutable
  #:transparent
  #:guard
  (λ (shape storage type-name)
    (if (every integer? shape)
        (if (homogenous? storage)
            (values shape storage)
            (error type-name "non-homogenous storage: ~e" storage))
        (error type-name "shape contains non-integers: ~e" shape))))
|#
;end future work

(define-syntax-rule (vector-cons v vs)
  (vector-append (vector v) vs))

(define (scalar? noun)
  (or (number? noun)
      (char? noun)))

(define-syntax-rule (array? noun)
  (not (scalar? noun)))

(define-syntax-rule (empty-array? noun)
  (equal? noun #()))

;get the items of a noun
;if the noun is an atom, the only item is the atom itself
;if the noun is not an atom but an array of rank n
; the items are the n-1 cells
;table on pg39, j for c programmers
(define (items noun)
  (if (scalar? noun)
      noun
      (vector->list noun)))

;(rank #(#(1 2) #(3 4)))
;pg 41, j for c programmers
(define-syntax-rule (rank array)
  (tally (shape array)))

;shape of an item of y
(define (shape-of-item y)
  (if (scalar? y)
      #()
      (shape (head y))))

;monad $
;(shape #(#(1 2) #(3 4)))
(define (shape noun)
  (cond [(scalar? noun) #()]
        [(equal? noun #()) #(0)]
        [else (vector-cons (vector-length noun)
                           (shape (vector-ref noun 0)))]))

;(cycle #(1 2 3 4) 7) ;=> '#(1 2 3 4 1 2 3)
;(cycle #(1 2 3) 0) ;=> '#()
;(cycle #(1 2 3) 2) ;=> '#(1 2)
(define (cycle noun times)
  (let [(len (vector-length noun))]
    (build-vector
     times
     (λ (i) (if (scalar? noun)
                noun
                (vector-ref noun (modulo i len)))))))

;dyad $
;pg 39, j for c programmers
;shape of result = x concatenated
; with shape of an item of y
;form an array with the above shape
;take items from y cyclically to fill the array until done
(define-syntax-rule (offsets ls)
  (cdr (scanl * 1 ls)))

(define-syntax-rule (get-index indices offsets)
  (apply + (map * indices offsets)))

(define-syntax-rule (get-element vec indices offsets)
  (vector-ref vec (modulo (get-index indices offsets)
                          (vector-length vec))))

(define (reshape x y)
  (let* [(xs (vector->list x))
         (offs (offsets xs))]
    (define (reshape-helper xs y indices)
      (if (= (length indices) (length xs))
          (get-element y (reverse indices) offs)
          (build-vector (list-ref xs (length indices))
                        (λ (j) (reshape-helper xs y (cons j indices))))))
    (reshape-helper xs y '())))

;monad #
;pg 44, j for c programmers
;returns a scalar equal to number of items in y
(define-syntax-rule (tally y)
  (if (scalar? y) 1 (vector-length y)))

;monad i.
;pg 44, j for c programmers
;i.y is the same as y $ ints, where ints is the list of all
;nonnegative integers in order
#|
(define (integers y)
  (if (scalar? y)
      (build-vector y (λ (i) i))
      (let [(num-items (apply * (vector->list 
                                 (reshape y (
|#

;monad #., verb-rank 1
;pg 47, j for c programmers
;if (shape y) is sy, then
;(shape (base-two y)) is sy with the last element dropped
(define (base-two y)
  ;rank 1 array to integer
  (define-syntax-rule (base-two-helper ls)
    (foldr (λ (curr accum) (+ curr (* 2 accum))) 0 ls))
  (cond [(scalar? y) y]
        [(> (rank y) 1) (vector-map base-two y)]
        [else (base-two-helper (reverse (vector->list y)))]))

;monad #:, verb-rank _
;pg 47, j for c programmers
(define (to-binary y)
  (define (to-binary-helper y)
    (cond [(zero? y) '()]
          [(even? y) (cons 0 (to-binary-helper (quotient y 2)))]
          [(odd? y) (cons 1 (to-binary-helper (quotient y 2)))]))
  (list->vector (reverse (to-binary-helper y))))

(define (antibase-two y)
  (let [(unpadded (if (scalar? y)
                      (to-binary y)
                      (vector-map antibase-two y)))]
    (pad-all unpadded (max-list-length unpadded))))

;in j terminology, list = an array of rank 1
;this function returns the length of the rank 1 cells of an array
(define (max-list-length y)
  (cond [(scalar? y) (error "not applicable")]
        [(> (rank y) 1) (max-list-length (vector-last y))]
        [else (vector-length y)]))

(define-syntax-rule (vector-last y)
  (vector-ref y (- (vector-length y) 1)))

(define (pad-all v len)
  (cond [(scalar? v) v]
        [(> (rank v) 1) (vector-map (λ (w) (pad-all w len)) v)]
        [else (pad v len)]))

(define-syntax-rule (pad v len)
  (let [(zeroes (make-vector (- len (vector-length v)) 0))]
    (vector-append zeroes v)))

;monad {.
;when called on an empty list, returns 0... why does j do this?
(define-syntax-rule (head y)
  (if (empty-array? y) 0 (vector-ref y 0)))

;dyad {
;(define-syntax-rule (index x y)
;  (vector-ref y x))

;copula =.
;need to figure out how to use macros to achieve this
;(define (copula x y)
;  (define x y))

;(get #(#(1 2) #(3 4)) '(1 1))
;(get 5 '())
(define (get array indices)
  (if (empty? indices)
      array
      (get (vector-ref array (car indices))
           (cdr indices))))

(define (prefix? l1 l2)
  (cond [(empty? l1) #t]
        [(equal? (car l1) (car l2))
         (prefix? (cdr l1) (cdr l2))]))

(define (remove-prefix l1 l2)
  (if (empty? l1)
      l2
      (remove-prefix (cdr l1) (cdr l2))))

; (suffix? '() '(1 2 3 4 5 6)) ;=> #t
; (suffix? '(4 5 6) '(1 2 3 4 5 6)) ;=> #t
; (suffix? '(4 5) '(1 2 3 4 5 6)) ;=> #f
(define (suffix? l1 l2)
  (let [(len1 (length l1))
        (len2 (length l2))]
    (cond [(< len1 len2) (suffix? l1 (cdr l2))]
          [(> len1 len2) #f]
          [else (equal? l1 l2)])))

; assumes that (suffix? l1 l2) is true
(define (remove-suffix l1 l2)
  (let [(len1 (length l1))
        (len2 (length l2))]
    (cond [(< len1 len2) (cons (car l2) (remove-suffix l1 (cdr l2)))]
          [(> len1 len2) '()]
          [else '()])))

;; take and drop
(define atake vector-take) ; array take
(define atake-right vector-take-right) ; array take right
(define adrop vector-drop) ; array drop
(define adrop-right vector-drop-right) ; array drop right

;; reverse a vector
(define (areverse vec)
  (let [(len (vector-length vec))]
    (build-vector len (λ (i) (vector-ref vec (- len i 1))))))

;; rotate a vector
(define (rotate vec)
  (let [(len (vector-length vec))]
    (build-vector len (λ (i) (vector-ref vec (- len i 1)))))) ;; TODO

#|
(define (promote a1 a2)
  (if (suffix? (shape a1) (shape a2))

(define (get-cell...
|#



;(a+ #(#(1 2) #(3 4)) #(#(1 2) #(3 5)))
(define (a+ a1 a2)
  (if (equal? (shape a1) (shape a2))
      (if (scalar? a1)
          (+ a1 a2)
          (vector-map a+ a1 a2))
      1 ;todo
      ))

;promotion