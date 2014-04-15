#lang racket

(define scalar? number?)

(define (array? a) (not (scalar? a)))

;(shape #(#(1 2) #(3 4)))
(define (shape array)
  (if (scalar? array)
      '()
      (cons (vector-length array)
            (shape (vector-ref array 0)))))

;(dimension #(#(1 2) #(3 4)))
(define (dimension array)
  (if (scalar? array)
      0
      (+ 1 (dimension (vector-ref array 0)))))

;(get #(#(1 2) #(3 4)) '(1 1))
;(get 5 '())
(define (get array indices)
  (if (empty? indices)
      array
      (get (vector-ref array (car indices)) (cdr indices))))

(define slice vector-ref)
(define slices vector->list)

(define (prefix? l1 l2)
  (if (empty? l1)
      #t
      (prefix? (car l1) (car l2))))

(define (remove-prefix l1 l2)
  (if (empty? l1)
      l2
      (remove-prefix (cdr l1) (cdr l2))))

(define (promote a1 a2)
  (if (prefix? (shape a1) (shape a2))

;(a+ #(#(1 2) #(3 4)) #(#(1 2) #(3 5)))
(define (a+ a1 a2)
  (if (equal? (shape a1) (shape a2))
      (if (scalar? a1)
          (+ a1 a2)
          (vector-map a+ a1 a2))
      1 ;todo
      ))

;promotion