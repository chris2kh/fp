#lang racket
(define (square x) 
    (* x x))

(define (sum-squares x y)
    (+ (square x) (square y)))

(define (sum-squares-first2 x y z)
    (cond ((and (>= x y) (>= y z)) (sum-squares x y))
          ((and (>= x y) (>= z y)) (sum-squares x z))
          (else (sum-squares y z))))

(sum-squares-first2 10 20 30)
(sum-squares-first2 5 5 3)
(sum-squares-first2 5 3 3)