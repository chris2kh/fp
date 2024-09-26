#lang racket

(define (foldr z f xs)
  (cond
    ((null? xs) `())
    (else (f (car xs) (foldr z f (cdr xs))))
  )
)

(define (filter p list)
  (define (helper x acc)
    (if (p x)
      (cons x acc)
      acc
    ))
  (foldr `() helper list)
)

(define (same-arity x . xs)
 (if (odd? x)
  (filter odd? xs)
  (filter even? xs)))

(same-arity 1 2 3 4 5 6 7)
(same-arity 2 2 3 4 5 6 7)