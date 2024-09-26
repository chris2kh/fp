#lang racket

(define (reverse xs)
  (deep-reverse xs)
)

(define (deep-reverse xs)
  (define (iter xs acc)
    (cond 
      ((null? xs) acc)
      ((list? (car xs)) (iter (cdr xs) (cons (deep-reverse (car xs)) acc)))
      (else (iter (cdr xs) (cons (car xs) acc)))
    )
  )
  (iter xs `())
)

(deep-reverse (list 1 2 3 4))
(deep-reverse (list (list 1 2) (list 3 4)))

