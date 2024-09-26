#lang racket

(define (fringe xs)
  (cond
    ((null? xs) `())
    ((list? (car xs)) (append (fringe (car xs)) (fringe (cdr xs))) )
    (else (cons (car xs) (fringe (cdr xs))))))


(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))