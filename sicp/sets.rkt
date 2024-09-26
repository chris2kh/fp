#lang racket
(define (subsets s)
(if (null? s)
(list `())
(let ((rest (subsets (cdr s))))
(append rest (map (lambda (xs) (cons (car s) xs)) rest)))))


(subsets (list 1 2 3))