#lang racket
(define (change ammount denominations)
  (cond ((or (null? denominations) (< ammount 0)) 0)
        ((= 0 ammount) 1)
        (else
         (+  (change ammount (cdr denominations))
             (change (- ammount (car denominations)) denominations)))))


(define coin-denominations
  (list 50 25 10 5 1))

(change 100 coin-denominations)