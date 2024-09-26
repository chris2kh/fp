#lang racket

(define (pow n k)
    (if (= k 0)
        1
        (* n (pow n (- k 1)))))


(pow 2 5)