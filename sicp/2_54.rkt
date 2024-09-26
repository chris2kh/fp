#lang racket

(define (my-equal? l1 l2)
 (cond  ((and (null? l1) (null? l2)) #t)
        ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
        ((and (pair? l1) (pair? l2)) 
                        (and (my-equal? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2))))
        (else #f)))
      
(my-equal? `(1 2 3 4) `(1 2 3 4))
(my-equal? `(1 (2 3) 4) `(1 (2 3) 4))