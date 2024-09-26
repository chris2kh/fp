#lang racket

(define (accumulate f z xs)
  (if (null? xs)
    z
    (f (car xs) (accumulate f z (cdr xs)))))


; (list (1 2 3) (4 5 6) (7 8 9))
; (define (accumulate-n f z xs))  = (1 + 4 + 7) (2 + 5 + 8) (3 6 9)
(define (accumulate-n f z xs)
  (if (null? (car xs))
    `()
     (cons (accumulate (lambda (x acc) (f (car x) acc)) z xs)
           (accumulate-n f z (map cdr xs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (transpose xs)
 (accumulate-n cons `() xs))

(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) (transpose m)))

(define (matrix-*-matrix m1 m2)
  (map (lambda (row) (map (lambda (col) (dot-product col row)) (transpose m1))) m2))


(matrix-*-vector (list (list 10 20) (list 30 40)) (list 5 6))
