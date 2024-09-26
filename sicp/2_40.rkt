#lang racket

(define (foldr z f xs)
  (if (null? xs)
      z
      (f (car xs) (foldr z f (cdr xs)))))


(define (concat xs)
  (foldr `() append xs))

(define (enumerate-interval a b)
  (if (= a b)
      `()
      (cons a (enumerate-interval (+ a 1) b))))


(define (flatMap f seq)
  (concat (map f seq)))

(define (unique-pairs n)
  (flatMap
   (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 i)))
   (enumerate-interval 1 n)))

;(unique-pairs 5)


;2.41
(define (sum-triple n s)
  (filter
   (lambda (triple) (= s (foldr 0 + triple)))
   (flatMap
    (lambda (i) (map (lambda (pair) (cons i pair)) (unique-pairs i))) (enumerate-interval 1 n))))

;(sum-triple 20 15)


(define (tuples k n)
  (if (= k 0)
      (list `())
      (flatMap
       (lambda (i) (map (lambda (xs) (cons i xs)) (tuples (- k 1 ) i))) (enumerate-interval 1 n))))


