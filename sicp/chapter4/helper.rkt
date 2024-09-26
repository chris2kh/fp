#lang racket

(provide tagged-list?)
(provide register)
(provide first)
(provide rest)
(provide true?)
(provide false?)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? tag (car exp))
      false))


(define (register variable new-value env) "to be implemented...")

(define first car)
(define rest cdr)

(define (true? x) (not (false? x)))
(define (false? x) (eq? x false))
