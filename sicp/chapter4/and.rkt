#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide and?)
(provide eval-and)

(define (and? exp) (tagged-list? exp 'and))

(define (operands exp) (cdr exp))
(define (first exp) (car exp))
(define (rest exp) (cdr exp))

(define (eval-helper operands env)
  (cond ( (null? (first operands)) #t)
        ( (true? (evall (first operands) env)) (eval-helper (rest operands) env))
        (else #f)))

(define (eval-and exp env)
  (eval-helper (operands exp) env)) 
    