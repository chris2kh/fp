#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide or?)
(provide eval-or)

(define (or? exp) (tagged-list? exp 'or))

(define (operands exp) (cdr exp))
(define (first exp) (car exp))
(define (rest exp) (cdr exp))

(define (eval-helper operands env)
  (cond ( (null? (car operands)) #f)
        ( (eval (car operands) env) #t)
        (else (eval-or (cdr operands) env))))

(define (eval-and exp env)
  (eval-helper (operands exp) env)) 
    