#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide lambda?)
(provide eval-lambda)
(provide make-lambda)

(define (lambda? exp) (tagged-list? exp  'lambda))

(define (args exp) (cadr exp))
(define (body exp) (cddr exp))

(define (eval-lambda exp env)
  (make-procedure (args exp) (body exp) env))

(define (make-procedure args body env) (list 'procedure args body env))
(define (make-lambda args body) (list 'lambda args body))