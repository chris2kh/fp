#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide assignment?)
(provide eval-assignment)

(define (assignment? exp) (tagged-list? exp  'set-exp!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env) 
  (register ( assignment-variable exp)
            ( evall (assignment-value exp) env)
              env))

