#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide begin?)
(provide eval-begin)

(define (begin? exp) (tagged-list? exp 'begin))
(define (last? exp) (null? (cdr exp)))
(define (first exp) (car exp))


(define (sequence exp) (cdr exp))
(define (eval-helper exp env)
  (cond ((last? exp) (evall (first exp) env))
        (else (evall (first exp) env) (eval-helper (cdr exp) env))))

(define (eval-begin exp env)
  (eval-helper (sequence exp) env)) 
    