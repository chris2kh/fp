
#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide cond?)
(provide eval-cond)

(define (cond? exp) (tagged-list? exp 'cond))


(define (clauses exp) (cdr exp))
(define (first-clause exp) (car exp))
(define (more? exp) (not (null? (cdr exp))))

(define (predicate clause) (car clause))
(define (actions clause) (cdr clause))

(define (eval-helper clauses env)
  (cond ((and (tagged-list? (first-clause clauses) 'else) (more? clauses))
                                  (error "else should be the last clause"))
        ((tagged-list? (first-clause clauses) 'else) (evall (actions clause) env))
        ((true? (predicate (first-clause clauses))) (evall (actions clause) env))
        (else (eval-helper (cdr clauses) env))))

(define (eval-cond exp env)
  (eval-helper (clauses exp) env)) 
    