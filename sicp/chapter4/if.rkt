#lang racket

(require "helper.rkt")
(require "evaluator.rkt")

(provide if?)
(provide eval-if)

(define (if? exp) (tagged-list? exp 'if))

(define (predicate exp) (cadr exp))
(define (if-path exp) (caddr exp))
(define (else-path exp) (caddr exp))

(define (eval-if exp env)
  (if (true? (eval (predicate exp) env))
      (eval (if-path exp) env)
      (eval (else-path exp) env)))


; optional if following sicp where cond is not a special form but syntactic sugar
; We also provide a constructor for if expressions, to be used by
; cond->if to transform cond expressions into if expressions:
;(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))

;(define (sequence->exp seq)
;  (cond ((null? seq) seq)
;        ((last-exp? seq) (first-exp seq))
;        (else (make-begin seq))))
;(define (make-begin seq) (cons 'begin seq))
;
;; derived expressions
;(define (cond? exp) (tagged-list? exp 'cond))
;(define (cond-clauses exp) (cdr exp))
;(define (cond-else-clause? clause)
;  (eq? (cond-predicate clause) 'else))
;(define (cond-predicate clause) (car clause))
;(define (cond-actions clause) (cdr clause))
;(define (cond->if exp) (expand-clauses (cond-clauses exp)))
;(define (test-clause? clause) (tagged-list? (cadr clause) '=>))
;(define (test clause) (car clause))
;(define (recipient clause) (caddr clause))
;(define (expand-clauses clauses)
;  (if (null? clauses)
;      'false ; no else clause
;      (let ((first (car clauses))
;            (rest (cdr clauses)))
;        (if (cond-else-clause? first)
;            (if (null? rest)
;                (sequence->exp (cond-actions first))
;                (error "ELSE clause isn't last: COND->IF"
;                       clauses))
;            (make-if (cond-predicate first)
;                     (if (test-clause? first)
;                       (cons (recipient first) (test first))   
;                       (sequence->exp (cond-actions first)))
;                     (expand-clauses rest))))))