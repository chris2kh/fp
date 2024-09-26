#lang racket

(require "helper.rkt")
(require "self-evaluating.rkt")
(require "variable.rkt")
(require "quote.rkt")
(require "assignment.rkt")
(require "definition.rkt")
(require "if.rkt")
(require "lambda.rkt")
(require "begin.rkt")
(require "cond.rkt")
(require "and.rkt")
(require "or.rkt")
(require "apply.rkt")

(provide evall)

; eval
(define (put key1 key2 op) 'ok)
(define (get key1 key2) 'ok)

(define (evall exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)    (eval-variable exp env))
        ((quoted? exp)      (eval-quoted exp env))
        ((assignment? exp)  (eval-assignment exp env))
        ((definition? exp)  (eval-definition exp env))
        ((if? exp)          (eval-if exp env))
        ((lambda? exp)      (eval-lambda exp env))
        ((begin? exp)       (eval-begin exp env))
        ((cond? exp)        (eval-cond exp env))
        ((and? exp)         (eval-and exp env))
        ((or? exp)          (eval-or exp env))
        ((let? exp)         (eval-let exp env))
        ((apply? exp)       (eval-apply exp env))
        (else
         (error "Unknown expression type: EVAL" exp))))



; 4.1
(define (list-of-values-left-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-right-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operands exps) env) rest))))


; 4.2 instead of  function application being (function args)
; it would be (call function args) in this case
(define (application2? exp) (tagged-list? exp 'call))
(define (operator2 exp) (cadr exp))
(define (operands2 exp) (cddr exp))

; 4.3
; put in a map with key1 = 'eval and key2= (car exp) (eg: define, if, lambda, cond, etc)
(put 'op 'quote text-of-quotation)
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (x y) (make-procedure (lambda-parameters x) (lambda-body x) y)))
(put 'op 'begin (lambda (x y) (eval-sequence (eval x) y)))
(put 'op 'cond (lambda (x y) (eval (cond->if x) y)))
(define (operator3 exp) (car exp))
(define (operands3 exp) (car exp))