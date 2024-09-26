#lang racket

(require "helper.rkt")
(require "lambda.rkt")
(require "evaluator.rkt")

(provide definition?)
(provide eval-definition)

(define (definition? exp) (tagged-list? exp  'define))
(define (constant-definition? exp) (symbol? (cadr exp)))
(define (register name value env) "to be implemented...")


; definition could be a definition of a constant (define <var> <value>)
; or a function (define (<var> args) (body))
; which is sintactic sugar for (define <var> (lambda (args) (body)))
(define (eval-definition exp env) 
  (if (constant-definition? exp)
      (register (constant-name exp) (evall (constant-value exp) env) env)
      (register (fun-name exp)      (evall (make-lambda (args exp) (body exp)) env))))


(define (constant-name exp) (cadr exp))
(define (constant-value exp) (caddr exp))

(define (fun-name exp) (cadr exp))
(define (args exp) (cdadr exp))
(define (body exp) (cddr exp))