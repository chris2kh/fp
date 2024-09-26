#lang racket

(require "helper.rkt")
(require "variable.rkt")
(require "lambda.rkt")
(require "evaluator.rkt")

(provide let?)
(provide eval-let)

(define (let? exp) (tagged-list? exp 'let))

(define var car)
(define value cadr)
(define (bindings exp) (cadr exp))
(define (body exp) (cddr exp))

(define (eval-let exp env)
  (if (named-let? exp)
      (handle-named-let exp env)
      (handle-regular-let exp env)))

; named let
(define (named-let? exp) (and (= 4 (length exp)) (not (variable? (caddr exp)))))
(define (handle-named-let exp env)
  (define (bindings exp) (caddr exp))
  (define (variable exp) (cadr exp))
  (define (body exp) (cadddr exp))

  (evall
    (list 'begin 
          (list 'define (variable exp) (make-lambda (map var (bindings exp)) (body exp)))
          (cons (variable exp) (map value (bindings exp))))
    env))


; regular let
(define (handle-regular-let exp env)
  (evall (cons 
          (list 'lambda (map var (bindings exp)) (body exp)) 
          (map value (bindings exp))) 
         env))


; let *
(define (let* exp) (tagged-list? exp 'let*))

(define (let*->nested-lets bindings body)
  (if (null? bindings)
      body 
      (cons 'let (cons (first bindings) (let*->nested-lets (rest bindings) body)))))

(define (eval-let* exp env)
  (evall (let*->nested-lets (bindings exp) (body exp)) env)) 