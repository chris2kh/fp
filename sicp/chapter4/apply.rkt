#lang racket

(require "helper.rkt")
(require "evaluator.rkt")
(require "lambda.rkt")

(provide apply?)
(provide eval-apply)

(define apply? pair?)

(define (operator exp) (car exp))
(define (args exp) ())
(define (evaluated-args args env)
  (if (null? args)
      '()
      (cons (evall (firs args) env)
            (evaluated-args (rest args) env))))


(define (eval-apply exp env)
  (apply (evall (operator exp env)) (evluated-args (args exp) env)))

(define (apply procedure args)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure args))
        ((compound-procedure? procedure)
         (evall (cons 'begin (procedure-body procedure))
            (extend-environment
                  (procedure-parameters procedure)
                  args
                  (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (compound-procedure? exp) (tagged-list? exp 'procedure))
(define (procedure-body procedure) (cadr procedure))
(define (procedure-parameters procedure) (caddr procedure))
(define (procedure-environment procedure) (cadddr procedure))

(define (primitive-procedure? foo) 1)
(define (apply-primitive-procedure procedure arguments) 1)
(define (extend-environment a b c) 1)