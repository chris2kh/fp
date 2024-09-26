#lang racket

(provide variable?)
(provide eval-variable)

(define variable? symbol?)
(define eval-variable? lookup-variable-value)
(define (lookup-variable-value exp env) "to be implemented...")