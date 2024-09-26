#lang racket

(require "helper.rkt")
(provide quote?)
(provide eval-quoted)

(define (quote? exp) (tagged-list? exp 'quote))

; jan 20/23
; env not needed for this use case, but it's passed for uniformity of the specific eval apis
; and maybe in the future if env is needed for quoted expressions, then no need to modify 
; the evaluator code
(define (eval-quoted exp env) (text-of-quotation exp))
(define (text-of-quotation exp) (cdr exp))