#lang racket

(provide self-evaluating?)
(define (self-evaluating? exp) (or (number? exp) (string? exp)))