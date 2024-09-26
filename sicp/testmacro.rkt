#lang racket
(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))