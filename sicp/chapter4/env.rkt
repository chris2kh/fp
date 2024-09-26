#lang racket

(provide lookup-variable)

(define (lookup-variable var env)
  (if (null? env) 
      (error "unbound var")
      (let ((result (lookup-inner var (car env))))
        (if (null? result)
            (lookup-variable var (cdr env))
            result))))

(define (set-variable! var val env)
  (if (null? env) 
      (error "unbound var")
      (let ((result (set-inner! var val (car env))))
        (if (null? result)
            (set-variable! var val (cdr env))
            result))))

(define (set-inner! var bindings)
  (cond ((null? bindings) '())
        ((eq? var (caar bindings)) (set-car! bindings (cons (carr bindings) val)) 'ok)
        (else (lookup-inner var (cdr bindings)))))


(define (lookup-inner var bindings)
  (cond ((null? bindings) '())
        ((eq? var (caar bindings)) (cdar bindings))
        (else (lookup-inner var (cdr bindings)))))

(define (extend-environment variables values parent-env)
  (cons (map cons variables values) parent-env)) 

(define (define-variable! var value env)
  (cons (cons (cons var value) (car env)) (cdr env)))


(define (enclosing-environment env) (cdr env))
(define (first-environment env) (car env))
(define the-empty-environment '())


(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-bindings-to-frame! var val frame)
  (begin
      (set-car! frame (cons var (car frame)))
      (set-cdr! frame (cons val (cdr frame)))))


(define (make-frame variables values) (cons variables values))