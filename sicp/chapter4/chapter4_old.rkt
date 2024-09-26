#lang racket

; eval
(define (lookup-variable-value exp env) 1)
(define (make-procedure foo bar zoo) 1)
(define (primitive-procedure? foo) 1)
(define (compound-procedure? foo) 1)
(define (apply-primitive-procedure procedure arguments) 1)
(define (procedure-body procedure) 1)
(define (extend-environment a b c) 1)
(define (procedure-parameters procedure) 1)
(define (procedure-environment procedure) 1)
(define (true? exp) 1)
(define (set-variable-value! a b c ) 1)
(define (define-variable! a b c ) 1)
(define (first-operands exp) 1)
(define (put key1 key2 op) 'ok)
(define (get key1 key2) 'ok)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ;((and? exp) (eval-and (operands exp) env))
        ;((or? exp) (eval-or (operands exp) env))
        ((and? exp) (eval (and->if (operands exp)) env))
        ((or? exp) (eval (or->if (operands exp)) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))



; apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

; procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; definitions
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)



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

; Representing Expressions

; numbers and strings are the only elements self evaluated
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; a variables is a symbol
(define (variable? exp) (symbol? exp))

; quote
(define (quoted? exp) (tagged-list? exp 'quote))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? tag (car exp))
      false))
(define (text-of-quotation exp) (cdr exp))

; assignment
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definition
; definition could be a definition of a variable (define <var> <value>)
; or a function (define (<var> args) (body))
; which is sintactic sugar for (define <var> (lambda (args) (body)))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

; lambdas
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

; conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; We also provide a constructor for if expressions, to be used by
; cond->if to transform cond expressions into if expressions:
(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; We also include a constructor sequence->exp (for use by cond-
; if) that transforms a sequence into a single expression, using
; begin if necessary:
; TODO entender cómo se usa ?? R// es porque la accion de un predicado verdadero en cond
; no obligatoramiente es una sola expresion, puede ser una secuencia de expresiones
; ej: cond ((= x 2) x) <---- una sola expresion luego del predicado
;     cond ((= x 2) (display "hola mundo") x) <--- dos expresiones luego del predicado
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; applications
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; derived expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (test-clause? clause) (tagged-list? (cadr clause) '=>))
(define (test clause) (car clause))
(define (recipient clause) (caddr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (test-clause? first)
                       (cons (recipient first) (test first))   
                       (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

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

(define (eval2 exp env)
  (let ((fun (get 'eval (operator3 exp))))
    (cond ( (self-evaluating? exp) exp)
          ( (variable? exp) (lookup-variable-value exp env))
          ( fun (fun (operands3 exp) env))
          ( (application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
          (else (error "Unknown expression type: EVAL" exp)))))


; 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))


(define (eval-and operands env)
  (cond ( (null? (car operands)) #t)
        ( (eval (car operands) env)  (eval-and (cdr operands) env))
        (else #f)))
        
(define (eval-or operands env)
  (cond ( (null? (car operands)) #f)
        ( (eval (car operands) env) #t)
        (else (eval-or (cdr operands) env))))

(define (and->if exp)
  (if (null? (car operands))
      #t
      (make-if (car operands)
               (and->if (cdr exp))
               #f)))

(define (or->if exp)
  (if (null? (car operands))
      #f
      (make-if (car operands)
               #t 
               (or->if (cdr exp)))))

; 4.5
