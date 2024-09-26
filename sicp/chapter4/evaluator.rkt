#lang sicp

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
        ((unbound? exp)     (eval-unbound exp env))
        ((apply? exp)       (eval-apply exp env))
        (else
         (error "Unknown expression type: EVAL" exp))))


;******************************
;*    Helper functions        *
;******************************
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? tag (car exp))
      false))


(define first car)
(define rest cdr)

(define (true? x) (not (false? x)))
(define (false? x) (eq? x false))



;******************************
;*    Self evaluating         *
;******************************
(define (self-evaluating? exp) (or (number? exp) (string? exp)))


;******************************
;*    variable                *
;******************************
(define variable? symbol?)
(define (eval-variable var) (lookup-variable-value var))



;******************************
;*    quote                   *
;******************************
(define (quoted? exp) (tagged-list? exp 'quote))

; jan 20/23
; env not needed for this use case, but it's passed for uniformity of the specific eval apis
; and maybe in the future if env is needed for quoted expressions, then no need to modify
; the evaluator code
(define (eval-quoted exp env) (text-of-quotation exp))
(define (text-of-quotation exp) (cdr exp))


;******************************
;*    assignment              *
;******************************
(define (assignment? exp) (tagged-list? exp  'set-exp!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! ( assignment-variable exp)
                       ( evall (assignment-value exp) env)
                       env))


;******************************
;*    definition              *
;******************************
(define (definition? exp) (tagged-list? exp  'define))
(define (constant-definition? exp) (symbol? (cadr exp)))


; definition could be a definition of a constant (define <var> <value>)
; or a function (define (<var> args) (body))
; which is sintactic sugar for (define <var> (lambda (args) (body)))
(define (eval-definition exp env)
  (if (constant-definition? exp)
      (define-variable! (constant-name exp) (evall (constant-value exp) env) env)
      (define-variable! (fun-name exp)      (evall (make-lambda (define-args exp) (define-body exp)) env))))


(define (constant-name exp) (cadr exp))
(define (constant-value exp) (caddr exp))

(define (fun-name exp) (cadr exp))
(define (define-args exp) (cdadr exp))
(define (define-body exp) (cddr exp))


;******************************
;*    if                      *
;******************************
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-path exp) (caddr exp))
(define (else-path exp) (caddr exp))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (evall (if-path exp) env)
      (evall (else-path exp) env)))


; optional if following sicp where cond is not a special form but syntactic sugar
; We also provide a constructor for if expressions, to be used by
; cond->if to transform cond expressions into if expressions:
;(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))

;(define (sequence->exp seq)
;  (cond ((null? seq) seq)
;        ((last-exp? seq) (first-exp seq))
;        (else (make-begin seq))))
;(define (make-begin seq) (cons 'begin seq))
;
;; derived expressions
;(define (cond? exp) (tagged-list? exp 'cond))
;(define (cond-clauses exp) (cdr exp))
;(define (cond-else-clause? clause)
;  (eq? (cond-predicate clause) 'else))
;(define (cond-predicate clause) (car clause))
;(define (cond-actions clause) (cdr clause))
;(define (cond->if exp) (expand-clauses (cond-clauses exp)))
;(define (test-clause? clause) (tagged-list? (cadr clause) '=>))
;(define (test clause) (car clause))
;(define (recipient clause) (caddr clause))
;(define (expand-clauses clauses)
;  (if (null? clauses)
;      'false ; no else clause
;      (let ((first (car clauses))
;            (rest (cdr clauses)))
;        (if (cond-else-clause? first)
;            (if (null? rest)
;                (sequence->exp (cond-actions first))
;                (error "ELSE clause isn't last: COND->IF"
;                       clauses))
;            (make-if (cond-predicate first)
;                     (if (test-clause? first)
;                       (cons (recipient first) (test first))
;                       (sequence->exp (cond-actions first)))
;                     (expand-clauses rest))))))



;******************************
;*    lambda                  *
;******************************
(define (lambda? exp) (tagged-list? exp  'lambda))

(define (lambda-args exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (eval-lambda exp env)
  (make-procedure (lambda-args exp) (lambda-body exp) env))

(define (make-lambda args body) (list 'lambda args body))




;******************************
;*    begin                   *
;******************************
(define (begin? exp) (tagged-list? exp 'begin))
(define (last? exp) (null? (cdr exp)))


(define (sequence exp) (cdr exp))
(define (eval-begin-helper exp env)
  (cond ((last? exp) (evall (first exp) env))
        (else (evall (first exp) env) (eval-begin-helper (cdr exp) env))))

(define (eval-begin exp env)
  (eval-begin-helper (sequence exp) env))


;******************************
;*    cond                    *
;******************************
(define (cond? exp) (tagged-list? exp 'cond))


(define (clauses exp) (cdr exp))
(define (first-clause exp) (car exp))
(define (more? exp) (not (null? (cdr exp))))

(define (cond-predicate clause) (car clause))
(define (actions clause) (cdr clause))

(define (eval-cond-helper clauses env)
  (cond ((and (tagged-list? (first-clause clauses) 'else) (more? clauses))
         (error "else should be the last clause"))
        ((tagged-list? (first-clause clauses) 'else) (evall (actions (first-clause clauses) env)))
        ((true? (cond-predicate (first-clause clauses))) (evall (actions (first-clause clauses) env)))
        (else (eval-cond-helper (cdr clauses) env))))

(define (eval-cond exp env)
  (eval-cond-helper (clauses exp) env))


;******************************
;*    and                     *
;******************************
(define (and? exp) (tagged-list? exp 'and))

(define (and-operands exp) (cdr exp))

(define (eval-and-helper operands env)
  (cond ( (null? (first operands)) #t)
        ( (true? (evall (first operands) env)) (eval-and-helper (rest operands) env))
        (else #f)))

(define (eval-and exp env)
  (eval-and-helper (and-operands exp) env))


;******************************
;*    or                      *
;******************************
(define (or? exp) (tagged-list? exp 'or))

(define (or-operands exp) (cdr exp))

(define (eval-or-helper operands env)
  (cond ( (null? (car operands)) #f)
        ( (evall (car operands) env) #t)
        (else (eval-or-helper (cdr operands) env))))

(define (eval-or exp env)
  (eval-or-helper (or-operands exp) env))


;******************************
;*    let                     *
;******************************
(define (let? exp) (tagged-list? exp 'let))

(define var car)
(define value cadr)
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

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
          (list 'lambda (map var (let-bindings exp)) (let-body exp))
          (map value (let-bindings exp)))
         env))


; let *
(define (let* exp) (tagged-list? exp 'let*))

(define (let*->nested-lets bindings body)
  (if (null? bindings)
      body
      (cons 'let (cons (first bindings) (let*->nested-lets (rest bindings) body)))))

(define (eval-let* exp env)
  (evall (let*->nested-lets (let-bindings exp) (let-body exp)) env))


;******************************
;*    unbound                 *
;******************************
(define (unbound? exp) (tagged-list? exp 'make-unbound!))
(define (unbound-var exp) (cadr exp))
(define (eval-unbound exp env) (delete-from-env (unbound-var exp) env))


;******************************
;*    apply                   *
;******************************
(define apply? pair?)

(define (operator exp) (car exp))
(define (args exp) (cdr exp))
(define (evaluated-args args env)
  (if (null? args)
      '()
      (cons (evall (first args) env)
            (evaluated-args (rest args) env))))


(define (eval-apply exp env)
  (apply (evall (operator exp env)) (evaluated-args (args exp) env)))

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


(define (primitive-procedure? foo) 1)
(define (apply-primitive-procedure procedure arguments) 1)


(define (make-procedure args body env) (list 'procedure args body env))
(define (compound-procedure? exp) (tagged-list? exp 'procedure))
(define (procedure-body procedure) (cadr procedure))
(define (procedure-parameters procedure) (caddr procedure))
(define (procedure-environment procedure) (cadddr procedure))


;******************************
;*    environments            *
;******************************
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-bindings-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value variable env)
  (define (lookup-bindings variables values)
    (cond ((null? variables) (lookup-variable-value variable (enclosing-environment env)))
          ((eq? variable (car variables)) (car values))
          (else (lookup-bindings (cdr variables) (cdr values)))))

    (if (null? env)
        (error "variable unbound error" variable)
        (lookup-bindings (frame-variables (first-frame env)) (frame-values (first-frame env)))))


(define (set-variable-value! variable value env)
  (define (set-inner-loop! variables values)
    (cond ((null? variables) (set-variable-value! variable value (enclosing-environment env)))
          ((eq? variable (car variables)) (set-car! values value))
          (else (set-inner-loop! (cdr variables) (cdr values)))))
    
  (if (null? env)
    (error "variable unbound error" variable)
    (set-inner-loop! (frame-variables (first-frame env)) (frame-values (first-frame env)))))

(define (define-variable! variable value env)
  (define (search-frame frame)
    (define (helper variables values)
      (cond ((null? variables) (add-bindings-frame! variable value frame))
            ((eq? variable (car variables)) (set-car! values value))
            (else (helper (cdr variables) (cdr values)))))
    (helper (frame-variables frame) (frame-values frame)))

  (search-frame (first-frame env)))



(define (delete-from-env variable env)
  (define (helper pre-var pre-val vars vals)
    (cond ((null? vars) (error "unbound variable" var))
          ( (and (eq? (car vars) variable) (null? pre-var) (null? pre-val))
                    (set-car! env (make-frame (cdr vars) (cdr vals))))
          ( (eq? (car vars) variable) 
                    (set-cdr! pre-var (cdr vars)) 
                    (set-cdr! pre-val (cdr vals))) 
          (else (helper vars vals (cdr vars) (cdr vals)))))

  (helper '() '() (frame-variables (first-frame env)) (frame-values (first-frame env))))

  


;(define my-env (list (cons '(x1 y1 z1) '(1 2 3)) (cons '(x2 y2 z2) '(4 5 6)) (cons '(x3 y3 z3) '(7 8 9))))

;4.11



(define (make-frame2 vars vals) (map cons vars vals))

; I need to mutate the frame when defining a new variable
; It doesnt work if I return a new frame because eval-definition
; doesn't return a the new environment, but relies on the side effect
(define (add-bindings-frame2! var val frame)
  ; pointer manipulation, tail will point to head until now
  (set-cdr! frame frame)
  ; add new head
  (set-car! frame (cons var val)))



(define (lookup-variable2-value variable env)
  (define (lookup-bindings bindings)
    (cond ((null? bindings) (lookup-variable2-value variable (enclosing-environment env)))
          ((eq? variable (caar bindings)) (cdar bindings))
          (else (lookup-bindings (cdr bindings)))))

    (if (null? env)
        (error "variable unbound error" variable)
        (lookup-bindings (first-frame env))))


(define (set-variable2-value! variable value env)
  (define (set-inner-loop! bindings)
    (cond ((null? bindings) (set-variable2-value! variable value (enclosing-environment env)))
          ((eq? variable (caar bindings)) (set-cdr! (car bindings) value))
          (else (set-inner-loop! (cdr bindings)))))
    
  (if (null? env)
    (error "variable unbound error" variable)
    (set-inner-loop! (frame-variables (first-frame env)) (frame-values (first-frame env)))))


(define (define-variable2! variable value env)
  (define (search-frame frame)
    (define (helper bindings)
      (cond ((null? bindings) (add-bindings-frame2! variable value frame))
            ((eq? variable (caar bindings)) (set-cdr! (car bindings) value))
            (else (helper (cdr bindings) (cdr values)))))
    (helper (frame-variables frame) (frame-values frame)))

  (search-frame (first-frame env)))

; 4.12

(define (traverse variable env)
  (define (helper variables values)
    (cond ((null? variables) (traverse variable (enclosing-environment env)))
          ((eq? variable (car variables)) (cons variables values))
          (else (helper (cdr variables) (cdr values)))))

  (if (null? env)
      "not binding found!"
      (helper (frame-variables (first-frame env)) (frame-values (first-frame env)))))


(define (set-variable-value3! var val env)
  (let ((result (traverse var env)))
    (if (eq? result "not binding found!")
        (error "unbound variable error" var)
        (set-car! (cdr result) val))))

(define (lookup-variable-value3 var env)
  (let ((result (traverse var env)))
    (if (eq? result "not binding found!")
        (error "unbound variable error" var)
        (cadr result)))) 

(define (define-variable-value3 var  val env)
  (let ((result (traverse var (cons (first-frame env) '()))))
    (if (eq? result "not binding found!") 
        (add-bindings-frame! var val (first-frame env))
        '())))



(define (traverse2 variable env found-handler not-found-handler)
  (define iter env)
    (define (helper variables values)
      (cond ((null? variables) (iter (enclosing-environment env)))
            ((eq? variable) (found-handler values))
            (else (helper (cdr variables) (cdr values)))))

  (if (null? env)
      (not-found-handler variable)
      (helper (frame-variables (first-frame env)) (frame-values (first-frame env))))

  (iter env))


(define (set-variable-value4! var val env)
  (let ( (found-handler     (lambda (values) (set-car! values val)))
         (not-found-handler (lambda (var)    (error "unbound variable" var))))
      (traverse var env found-handler not-found-handler)))

(define (lookup-variable-value4! var env)
  (let ( (found-handler      (lambda (values) (car values)))
         (not-found-handler  (lambda (var)    (error "unbound variable" var))))
      (traverse var env found-handler not-found-handler)))


(define (define-variable-value4 var val env)
  (let ( (found-handler     (lambda (values) (set-car! values val)))
         (not-found-handler (lambda (var)    (add-bindings-frame! var val (first-frame env)))))
      (traverse var (cons (first-frame env) '()) found-handler not-found-handler)))

