#lang racket

; dc/dx = 0
; dx/dx = 1
; d (u + v)/dx = du/dx + dv/dx
; d (uv)/dx = u . dv/dx + v . du/dx
; d (u^n) = n . (u^(n-1) . du/dx)

(define (variable? e) (symbol? e))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))


(define (=number? exp num) (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-mult m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentation x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        (else (list `** x n))))


(define (sum? e)  (and (pair? e) (eq? `+ (car e))))
(define (mult? e) (and (pair? e) (eq? `* (car e))))
(define (exponentation? e) (and (pair? e) (eq? `** (car e))))
(define (addend e) (second e))
(define (augend e)
  (if (= (length (cddr e)) 1)
      (third e)
      (make-sum (addend (cdr e)) (augend (cdr e)))))

(define (multiplier e) (second e))
(define (multiplicand e)
  (if (= (length (cddr e)) 1)
      (third e)
      (make-mult (multiplier (cdr e)) (multiplicand (cdr e)))))




(define (base e) (second e))
(define (power e) (third e))


(define (deriv exp var)
  (cond ((number? exp )   0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)      (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((mult? exp)     (make-sum
                          (make-mult (multiplier exp) (deriv (multiplicand exp) var))
                          (make-mult (deriv (multiplier exp) var) (multiplicand exp))))
        ((exponentation? exp)
         (make-mult
          (power exp)
          (make-mult (make-exponentation (base exp) (make-sum (power exp) -1))
                     (deriv (base exp) var))))
        (else (error "not supported expression" exp))))


;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
;(deriv '(* (* x y) (+ x 3)) 'x)
;(deriv '(** x 1) 'x)

;(deriv `(+ x  x x x) 'x)
;(deriv '(* (* x y) (+ x 3)) 'x)



(define (make-sum1 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-mult1 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentation1 x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        (else (list x `** n))))


(define (sum1? e)  (and (pair? e) (eq? `+ (second e))))

(define (mult1? e)
  (or
   (and (pair? e)
        (eq? `* (second e))
        (not (null? (cddr e)))
        (= 1 (length (cddr e))))

   (and (pair? e)
        (eq? `* (second e))
        (not (null? (cddr e)))
        (> (length (cddr e)) 1)
        (eq? `* (cadddr e)))
   ))

(mult1? '(x * 2 * 3 + 5))
(multiplicand '(x * 2 * 3 + 5))




(define (exponentation1? e) (and (pair? e) (eq? `** (second e))))
(define (addend1 e) (first e))
(define (augend1 e)
  (if (= (length (cddr e)) 1)
      (third e)
      (make-sum1 (addend1 (cdr e)) (augend1 (cdr e)))))

(define (multiplier1 e) (first e))
(define (multiplicand1 e)
  (if (= (length (cddr e)) 1)
      (third e)
      (make-mult1 (multiplier1 (cddr e)) (multiplicand1 (cddr e)))))


(define (base1 e) (first e))
(define (power1 e) (third e))


(define (deriv1 exp var)
  (cond ((number? exp )   0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum1? exp)      (make-sum1 (deriv1 (addend1 exp) var) (deriv1 (augend1 exp) var)))
        ((mult1? exp)     (make-sum1
                           (make-mult1 (multiplier1 exp) (deriv1 (multiplicand1 exp) var))
                           (make-mult1 (deriv1 (multiplier1 exp) var) (multiplicand1 exp))))
        ((exponentation1? exp)
         (make-mult1
          (power1 exp)
          (make-mult1 (make-exponentation1 (base1 exp) (make-sum1 (power1 exp) -1))
                      (deriv1 (base1 exp) var))))
        (else (error "not supported expression" exp))))


;(deriv '(+ x 3) 'x)
;(deriv1 '(x + 3) 'x)

;(deriv '(* x y) 'x)
;(deriv1 '(x * y) 'x)



;(deriv '(** x 1) 'x)
;(deriv1 '(x ** 1) 'x)

;(deriv `(+ x  x x x) 'x)
;(deriv1 `(x + (x + (x + x))) 'x)





(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (derive-sum operands var)
  (make-sum (derive2 (car operands) var) (derive2 (cadr operands) var)))

(define (derive-mult operands var)
  (make-sum
   (make-mult (derive2 (car operands) var) (cadr operands))
   (make-mult (car operands) (derive2 (cadr operands) var))))

(define (install-derive-package)
  ;; internal procedures
  (define (derive-sum operands var)
    (make-sum (derive2 (car operands) var) (derive2 (cadr operands) var)))

  (define (derive-mult operands var)
    (make-sum
     (make-mult (derive2 (car operands) var) (cadr operands))
     (make-mult (car operands) (derive2 (cadr operands) var))))

  (define (derive-exponentation exp)
    (make-mult
     (power1 exp)
     (make-mult (make-exponentation (base exp) (make-sum (power exp) -1))
                (deriv2 (base1 exp) var))))

  ;; interface to the rest of the system
  (put 'deriv '+  derive-sum)
  (put 'deriv '*  derive-mult)
  (put 'deriv '** derive-exponentation)
  'done)

; numbers
(define (install-numbers-package)
  (define (equ? a b)
    (= a b))

  (put `equ? `(scheme-number scheme-number) equ?)

  (define (=zero? x) (= x 0))
  (put `=zero? `(scheme-number) =zero?)


  'done)

(define (install-rat-package)
  (define (equ? a b)
    (= (* (denom a) (numen b)) (* (denom b) (numen a))))

  (put `equ? `(rat-number rat-number) equ?)

  (define (=zero? x) (= (denum x) 0))
  (put `=zero? `(rat-number) =zero?)

  'done)

(define (install-complex-package)
  (define (equ? a b)
    (and
     (= (magnitude a) (magnitude b))
     (= (angle a) (angle b))))

  (put `equ? `(complex complex) equ?)

  (define (=zero? x) (= (magnitude x) 0))
  (put `=zero? `(rat-number) =zero?)
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond (= type1 type2) (error "No method for these types" (list op type-tags))
                    (else
                        (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                          (cond (t1->t2
                                  (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                                  (apply-generic op a1 (t2->t1 a2)))
                          (else (error "No method for these types"
                                     (list op type-tags))))))))
              (error "No method for these types"
                     (list op type-tags)))))))


































