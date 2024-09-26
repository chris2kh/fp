#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch lenght structure)
  (list lenght structure))

(define (left-branch mobile)
  (first mobile))

(define (right-branch mobile)
  (second mobile))

(define (branch-length branch)
  (first branch))

(define (branch-structure branch)
  (second branch))

;----------------------------------------



(define (total-weight-m mobile)
  (+ (total-weight-b (left-branch mobile) (total-weight-b (right-branch mobile)))))

(define (total-weight-b branch)
  (let ((structure (branch-structure branch)))
    (cond 
      ((null? branch) 0)
      ((pair? structure) (total-weight-m structure))
      (else structure)
    )
  )
)

(define (is-balanced-b? branch)
  (let ((structure (branch-structure branch)))
    (cond
      ((pair? structure) (is-balanced-m? structure))
      (else #t)
    )
  )
)

(define (is-balanced-m? mobile)
  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
       (is-balanced-b? (left-branch mobile))
       (is-balanced-b? (right-branch mobile))))


(define (torque branch)
  (* (branch-length branch) (* (total-weight-b branch))))


