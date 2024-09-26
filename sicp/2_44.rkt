#lang racket


(define (right-split painter n)
  (if (= n 0)
      painter
      (beside painter (below (right-split (- n 1) (right-split (- n 1)))))
      ))



(define (up-split painter n)
  (if (= n 0)
      painter
      (below painter (beside (up-split (- n 1) (up-split (- n 1)))))
      ))


(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((recur ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 recur recur))))))




;2.46
(define (make-vect a b)
  (cons a b))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v u)
  (make-vect (+ (xcor-vect v) (xcor-vect u))
             (+ (ycor-vect v) (ycor-vect u))))


(define (sub-vect v u)
  (make-vect (- (xcor-vect v) (xcor-vect u))
             (- (ycor-vect v) (ycor-vect u))))

(define (scale-vect n v)
  (make-vect (* n (xcor-vect v)) (* n (ycor-vect v))))


;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (first frame))

(define (edge1-frame frame)
  (second frame))

(define (edge2-frame frame)
  (third frame))


(define (make-frameB origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frameB frame)
  (first frame))

(define (edge1-frameB frame)
  (car (second frame)))

(define (edge2-frameB frame)
  (cdr (third frame)))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (segments->painter segment-list)
(lambda (frame)
  (for-each
   (lambda (segment)
     (draw-line
      ((frame-coord-map frame)
       (start-segment segment))
      ((frame-coord-map frame)
       (end-segment segment))))
   segment-list)))

(define (perimeter)
  (segments->painter (list (make-segment 0 0) 
                           (make-segment 0 1)
                           (make-segment 1 0)
                           (make-segment 1 1))))

                          