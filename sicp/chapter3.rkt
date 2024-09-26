#lang sicp

(define (display-newline x)
  (display x)
  (display "\n")
  )

;3.1
(define (make-acumulator acc)
  (lambda (x)
    (begin
      (set! acc (+ acc x))
      acc)))

(define A (make-acumulator 10))
;(A 5)
;(A 20)

;3.2
(define (make-monitored f)
  (let ((calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) calls)
            ((eq? x 'reset) (set! calls 0))
            (else
             (begin (set! calls (+ calls 1))
                    (f x)))))))

(define (double x) (* 2 x))
(define mf (make-monitored double))

;(mf 10)
;(mf 20)
;(mf 50)
;(mf 'how-many-calls?)
;(mf 'reset)
;(mf 100)
;(mf 'how-many-calls?)

;3.4
(define (make-account password initial-deposit)
  (define failed-attempts 0)
  (define my-password password)
  (define balance initial-deposit)

  (define (is-valid password) (eq? my-password password))
  (define (call-the-cops) (error "calling the cops..."))


  (define (withdraw ammount)
    (if (> ammount balance)
        (error "not enough funds!!")
        (begin (set! balance (- balance ammount))
               balance)))

  (define (deposit ammount)
    (begin (set! balance (+ balance ammount))
           balance))


  (define (invalid-password-logic)
    (begin
      (set! failed-attempts (+ failed-attempts 1))
      (if (= 7 failed-attempts)
          (call-the-cops)
          (lambda _ "not valid username and/or password"))))


  (define (dispatch-after-validations password message)
    (cond ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit)  deposit)
          (else (error "not valid operation"))))

  (define (validations password message)
    (cond ((not (is-valid password)) (invalid-password-logic))
          (else
           (begin (set! failed-attempts 0) (dispatch-after-validations password message))
           )))

  validations
  )

;(define bancolombia (make-account 'pass123 100))
;((bancolombia 'pass124 'deposit)  200)
;((bancolombia 'pass124 'withdraw) 280)
;((bancolombia 'pass124 'deposit)  200)
;((bancolombia 'pass124 'deposit)  200)
;((bancolombia 'pass124 'deposit)  200)
;((bancolombia 'pass124 'deposit)  200)
;((bancolombia 'pass123 'deposit)  1000)
;((bancolombia 'pass124 'deposit)  200)

(define random-init 0)
(define (rand-update x) (+ x 1))
(define rand
  (let ((x random-init))
    (lambda () (set! x (rand-update x)) x)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials test)
  (define (iter i successes)
    (cond ((= i 0) (/ successes trials))
          ((test) (iter (- i 1) (+ successes 1)))
          (else (iter (- i 1) successes))))
  (iter trials 0.0))

(define (estimate-pi1 trials)
  (sqrt (/ 6 (monte-carlo1 trials cesaro-test1 random-init))))
(define (cesaro-test1 seed)
  (let* ((x1 (rand-update seed))
         (x2 (rand-update x1)))
    (cons (= (gcd x1 x2) 1) x2)
    ))

(define (monte-carlo1 trials test seed)
  (define (iter i state) ; state = (numSuccesses, latestSeed)
    (let ((results (test (cdr state))))
      (cond ((= i 0) (/ (car state) trials))
            ((car results) (iter (- i 1) (cons (+ (car state) 1) (cdr results))))
            (else (iter (- i 1) ((car state) (cdr results)))))))
  (iter trials (cons 0 seed)))


;3.5
(define (random-in-range low high)
  (+ low (* (random) (- high low))))


(define (monte-carlo-integration x1 x2 y1 y2 predicate num-trials)
  (define (experiment)
    (predicate (random-in-range (min x1 x2) (max x1 x2))
               (random-in-range (min y1 y2) (max y1 y2))))

  (* (monte-carlo num-trials experiment) (* (abs (- x2 x1)) (abs (- y2 y1))))
  )

;(monte-carlo-integration -1 1 -1 1 (lambda (x y) (<= (+ (sqr x) (sqr y)) 1)) 1000000)

;3.6
(define rand1
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))))
    dispatch))

;(rand1 'generate)
;((rand1 'reset) 0)
;(rand1 'generate)

;3.7
(define (make-account1 password initial-deposit)
  (define failed-attempts 0)
  (define my-password password)
  (define balance initial-deposit)

  (define (is-valid password) (eq? my-password password))
  (define (call-the-cops) (error "calling the cops..."))


  (define (withdraw ammount)
    (if (> ammount balance)
        (error "not enough funds!!")
        (begin (set! balance (- balance ammount))
               balance)))

  (define (deposit ammount)
    (begin (set! balance (+ balance ammount))
           balance))


  (define (invalid-password-logic)
    (begin
      (set! failed-attempts (+ failed-attempts 1))
      (if (= 7 failed-attempts)
          (call-the-cops)
          (lambda _ "not valid username and/or password"))))


  (define (dispatch-after-validations message)
    (cond ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit)  deposit)
          (else (error "not valid operation"))))

  (define (validations password message)
    (cond ((eq? message 'special-check-is-valid) (is-valid password))
          ((not (is-valid password)) (invalid-password-logic))
          (else
           (begin (set! failed-attempts 0) (dispatch-after-validations message))
           )))

  validations
  )

(define (make-joint user1-account user1-password user2-password)
  (if (user1-account user1-password 'special-check-is-valid)
      (lambda (password message)
        (if (eq? user2-password password)
            (user1-account user1-password message)
            (user1-account password message))); increment bad attemtps in account
      (error "not valid acccount and/or password")))

;(define cristian-account (make-account1 'pass123' 1000))
;((cristian-account 'pass123 'withdraw) 200)

;(define juliana-account (make-joint cristian-account 'pass123 'junago123))
;((juliana-account 'junago123 'withdraw) 500)

;((cristian-account 'pass123 'deposit) 3000)
;((cristian-account 'pass123 'withdraw) 1300)
;((juliana-account 'junago123 'deposit) 800)
;((cristian-account 'pass123 'withdraw) 2800)


(define (make-fun state)
  (lambda (n) (set!  state (* state n)) state))

(define f (make-fun 1))
(define g (make-fun 1))

;(+ (f 0) (f 1))
;(+ (g 1) (g 0))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;(define v (mcons 'a (mcons 'b (mcons 'c (mcons 'd '())))))
;(mystery v)



;3.17
(define (member? x xs)
  (cond ((null? xs) #f)
        ((eq? (car xs) x) #t)
        (else (member? x (cdr xs)))))


(define count-pairs
  (let ((already-seen nil))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((member? (car x) already-seen) 0)
            (else (begin (set! already-seen (cons (car x) already-seen))
                         (+ 1 (count-pairs (cdr x)) (count-pairs (car x)))))))))


(define (count-pairs-pure-style x already-seen)
  (cond ((not (pair? x)) (cons 0 already-seen))
        ((member? (car x) already-seen) (cons 0 already-seen))
        (else
         (let* ((tuple1 (count-pairs-pure-style (car x) (cons (car x) already-seen)))
                (pairs-in-car (car tuple1))
                (already-seen-updated (cdr tuple1))
                (tuple2 (count-pairs-pure-style (cdr x) already-seen-updated))
                (pairs-in-cdr (car tuple2)))
           (cons (+ 1 pairs-in-car pairs-in-cdr) (cdr tuple2))))))


;(define pair3 (cons 3 4))
;(define pair2 (cons pair3 pair3))
;(define pair1 (cons pair2 pair2))
;(count-pairs pair1)
;(car (count-pairs-pure-style pair1 `()))

;3.18

(define is-cycle?
  (let ((already-seen nil))
    (lambda (xs)
      (cond ((null? xs) #f)
            ((member? xs already-seen) #t)
            (else (begin (set! already-seen (cons xs already-seen))
                         (is-cycle? (cdr xs))))))))



;3.19
(define (is-cycle1-helper? pointer1 pointer2)
  (cond ((or (null? pointer2)
             (null? (cdr pointer2))
             (null? (cdr (cdr pointer2)))) #f)
        ((eq? pointer1 pointer2) #t)
        (else (let ( (updated-pointer1 (cdr pointer1))
                     (updated-pointer2 (car (cdr (cdr pointer2)))))
                (is-cycle1-helper? updated-pointer1 updated-pointer2)))))

(define (is-cycle1? xs)
  (if (null? xs)
      #f
      (is-cycle1-helper? xs (cdr xs))))



(define (last-pair xs)
  (cond ( (null? xs) xs)
        ( (null? (cdr xs)) xs)
        (else (last-pair (cdr xs)))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define a (make-cycle (list 1 2 3)))
;(is-cycle1? a)

; queues as cons front-ptr rear-ptr
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;3.21
;(define print-queue front-ptr)
;
;(define q1 (make-queue))
;(print-queue (insert-queue! q1 'a))
;(print-queue (insert-queue! q1 'b))
;(print-queue (delete-queue! q1))
;(print-queue (delete-queue! q1))

;3.22
; queues as object

(define (make-queue1)
  (define _front-ptr nil)
  (define _rear-ptr  nil)

  (define (empty?)
    (null? _front-ptr))

  (define (_front-queue)
    (if (empty?)
        (error "FRONT called with an empty queue")
        (car _front-ptr)))


  (define (insert! item)
    (let ((new-pair (cons item '())))
      (cond ((empty?)
             (set! _front-ptr new-pair)
             (set! _rear-ptr  new-pair)
             _front-ptr)
            (else
             (set-cdr! _rear-ptr new-pair)
             (set! _rear-ptr new-pair)
             _front-ptr))))


  (define (delete!)
    (cond ((empty?)
           (error "DELETE! called with an empty queue"))
          (else (set!  _front-ptr (cdr _front-ptr)) _front-ptr)))


  (define (dispatch m . args)
    (cond ((eq? m 'empty?)   (empty?))
          ((eq? m 'insert!)  (insert! (car args)))
          ((eq? m 'delete!)  (delete!))
          (else (error "message not understood" m))))

  dispatch)

(define q (make-queue1))
;(q 'insert! 'a)
;(q 'insert! 'b)
;(q 'delete!)
;(q 'delete!)

;3.23
(define (make-dq) (cons '() '()))
(define (set-first! dq ptr) (set-car! dq ptr))
(define (set-last! dq ptr) (set-cdr! dq ptr))
(define (first dq) (car dq))
(define (last dq) (cdr dq))
(define (empty-dq? dq) (null? (first dq)))

(define (next node) (cddr node))
(define (previous node) (cadr node))
(define (make-node val) (cons val (cons '() '())))
(define (set-next! node next-node) (set-cdr! (cdr node) next-node))
(define (set-previous! node previous-node) (set-car! (cdr node) previous-node))


(define (to-vals dq)
  (define (iter node acc)
    (if (null? node)
        acc
        (iter (next node) (append acc (list (car node))))))
  (iter (first dq) '()))

(define (front-delete-dq! dq)
  (cond ((empty-dq? dq)
         (error "DELETE! called with an empty dequeue" dq))
        ((eq? (first dq) (last dq)) (set-first! dq nil) (set-last! dq nil) dq)
        (else
         (let ((temp (first dq)))
           (set-first! dq (next (first dq)))
           (set-next! temp nil)
           (set-previous! (first dq) nil) dq))))

(define (rear-delete-dq! dq)
  (cond ((empty-dq? dq)
         (error "DELETE! called with an empty dequeue" dq))
        ((eq? (first dq) (last dq)) (set-first! dq nil) (set-last! dq nil) dq)
        (else
         (let ((temp (last dq)))
           (set-last! dq (previous (last dq)))
           (set-previous! temp nil)
           (set-next! (last dq) nil) dq))))

(define (front-insert-dq! dq item)
  (let ((new-node (make-node item)))
    (cond ((empty-dq? dq)
           (set-first! dq new-node)
           (set-last!  dq new-node)
           dq)
          (else
           (set-previous! (first dq) new-node)
           (set-next! new-node (first dq))
           (set-first! dq new-node)
           dq))))


(define (rear-insert-dq! dq item)
  (let ((new-node (make-node item)))
    (cond ((empty-dq? dq)
           (set-first! dq new-node)
           (set-last! dq new-node)
           dq)
          (else
           (set-next! (last dq) new-node)
           (set-previous! new-node (last dq))
           (set-last! dq new-node)
           dq))))


;(define dq (make-dq))
;(front-insert-dq! dq 'a)
;(front-insert-dq! dq 'b)
;(rear-insert-dq! dq 'c)
;(front-insert-dq! dq 'f)
;(rear-delete-dq! dq)
;(rear-delete-dq! dq)
;(rear-insert-dq! dq 'z)
;(to-vals dq)
;(front-insert-dq! dq 'W)
;(rear-insert-dq! dq 'X)
;(front-insert-dq! dq 'Y)
;(to-vals dq)
;(front-delete-dq! dq)
;(front-delete-dq! dq)
;(front-delete-dq! dq)


; one dimension table
(define (assoc key table)
  (cond ((null? table) #f)
        ((eq? (car (car table)) key) (car table))
        (else (assoc key (cdr table)))))

(define (lookup key table)
  (let  ((record (assoc key (cdr table))))
    (cond (record (cdr record))
          (else #f))))

(define (insert! val key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record val)
        (set-cdr! table (cons (cons key val) (cdr table)))))
  'ok)

(define (make-table-with-key key) (list key))
(define (make-table) (make-table-with-key '*table*))

; two dimension table
(define (lookup2 key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (lookup key2 subtable)
        #f)))

(define (insert2! val key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (insert! val key2 subtable)
        (let ((new-subtable (make-table-with-key key1)))
          (begin
            (insert! new-subtable key1 table)
            (set-cdr! table (cons new-subtable (cdr table))))))))

; 3.24
(define (make-table-oop same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key table)
      (cond ((null? table) #f)
            ((same-key? (car (car table)) key) (car table))
            (else #f)))

    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table-oop eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; 3.25
(define (lookupNth keys table)
  (cond ((null? keys) #f)
        ((null? (cdr keys)) (lookup (car keys) table))
        (else
         (let ((subtable (assoc (car keys) (cdr table))))
           (if subtable
               (lookupNth (cdr keys) subtable)
               #f)))))

(define (insertNth! val keys table)
  (cond ((null? keys) table)
        ((null? (cdr keys)) (insert! val (car keys) table))
        (else
         (let ((subtable (assoc (car keys) (cdr table))))
           (if subtable
               (insertNth! val (cdr keys) subtable)
               (let ((subtable (make-table)))
                 (insertNth! val (cdr keys) subtable)
                 (set-cdr! table
                           (cons
                            (cons (car keys) (cdr subtable))
                            (cdr table)))))))))

;(define colombia (make-table))
;(insert! 1500 'pereira colombia)
;(insert! 2000 'manizales colombia)
;(insert! 3000 'cali colombia)
;(insert! 5000 'bogota colombia)
;
;
;(define argentina (make-table))
;(insert! 8000 'buenos_aires argentina)
;(insert! 2150 'mar_del_plata argentina)
;(insert! 1000 'rosario argentina)
;(insert! 800 'cordoba argentina)
;
;(define usa (make-table))
;(insert! 70000 'new_york usa)
;(insert! 9900 'boston usa)
;(insert! 3300 'miami usa)
;
;(define america (make-table))
;(insert! (cdr colombia)  'colombia america)
;(insert! (cdr argentina) 'argentina america)
;
;(insertNth! 4400 (list 'colombia 'bucaramanga) america)
;(insertNth! 6000  (list 'colombia 'bogota) america)
;(insertNth! 6000 (list 'mexico 'df) america)
;(lookupNth (list 'mexico 'df) america)
;(lookup2 'mexico 'df america)
;(insertNth! 16000 (list 'mexico 'df) america)
;(insertNth! 2000 (list 'mexico 'cancun) america)
;(lookup2 'mexico 'df america)
;america


; 3.26
(define (empty-tree) (list '() '() '()))
(define (make-tree l val r) (list l val r))
(define (get-key tree) (car (cadr tree)))
(define (get-val tree) (cdr (cadr tree)))
(define (set-val! val tree) (set-cdr! (cadr tree) val))
(define (set-left! l tree) (set-car! tree l))
(define (set-right! r tree) (set-cdr! (cdr tree) r))
(define left-branch car)
(define right-branch caddr)

(define (lookup-tree key tree)
  (cond ((null? tree) #f)
        ((eq? key (get-key tree)) (get-val tree))
        ((< key (cadr tree)) (lookup-tree key (left-branch)))
        (else (lookup-tree key (right-branch)))))

(define (insert-tree key val tree)
  (cond ((null? tree) (make-tree nil (cons key val) nil))
        ((eq? key (get-key tree)) (set-val! val tree) tree)
        ((< key (cadr tree)) (set-left! tree (insert-tree key val (left-branch tree))) tree)
        (else (set-right! tree (insert-tree key val (right-branch tree))) tree)))

; section circuit simulation
(define (get-signal x) 1)
(define (after-delay delay f) (f))
(define inverter-delay 1)
(define or-gate-delay 1)
(define and-gate-delay 1)
(define (set-signal! output new-value)  (set! output new-value))
(define (add-action! wire action) `())
(define (make-wire) `())

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-and x y)
  (cond ((and (= x 0) (= y 0)) 0)
        ((and (= x 0) (= y 1)) 0)
        ((and (= x 1) (= y 0)) 0)
        ((and (= x 1) (= y 1)) 1)
        (else (error "Invalid signals"))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or x y)
  (cond ((or (= x 0) (= y 0)) 0)
        ((or (= x 0) (= y 1)) 1)
        ((or (= x 1) (= y 0)) 1)
        ((or (= x 1) (= y 1)) 1)
        (else (error "Invalid signals"))))

;3.28
(define (or-gate2 A B result)
  (let ((not_A (make-wire))
        (not_B (make-wire))
        (not_A_not_B (make-wire)))
    (inverter A not_A)
    (inverter B not_B)
    (and-gate not_A not_B not_A_not_B)
    (inverter not_A_not_B result))
  )

(define (or-gate input1 input2 output)
  (define (handler)
    (let ((new-output (logical-or (get-signal input1) (get-signal input2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-output)))))

  (add-action! input1 handler)
  (add-action! input2 handler))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;3.30
(define (ripple-carry-adder as bs ss c)
  (define (helper ras rbs rss c)
    (if (null? ras)
        'ok
        (let ((new-c (make-wire)))
          (full-adder (car ras) (car bs) c (car ss) new-c)
          (helper (cdr ras) (cdr bs) (cdr ss) new-c))))

  (helper (reverse as) (reverse bs) (reverse ss) c))

;3.50 streams
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-for-each proc s)
  (if (null-stream? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-newline s))


(define (delay f) (lambda () f))
(define (force f) (f))
(define (cons-stream a b) (cons a b))
(define stream-car car)
(define null-stream? null?)
(define (stream-cdr xs) (force (cdr xs)))





(define (stream-map2 proc . argstreams)
  (if (null-stream? (stream-car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (memo-proc (lambda () (apply stream-map2
                                    (cons proc (map stream-cdr argstreams))))))))

(define (stream-map f xs)
  (if (null? xs)
      the-empty-stream
      (cons-stream (f (stream-car xs))  (memo-proc (lambda () (stream-map f (stream-cdr xs)))))))

(define (show x)
  (display-newline x)
  x)

(define (stream-enumerate-interval a b)
  (if (> a b)
      `()
      (cons-stream a (memo-proc (lambda () (stream-enumerate-interval (+ a 1) b))))))

(define (stream-ref xs pos)
  (cond ((null-stream? xs) (error "empty stream found"))
        ((= 0 pos) (stream-car xs))
        (else (stream-ref (stream-cdr xs) (- pos 1)))))



;(define x (stream-map2 show (stream-enumerate-interval 0 5)))
;(stream-ref x 2)
;(stream-ref x 4)

(define (take n seq)
  (if (or (= 0 n) (null-stream? seq))
      '()
      (cons (stream-car seq) (take (- n 1) (stream-cdr seq)))))


(define (stream-filter predicate seq)
  (cond ((null-stream? seq) `())
        ((predicate (stream-car seq)) (cons-stream (stream-car seq)
                                                   (lambda () (stream-filter predicate (stream-cdr seq)))))
        (else (stream-filter predicate (stream-cdr seq)))))


(define (integers-from n)
  (cons-stream n  (lambda () (integers-from (+ n 1)))))

(define integers (integers-from 1) )

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))
;(stream-ref no-sevens 100)


(define (eratostenes seq)
  (cons-stream (stream-car seq)
               (lambda ()
                 (eratostenes
                  (stream-filter
                   (lambda (x) (not (divisible? x (stream-car seq)))) (stream-cdr seq))))))

(define primes (eratostenes (integers-from 2)))

(define (add-streams s1 s2) (stream-map2 + s1 s2))

;3.53
(define s (cons-stream 1 (lambda () (add-streams s s))))

;(take 10 s)

;3.54
(define (mul-streams s1 s2) (stream-map2 * s1 s2))

(define factorials (cons-stream 1 (lambda () (mul-streams (stream-cdr integers) factorials))))
;(take 10 factorials)

;3.55
(define (partial-sums seq)
  (cons-stream (stream-car seq) (lambda () (add-streams (stream-cdr seq) (partial-sums seq)))))

;(take 10 (partial-sums integers))

;3.56
(define (scale-stream seq n)
  (stream-map (lambda (x) (* x n)) seq))



(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))


;(define S
; (cons-stream 1 (lambda ()
;                (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5))))))

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (lambda () (stream-map (lambda (guess) (sqrt-improve guess x)) guesses))))
  guesses)

;(take 10 (sqrt-stream 2))


(define (sqrt-stream2 x)
  (cons-stream 
    1.0 
    (lambda () (stream-map (lambda (guess) (sqrt-improve guess x)) (sqrt-stream2 x)))))


; 3.64
(define (stream-limit seq tolerance)
  (let ((rest  (stream-cdr seq)))
  (if (<= (abs (- (stream-car seq) (stream-car rest))) tolerance)
      (stream-car rest)
      (stream-limit rest tolerance))))

(define (sqrt x tolerance)
  (stream-limit  (sqrt-stream x) tolerance))

;(sqrt 2 0.05)

(define (inside? tolerance)
  (lambda (seq) (<= (abs (- (stream-car seq) (stream-car (stream-cdr seq)))) tolerance)))

(define (dropUntil p seq)
  (if (p seq) seq
      (dropUntil p (stream-cdr seq))))

(define (sqrt2 x tolerance)
  (stream-car
  (stream-cdr
  (dropUntil (inside? tolerance) (sqrt-stream x)))))

;(sqrt2 2 0.05)


