#lang racket

(define (my-equal? l1 l2)
  (cond  ((and (null? l1) (null? l2)) #t)
         ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
         ((and (pair? l1) (pair? l2))
          (and (my-equal? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2))))
         (else #f)))

(define (union-set A B)
  (cond ((null? A) B)
        ((null? B) A)
        ((not (element-of-set? (car A) B)) (cons (car A) (union-set (cdr A) B)))
        (else (union-set (cdr A) B))))

(define (intersection-set A B)
  (cond ((or (null? A) (null? B)) '())
        ((element-of-set? (car A) B) (cons (car A) (intersection-set (cdr A) B)))
        (else (intersection-set (cdr A) B))))

(define (element-of-set? x A)
  (cond ((null? A) #f)
        ((my-equal? x (car A)) #t)
        ( else (element-of-set? x (cdr A)))))

(define (adjoin-set x A)
  (if (element-of-set? x A)
      A
      (cons x A)))



(define (entry tree) (car tree))

(define (left-branch1 tree) (cadr tree))

(define (right-branch1 tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (is-empty-tree? tree)
   (null? tree))


(define (element-of-set?3 x A)
  (cond ((is-empty-tree? A) #f)
        ((= x (entry A)) #t)
        ((< x (entry A)) (element-of-set?3  x (left-branch1 A)))
        ((> x (entry A)) (element-of-set?3  x (right-branch1 A)))))


(define (to-list1 tree)
  (if (is-empty-tree? tree)
      `()
      (append (to-list1 (left-branch1 tree)) (cons (entry tree) (to-list1 (right-branch1 tree))))))


(define (to-list2 tree)
  (define (iter tree result)
    (if (null? tree)
        result
        (iter (left-branch1 tree) (cons (entry tree) (iter (right-branch1 tree) result)))))
  (iter tree `()))


;(to-list2 tree)

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))



(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)


(define (is-present? symbol symbols)
  (cond ((null? symbols) #f)
        ((eq? symbol (car symbols)) #t)
        (else (is-present? symbol (cdr symbols)))))


(define (where-present symbol tree)
  (cond ((is-present? symbol (symbols (left-branch tree))) `left)
        ((is-present? symbol (symbols (right-branch tree))) `right)
        (else `none)))


(define (encode-symbol symbol tree)
  (cond ((leaf? tree) `())

        ((equal? `left (where-present symbol tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))

        ((equal? `right (where-present symbol tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))

        (else (error "symbol not found in tree!"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define message (decode sample-message sample-tree))
(encode message sample-tree)

(define (adjoin-set1 x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set1 x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set1 (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (succesive-merge tree-set)
  (cond ((= 0 (length tree-set)) `()) 
        ((= 1 (length tree-set)) (car tree-set)) 
        ((= 2 (length tree-set)) (make-code-tree (first tree-set) (second tree-set)))
        (else (succesive-merge
                (adjoin-set1  (make-code-tree (first tree-set) (second tree-set)) 
                              (cddr tree-set))))))

(define (generate-huffman-tree pairs)
 (succesive-merge (make-leaf-set pairs)))

;(encode `( H A B A C E F G H)
  ;(generate-huffman-tree `((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))

;(decode '(1 0 0 0 0 1 1 1 0 1 1 0 1 1 0 1 1 1 0 1 0 1 0 0 1 1 0 0 0)
  ;(generate-huffman-tree `((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
(length
(encode 
  `(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB 
    SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
   (generate-huffman-tree `((A 2 ) (GET 2 ) (SHA 3 ) (WAH 1) (BOOM 1) (JOB 2 ) (NA 16 ) (YIP 9)))))

(* 3 (length
  `(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB 
    SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)))
