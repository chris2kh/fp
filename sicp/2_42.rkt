#lang racket 
 (define (enumerate-interval low high) 
   (cond ((> low high) null) 
         ((= low high) (list high)) 
         (else (cons low (enumerate-interval (+ 1 low) high))))) 
  
 (define (flatmap proc seq) 
   (foldr append null (map proc seq))) 
  
 (define empty-board null) 
  
 (define (safe? test-column positions) 
   ;is the coordinate in the set of positions with the given column 
   ;"safe" with respect to all the other coordinates (that is, does not 
   ;sit on the same row or diagonal with any other coordinate)? 
   ;we assume all the other coordinates are already safe with respect 
   ;to each other  
   (define (two-coordinate-safe? coordinate1 coordinate2) 
     (let ((row1 (row coordinate1)) 
           (row2 (row coordinate2)) 
           (col1 (column coordinate1)) 
           (col2 (column coordinate2))) 
       (if (or (= row1 row2) 
               (= (abs (- row1 row2)) (abs (- col1 col2)))) 
           #f 
           #t))) 
   (let ((test-coordinate (get-coordinate-by-column test-column positions))) 
     ;check the test coordinate pairwise against every other coordinate, 
     ;rolling the results up with an "and," and seeding the and with 
     ;an initial "true" value (because a list with one coordinate is 
     ;always "safe" 
         (foldr (lambda (coordinate results)  
                        (and (two-coordinate-safe? test-coordinate coordinate) 
                             results)) 
                #t 
                (remove test-coordinate positions)))) 
                         
      
 (define (adjoin-position new-row new-column existing-positions) 
   (cons (make-coordinate new-row new-column) existing-positions)) 
  
    
 (define (make-coordinate row column) 
   (list row column)) 
 (define (row coordinate) 
   (car coordinate)) 
 (define (column coordinate) 
   (cadr coordinate)) 
 (define (get-coordinate-by-column target-column coordinates) 
   (cond ((null? coordinates) null) 
         ((= target-column (column (car coordinates))) (car coordinates)) 
         (else (get-coordinate-by-column target-column (cdr coordinates))))) 
  
    
  
 (define (queens board-size) 
   (define (queen-cols k) 
     (if (= k 0) 
         (list empty-board) 
         (filter 
          (lambda (positions) (safe? k positions)) 
          (flatmap 
           (lambda (rest-of-queens) 
             (map (lambda (new-row) 
                    (adjoin-position 
                     new-row k rest-of-queens)) 
                  (enumerate-interval 1 board-size))) 
           (queen-cols (- k 1)))))) 
   (queen-cols board-size)) 

   (queens 8)

 (define (slow-queens board-size) 
   (define (queen-cols k) 
     (if (= k 0) 
         (list empty-board) 
         (filter 
          (lambda (positions) (safe? k positions)) 
          (flatmap 
           (lambda (new-row) 
             (map (lambda (rest-of-queens) 
                    (adjoin-position 
                     new-row k rest-of-queens)) 
                  (queen-cols (- k 1))))
           (enumerate-interval 1 board-size)))))
   (queen-cols board-size))

(slow-queens 1)