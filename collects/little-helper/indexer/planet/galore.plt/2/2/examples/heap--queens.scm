; queens.ss  --  Jens Axel Soegaard  -- 18th may 2003 / 18th dec 2005 

(require (planet "heap.scm" ("soegaard" "galore.plt" 2 1))
         (lib "67.ss" "srfi"))

; THE PUZZLE

; Place 8 queens on a chess board, such that no queen
; can beat another. No pair of queens are on the
; same row, column or diagonal.

; row           p
;  0 *--------  0
;  1 ---*-----  3
;  2 -*-------  1
;  3 ----*----  4
;  4 ---------
;  5 ---------
;  6 ---------
;  7 ---------

; A configuration c is represented c = (4 1 3 0).
; The first empty row is (length c) = 5

; Predicate 
;  (peace? c p) :  A queen in position p in row (length c) is
;                  not in conflict with any queen in c.

(define (peace? c p)
  (and (not (member p c))  ; column
       (let loop ([c c]
                  [nw (- p 1)]  ; position of north-west diagonal above 
                  [ne (+ p 1)]) ; position of north-east diagonal above
         (or (null? c)
             (and (not (= (car c) nw))
                  (not (= (car c) ne))
                  (loop (cdr c) (- nw 1) (+ ne 1)))))))

; interval : integer integer -> (list integer)
;  (interval m n) = (list m m+1 ... n)
(define (interval m n)
  (if (> m n)
      '()
      (cons m (interval (+ m 1) n))))

; while searching for a peaceful configuration,
; we look at the longest configurations first.
(define (configuration-compare l1 l2)
  (integer-compare (length l2) (length l1)))

; queens : integer -> configuration or 'no-solution
;  find a configuration of queens that solve the n-queen problem
(define (queens n)
  (define (search h)
    ; h is a heap of configurations
    (if (empty? h)
        'no-solution  
        (let* ([c (find-min h)]
               [h (delete-min h)])
          (cond
            [(= (length c) n)  
             ; if the length of the configuration is n, all queens are placed
             c]
            [else
             ; otherwise extend the configuration with one more
             ; queen - insert all ways to do that in the heap and
             ; search again
             (search (insert* (map (lambda (p) (cons p c))
                                   (filter (lambda (p) (peace? c p))
                                           (interval 0 (- n 1))))
                              h))]))))
  (search (insert '() (empty configuration-compare))))

; solve 8-queen and print the solution:

(let* ([n 8]
       [solution (queens n)])
  (do ([rows solution (cdr rows)])
    [(null? rows)  (void)]
    (let ([s (make-string n #\.)])
      (string-set! s (car rows) #\#)
      (display s)
      (newline)))
  (display solution))

; Note: solving 16-queen takes less than a second on my machine
