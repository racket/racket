;;; knights-tour.scm  --  Jens Axel Soegaard

(require (planet "priority-queue.scm" ("soegaard" "galore.plt" 2 1)))

; This example shows the use of priority queues for 
; combinatorial seaching.

; PROBLEM:  Place a knight in the corner of an nxm board.
;           Find a tour i.e. a sequence of moves, s.t.
;           the knight visits all squares exactly once.

(define n 6)
(define m 4)

; The problem with many illustrations and variations:
;     <http://www.borderschess.org/KnightTour.htm>
; A prolog solution can be found at
;     <http://okmij.org/ftp/Prolog/Prolog.html#knights>


; We surround the nxm board with a border of width 2.
; Thus for n=4 and m=3 we look at:

; ########
; ########
; ##----##
; ##----##
; ##----##
; ########
; ########

; All squares are numbered from 0 to (n+4)(m+4)-1.

;   0  1  2  3  4  5  6  7   ; n=4, width=8
;   8  9 10 11 12 13 14 15   ; m=3, height=7
;  16 17  .  .  .  . 22 23             
;  Here 18 19 20 21 is the first row of the actual board


; We need a predicate to determine whether a position x
; is on the board or not.

(define width  (+ n 4))
(define height (+ m 4))

(define (on-board? x)
  (and (<= 2 (modulo x width)    (+ n 1))
       (<= 2 (quotient x width) (+ m 1))))


; neighbours : integer -> (list integer)
;  return a list of positions to which the knight can move
(define (neighbours x)
  (define (u x) (- x width))  ; up
  (define (d x) (+ x width))  ; down
  (define (l x) (- x 1))      ; left
  (define (r x) (+ x 1))      ; right
  
  (filter on-board?
          (list (u (u (r x))) (u (u (l x)))
                (l (l (u x))) (l (l (d x)))
                (d (d (l x))) (d (d (r x)))
                (r (r (u x))) (r (r (d x))))))


; insert-all : priority-queue list -> priority-queue
;  insert all elements of the list l into the priority queue
(define (insert-all l pri pq)
  (foldl (lambda (x pq) (insert x pri pq))
         pq
         l))

(define (find-tour start)
  ; search : priority-queue -> tour or 'no-solution
  ;   finds a partial tour in pq that can be extended
  ;   to a full tour and returns it.
  ;   if no such tour exists 'no-solution is returned
  (define (search pq)
    (if (empty? pq)
        'no-solution  
        (let* ([tour        (find-min pq)]
               [pri         (find-min-priority pq)]
               [pq          (delete-min pq)]
               [extend-tour (lambda (x) 
                              (cons x tour))]
               [new?        (lambda (x)
                              (not (member x tour)))])
          (if (= pri (* m n))
              (reverse tour)
              (search (insert-all (map extend-tour
                                       (filter new? (neighbours (first tour))))
                                  (add1 pri)
                                  pq))))))
  (search (insert (list start) 1 
                  (empty (lambda (n1 n2) (- (number-compare n1 n2)))))))
  
(define upper-left-corner (+ (* 2 width) 2))

; A tour can now be found with:

;   (find-tour upper-left-corner)

; Let us define a some helpers in order to display the solution
; in a more human readable way.


; index : integer list -> integer 
;  return the index of x in l
(define (index x l)
  (let loop ([i 0]
             [l l])
    (cond
      [(null? l)        #f]
      [(= (first l) x)  i]
      [else             (loop (add1 i) (rest l))])))

(define (interval m n)
  (if (> m n)
      '()
      (cons m (interval (add1 m) n))))

(define (display-tour tour)
  (define (display2 n)
    (if (number? n)
        (if (<= 0 n 9)
            (begin
              (display " ") (display n))
            (display n))
        (display n))
    (display " "))

  (if (not (list? tour))
      (display "No solution")
      (let loop ([x upper-left-corner])
        (if (<= x (sub1 (* width (- height 2))))
            (begin
              (if (on-board? x)
                  (display2 (index x tour)))
              (if (= (modulo x width) (sub1 width))
                  (newline))
              (loop (add1 x)))
            (void)))))

(display "Please wait...\n")
(time (display-tour (find-tour upper-left-corner)))
