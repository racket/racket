(module utils mzscheme
  ; See boardsig.ss for the core utilities.
  (require "board-size.ss"
           "board.ss")
  
  (provide
   n-times      ; call a procedure with each number in [0, n]
   n-map        ; call a procedure with each number in [0, n] to make a list
   print-board  ; pretty-prints a board
   find-winner  ; checks a board to see if there's a winner
   other-player ; x -> o   or  o -> x

   quicksort    ; Quicksort

   pick-best    ; takes a list of (cons <num> <val>) and returns pair
                ;  with the biggest <num>, randomly chossing among
                ;  equals
   )
   
  ; Call f with each number in [0, n]
  (define (n-times n f)
    (let loop ([i 0])
      (unless (= i n)
        (f i)
        (loop (add1 i)))))
  
  ; Call f with each number in [0, n] to make a list
  (define (n-map n f)
    (let loop ([i 0])
      (if (= i n)
          null
          (cons (f i)
                (loop (add1 i))))))
  
  
  ; Pretty-prints the board
  (define print-board
    (case-lambda
      [(b) (print-board b (current-output-port))]
      [(b port)
       (let ([n (current-board-size)])
         (n-times n (lambda (j)
                      (n-times n (lambda (i)
                                   (fprintf port "~a " (let ([v (board-cell b i j)])
                                                         (cond
                                                           [(eq? v none) '-]
                                                           [(eq? v x) 'x]
                                                           [(eq? v o) 'o])))))
                      (newline port))))]))
  
  ; Given a player (board cell value), get the other one
  (define (other-player as-player)
    (if (eq? as-player x)
        o
        x))
  
  ; See if the board has a winner; returns o, x, or #f
  (define (find-winner board)
    (let ([n (current-board-size)])
      (let ([row-wins-x (make-vector n 1)]
            [row-wins-o (make-vector n 1)]
            [col-wins-x (make-vector n 1)]
            [col-wins-o (make-vector n 1)])
        (n-map n (lambda (i)
                   (n-map n (lambda (j)
                              (let ([v (board-cell board i j)])
                                (cond
                                  [(eq? v x) (vector-set! col-wins-o i 0)
                                   (vector-set! row-wins-o j 0)]
                                  [(eq? v o) (vector-set! col-wins-x i 0)
                                   (vector-set! row-wins-x j 0)]
                                  [else (vector-set! col-wins-o i 0)
                                        (vector-set! row-wins-o j 0)
                                        (vector-set! col-wins-x i 0)
                                        (vector-set! row-wins-x j 0)]))))))
        (let ([o-wins (+ (apply + (vector->list row-wins-o))
                         (apply + (vector->list col-wins-o)))]
              [x-wins (+ (apply + (vector->list row-wins-x))
                         (apply + (vector->list col-wins-x)))])
          (cond
            [(= o-wins x-wins) #f]
            [(> o-wins x-wins) o]
            [else x])))))
  
  (define quicksort
    (lambda (l less-than)
      (let* ([v (list->vector l)]
             [count (vector-length v)])
        (let loop ([min 0][max count])
          (if (< min (sub1 max))
              (let ([pval (vector-ref v min)])
                (let pivot-loop ([pivot min]
                                 [pos (add1 min)])
                  (if (< pos max)
                      (let ([cval (vector-ref v pos)])
                        (if (less-than cval pval)
                            (begin
                              (vector-set! v pos (vector-ref v pivot))
                              (vector-set! v pivot cval)
                              (pivot-loop (add1 pivot) (add1 pos)))
                            (pivot-loop pivot (add1 pos))))
                      (if (= min pivot)
                          (loop (add1 pivot) max)
                          (begin
                            (loop min pivot)
                            (loop pivot max))))))))
        (vector->list v))))
  
  ; Compare the values in goodness and pick the biggest element.
  ; Takes a non-empty list of (cons <goodness> <choice>)
  ; If <goodness> is a pair, look at the car.
  ; Return (cons <goodness> <choice>).
  (define (pick-best choices)
    (let loop ([l (cdr choices)]
               [goodness (let ([g (caar choices)])
                           (if (pair? g)
                               (car g)
                               g))]
               [result (car choices)])
      (if (null? l)
          result
          (let ([v (let ([v (caar l)])
                     (if (pair? v)
                         (car v)
                         v))])
            (cond
	      [(> v goodness) (loop (cdr l) v (car l))]
	      [(and (= v goodness)
		    ; pick randomly
		    (zero? (random 2)))
	       (loop (cdr l) v (car l))]
	      [else (loop (cdr l) goodness result)]))))))
