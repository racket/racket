(module robot mzscheme
  (require "counter.rkt"
           "board.rkt"
           "utils.rkt"
           mzlib/unitsig)
  
  (provide robot)
  
  (define robot
    (lambda (n history-in)
      
      ;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define start-time (current-seconds))
      
      (define RUN-TIME 25) ; 5 seconds less, just to be on the safe side
      
      (define US-PIECE-WEIGHT 10)
      (define US-MIDDLE-PIECE-WEIGHT 1)
      (define US-LINE-PIECE-WEIGHT 1)
      (define US-CONNECT-PIECE-WEIGHT 2)
      
      (define THEM-PIECE-WEIGHT (- US-PIECE-WEIGHT))
      (define THEM-MIDDLE-PIECE-WEIGHT (- US-MIDDLE-PIECE-WEIGHT))
      (define THEM-LINE-PIECE-WEIGHT (- US-LINE-PIECE-WEIGHT))
      (define THEM-CONNECT-PIECE-WEIGHT (- US-CONNECT-PIECE-WEIGHT))
      
      (define THREAD-LIMIT 4)
      (define thread-count 0)
      
      ;; Read in the board state and history,
      ;; converting history to our format
      (define-values (board history as-player turn-number)
        (let loop ([history-in history-in][b (new-board n)][h (new-history)][x? #t][t 0])
          (let ([h (extend-history! b h)])
            (if (null? history-in)
                (values b h (if x? x o) t)
                (begin
                  (let* ([d (caar history-in)]
                         [p (cadar history-in)])
                    (loop (cdr history-in)
                          (push b
                                (case d
                                  [(t) 'top]
                                  [(b) 'bottom]
                                  [(l) 'left]
                                  [(r) 'right])
                                (sub1 p)
                                (if x? x o))
                          h
                          (not x?)
                          (add1 t))))))))
      
      (define RECURSION-DEPTH 
        (if (< turn-number 2)
            1
            #f))
      
      (define WINNER-GOODNESS +inf.0)
      (define LOSER-GOODNESS -inf.0)
      (define MEDIUM-GOODNESS 0)
      
      (define IMMEDIATE-WINNER-GOODNESS (cons WINNER-GOODNESS 1))
      (define IMMEDIATE-LOSER-GOODNESS (cons LOSER-GOODNESS 1))
       
      (define get-goodness 
        (invoke-unit/sig counter@ (n) params^))
      
      ;;  Pick a move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define (make-get-best-move depth)
        (lambda (board history as-player min-needed skip-right?)
          (let* ([futures (n-map (if skip-right? (sub1 n) n)
                                 (lambda (i) (cons (push board 'top i as-player) i)))]
                 [winners (map (lambda (f) (cons (find-winner (car f)) (cdr f))) futures)])
            ;; Can we win??
            (let ([winning-move (ormap (lambda (w) (and (eq? (car w) as-player) (cdr w))) winners)])
              (if winning-move
                  ;; Yea!!
                  (cons IMMEDIATE-WINNER-GOODNESS winning-move)
                  
                  ;; Drop moves where we lose by the other player winning, or we've been
                  ;;  there before
                  (let ([futures 
                         (let loop ([fs futures][ws winners])
                           (cond
                             [(null? fs) null]
                             [(or (caar ws) ; if there's a winner, it's not me!
                                  (find-board-in-history (caar fs) history))
                              (loop (cdr fs) (cdr ws))]
                             [else (cons (car fs) (loop (cdr fs) (cdr ws)))]))])
                    
                    (if (null? futures)
                        ;; No non-losing moves? Pick a default with a very low goodness
                        (cons IMMEDIATE-LOSER-GOODNESS 0)
                        
                        ;; Now for the hard part. 
                        (let* (;; First, estimate the goodness of the possible moves
                               [goodnesses (map (lambda (f)
                                                  (get-goodness (car f) board as-player depth))
                                                futures)]
                               
                               [good-futures (map cons goodnesses futures)]
                               ; Now we have a list of (cons <goodness> (cons <board> <move>))
                               
                               ;; Filter the set of futures to choose a plausible set under the lookahead
                               ;;   threshold:
                               [local-good-futures good-futures]
                               
                               [good-so-far LOSER-GOODNESS]
                               
                               ;; If we're not at depth 0, do a recursive lookahead
                               [good-futures 
                                (if (positive? depth)
                                    (map (lambda (gf)
                                           (cons (if (>= good-so-far min-needed)
                                                     ;; We've got something good enough, so just return LOSER
                                                     IMMEDIATE-LOSER-GOODNESS
                                                     
                                                     (let ([move (search-space (cadr gf) 
                                                                               (extend-history (cadr gf) history)
                                                                               (make-get-best-move (sub1 depth))
                                                                               (other-player as-player)
                                                                               ;; Need something better than what we have...
                                                                               (- (car gf) good-so-far))])
                                                       
                                                       '(when (and (= depth RECURSION-DEPTH))
                                                          (eprintf "Returned goodness: ~a\n"  (car move))
                                                          (print-board (cadr gf) (current-error-port)))
                                                       
                                                       (let ([g (car move)])
                                                         (let* ([new-goodness 
                                                                 (if (number? g)
                                                                     ; normal future
                                                                     (- (car gf) g)
                                                                     
                                                                     ; win/lose - try to delay a loss
                                                                     (cons (- (car g)) (+ 10000 (car gf) (cdr g))))]
                                                                [new-goodness-val (if (pair? new-goodness)
                                                                                      (car new-goodness)
                                                                                      new-goodness)])
                                                           
                                                           ;; New best goodness?
                                                           (when (> new-goodness-val good-so-far)
                                                             (set! good-so-far new-goodness-val))
                                                           
                                                           new-goodness))))
                                                 (cdr gf)))
                                         local-good-futures)
                                    
                                    ; Use what we have
                                    local-good-futures)])
                          
                          '(when (and (= depth RECURSION-DEPTH))
                             (for-each (lambda (gf)
                                         (eprintf "Goodness: ~a\n"  (car gf))
                                         (print-board (cadr gf) (current-error-port)))
                                       good-futures))
                          
                          
                          (let ([r (if (andmap (lambda (x) (and (pair? (car x)) (= (caar x) LOSER-GOODNESS))) good-futures)
                                       ;; All losers; pick to delay the inevitable
                                       (let ([m (pick-best (map (lambda (x) (cons (cdar x) (cdr x))) good-futures))])
                                         (cons (cons LOSER-GOODNESS (car m)) (cdr m)))
                                       
                                       ;; Weighted non-losses. Pick one now:
                                       (pick-best good-futures))])
                            
                            ;; Strip the board out, getting just the index
                            (cons (car r) (cddr r)))))))))))
      
      
      ;; Symmetry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; search-space is a symmetry helper. Lets you write a move chooser,
      ;; get-best-move, that only needs to consider top pushes; the results
      ;; from calling get-best-move four times are filtered down to a single
      ;; choice. Also uses threads in the hope of exploiting extra
      ;; processors.
      
      (define (search-space board history get-best-move as-player min-needed)
        ;; get-best-move is called with:
        ;;        board  history as-player skip-right?
        ;;  It is actually called four times, once for each rotation of the board, so
        ;;  get-best-move should only contemplate the space derived from pushing
        ;;  at the top on the next move. If skip-right is #t, it shouldn't
        ;;  contemplate pushing into the leftmost column because nothing's there;
        ;;  a different rotation will catch the possibility.
        ;; The return value of get-best-move should be
        ;;   (cons <goodness-rating> <push-position>)
        ;;  where <goodness-rating> is a number or
        ;;     (box <aux-goodness>)
        ;;  the box means loser; if all moves lose, the best <aux-godness> 
        ;;  is picked.
        ;; The return values of `search-space' is
        ;;   (cons <goodness-rating> (cons <side> <push-position>))
        ;;  where <side> is in '(#\T #\B #\R #\L)
        
        (let* ([top-board board]
               [right-board (rotate-cw top-board 1)]
               [bottom-board (rotate-cw right-board 1)]
               [left-board (rotate-cw bottom-board 1)]
               [moves (make-vector 4)]
               [win-move #f]
               [s (make-semaphore 0)]
               [go (lambda (parallel? board pos transform)
                     (let ([f (lambda ()
                                (let ([v (transform 
                                          (get-best-move board history as-player
                                                         min-needed
                                                         (eq? none (board-cell board (sub1 n) 0))))])
                                  (vector-set! moves pos v)
                                  (let ([value (if (pair? (car v))
                                                   (caar v)
                                                   (car v))])
                                    (when (>= value min-needed)
                                      ; Exceeded minimum necessary weight - short-circuit the rest
                                      (set! win-move v)
                                      (semaphore-post s)
                                      (semaphore-post s)
                                      (semaphore-post s))
                                    (semaphore-post s))))])
                       (unless win-move
                         (if (and parallel? (< thread-count THREAD-LIMIT))
                             (thread (lambda ()
                                       ;; No locking on thread-count. It will be a little
                                       ;;  inaccurate, but who cares?
                                       (set! thread-count (add1 thread-count))
                                       (f)
                                       (set! thread-count (sub1 thread-count))))
                             (f)))))])
          (go #t top-board 0 (lambda (x) (cons (car x) (cons #\T (cdr x)))))
          (go #t bottom-board 1 (lambda (x) (cons (car x) (cons #\B (- n 1 (cdr x))))))
          (go #t left-board 2 (lambda (x) (cons (car x) (cons #\R (cdr x)))))
          (go #f right-board 3 (lambda (x) (cons (car x) (cons #\L (- n 1 (cdr x))))))
          (semaphore-wait s)
          (semaphore-wait s)
          (semaphore-wait s)
          (semaphore-wait s)
          (if win-move
              win-move
              (let ([l (vector->list moves)])
                (if (andmap (lambda (x) (and (pair? (car x)) (= (caar x) LOSER-GOODNESS))) l)
                    ;; All losers; pick to delay the inevitable
                    (let ([m (pick-best (map (lambda (x) (cons (cdar x) (cdr x))) l))])
                      (cons (cons LOSER-GOODNESS (car m)) (cdr m)))
                    
                    ;; Pick a non-losing move
                    (pick-best l))))))
      
      ;; Using up our time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; Loop, choosing a deeper level of dept-first searching
      ;;  each time. Kill everything an use the most-recently
      ;;  computed value if we run out of time. The goodness
      ;;  cache is reset each time; presumably, each iteration
      ;;  calculates more precise values, overriding the old ones.
      
      (define (use-up-time f)
        (let ([result (cons (list-ref '(#\T #\B #\L #\R) (random 4)) (random n))] ; worst-case default
              [c (make-custodian)])
          (parameterize ([current-custodian c])
            (thread 
             (lambda ()
               (let loop ([iteration 0])
                 ; (eprintf "Starting iteration ~a\n" iteration)
                 (set! result (f iteration))
                 '(eprintf " [finished iteration depth ~a: ~a~a]\n" 
                           iteration (cadr result) (add1 (cddr result)))
                 (unless (or (pair? (car result)))
                   (loop (add1 iteration)))))))
          (let loop ()
            (let ([sleep-time (- RUN-TIME (- (current-seconds) start-time))])
              (when (> sleep-time 1) 
                (sleep 1)
                (unless (pair? (car result)) ; pair indicates win/lose
                  (loop)))))
          result))
      
      
      ;; Result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define (go)
        '(begin
           (eprintf "Start:\n")
           (print-board board (current-error-port)))
        (let* ([go (lambda (i) 
                     (set! RECURSION-DEPTH i)
                     (search-space board history (make-get-best-move i) as-player WINNER-GOODNESS))]
               [depth RECURSION-DEPTH]
               [result (if depth
                           (go depth)
                           (use-up-time go))])
          '(when (pair? (car result))
             (eprintf "we ~a\n"
                      (if (= (caar result) LOSER-GOODNESS) "lose" "win")))
          (output-move (cdr result))))
      
      ;; Given (cons <side> <index>), returns the move
      ;;   <side> is in '(#\T #\B #\R #\L)
      (define (output-move move)
        (list (case (car move)
                [(#\T) 't]
                [(#\B) 'b]
                [(#\R) 'r]
                [(#\L) 'l])
              (add1 (cdr move))))
      
      (go))))
