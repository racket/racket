#lang racket
(provide/contract [solve (-> (listof (listof integer?))  ; row-info
                             (listof (listof integer?))  ; col-info
                             (-> number? number? symbol? ; set-entry
                                 void?)
                             (-> number? void?)          ; setup-progress
                             void)])

(define (solve row-info col-info set-entry setup-progress)
  (local [
          (define (pause) '(sleep 1/16))
          
          ; all test cases are commented out.
          
          ; to work on large lists, we must make filter tail-recursive. 
          ; this one reverses.
          
          ; filter-rev : returns a list of all elements in a-list which 
          ; satisfy the predicate.  If a precedes b in a-list, and both
          ; occur in the result, then b will precede a in the result.
          ; ((A -> boolean) (list-of A) -> (list-of A))
          
          (define (filter-rev fun a-list)
            (foldl (lambda (elt built-list) 
                     (if (fun elt) 
                         (cons elt built-list)
                         built-list))
                   null
                   a-list))
          
          ;(equal? (filter-rev (lambda (x)  (> x 13)) '(2 98 27 1 23 2 09))
          ;       '(23 27 98))
          
          
          ; transpose : transposes a matrix represented as a list of lists
          ; ((list-of (list-of T)) -> (list-of (list-of T)))
          
          (define (transpose list-list)
            (apply map list list-list))
          
          ;(equal? (transpose '((a b c d e)
          ;                    (f g h i j)
          ;                    (k l m n o)))
          ;       '((a f k)
          ;         (b g l)
          ;         (c h m)
          ;         (d i n)
          ;         (e j o)))
          
          
          ; TYPE-DECLARATIONS:
          ; there are three kinds of cell-list: the board-row-list, the tally-list, and the try-list.
          ;    
          ; (type: board-row (list-of (union 'off 'on 'unknown)))
          ; (type: tally-row (list-of (union 'off 'on 'unknown 'maybe-off 'maybe-on 'mixed)))
          ; (type: try-row (list-of (union 'maybe-off 'maybe-on 'unknown)))
          (define try-row? (listof (symbols 'maybe-off 'maybe-on 'unknown)))
          (define try-batch? (listof (or/c number? (listof try-row?))))
          
          ;
          ; (type: board (list-of board-row))
          
          ; board-ref : returns the board element in (col,row);
          ; (board num num -> (union 'on 'off 'unknown))
          
          (define (board-ref board row col)
            (list-ref (list-ref board row) col))
          
          ; board-width : returns the width of the board
          ; (board -> num)
          
          (define (board-width board)
            (length (car board)))
          
          ; board-height : returns the height of the board
          ; (board -> num)
          
          (define (board-height board)
            (length board))
          
          ; extract-rows : returns the board as a list of rows
          ; (board -> board)
          
          (define (extract-rows board)
            board)
          
          ; extract-cols : returns the board as a list of columns
          ; (board -> board)
          
          (define (extract-cols board)
            (transpose board))
          
          ; reassemble-rows : turns a list of rows into a board
          ; (board -> board)
          
          (define (reassemble-rows board-line-list)
            board-line-list)
          
          ; reassemble-cols : turns a list of columns into a board
          ; (board -> board)
          
          (define (reassemble-cols board-line-list)
            (transpose board-line-list))
          
          ; entirely-unknown : does this row consist entirely of 'unknown?
          
          (define (entirely-unknown row)
            (andmap (lambda (x) (eq? x 'unknown)) row))
          
          ; finished? : does this board contain no unknown squares?
          
          (define (finished? board)
            (not (ormap (lambda (row) (ormap (lambda (cell) (eq? cell 'unknown)) row)) board)))
          
          
          ; threshold info : the threshold is the limit at which
          ; memoize-tries will simply give up.
          
          (define initial-threshold 2000)
          
          (define (next-threshold threshold)
            (+ threshold 2000))
          
          
          ; procedures to simplify the construction of test cases:
          
          ; condensed->long-form : takes a tree of short-form symbols and
          ; converts them to their long form, following this mapping:
          ; u -> unknown     |  X -> off
          ; ? -> maybe-on    |  O -> on
          ; ! -> maybe-off   |  * -> mixed
          
          (define (condensed->long-form symbol-tree)
            (cond [(cons? symbol-tree)
                   (cons (condensed->long-form (car symbol-tree))
                         (condensed->long-form (cdr symbol-tree)))]
                  [(case symbol-tree
                     ((u) 'unknown)
                     ((?) 'maybe-on)
                     ((!) 'maybe-off)
                     ((X) 'off)
                     ((O) 'on)
                     ((*) 'mixed)
                     ((()) '())
                     (else (error 'condensed->long-form "bad input: ~a" symbol-tree)))]))
          
          ;(equal? (condensed->long-form '(((? !) u) (* () X O)))
          ;        '(((maybe-on maybe-off) unknown) (mixed () off on)))
          
          ; check-changed : check whether a tally-row reveals new information to be added
          ; to the grid
          ; (tally-row -> boolean)
          
          (define (check-changed tally-list)
            (ormap (lambda (cell)
                     (case cell
                       ((off on unknown mixed) #f)
                       ((maybe-off maybe-on) #t)
                       (else (error "unknown element found in check-changed: ~a" cell))))
                   tally-list))
          
          ;(and (equal? (check-changed '(off off on unknown mixed)) #f)
          ;       (equal? (check-changed '(off on maybe-off on mixed)) #t)
          ;       (equal? (check-changed '(off maybe-on on on unknown)) #t))
          
          ; rectify : transform a tally-row into a board row, by changing maybe-off
          ; to off and maybe-on to on.
          ; (tally-row -> board-row)
          
          (define (rectify tally-list)
            (map (lambda (cell)
                   (case cell
                     ((off on unknown) cell)
                     ((maybe-off) 'off)
                     ((maybe-on) 'on)
                     ((mixed) 'unknown)
                     (else (error "unknown element in rectified row"))))
                 tally-list))
          
          ;(equal? (rectify '(off on maybe-on mixed unknown maybe-off))
          ;       '(off on on unknown unknown off))
          
          ; make-row-formulator:
          ; given a set of block lengths, create a function which accepts a 
          ; set of pads and formulates a try-row:
          ; (num-list -> (num-list num -> (list-of (union 'maybe-off 'maybe-on 'unknown))))
          
          (define (make-row-formulator blocks)
            (lambda (pads)
              (apply append
                     (let loop ([pads pads]
                                [blocks blocks])
                       (cond [(null? (cdr pads)) 
                              (if (null? blocks)
                                  (list (build-list (car pads) (lambda (x) 'maybe-off)))
                                  (list (cons 'maybe-off (build-list (apply + -1 (car pads) blocks) (lambda (x) 'unknown)))))]
                             [else 
                              (cons (build-list (car pads) (lambda (x) 'maybe-off))
                                    (cons (build-list (car blocks) (lambda (x) 'maybe-on))
                                          (loop (cdr pads) (cdr blocks))))])))))
          
          #|
            (equal? ((make-row-formulator '(3 1 1 5)) '(1 2 1 3 3))
                    '(maybe-off maybe-on maybe-on maybe-on maybe-off maybe-off maybe-on maybe-off maybe-on 
                                maybe-off maybe-off maybe-off maybe-on maybe-on maybe-on maybe-on maybe-on
                                maybe-off maybe-off maybe-off))
            
            (equal? ((make-row-formulator '(3 1 1 5)) '(2 4 4))
                    '(maybe-off maybe-off
                      maybe-on maybe-on maybe-on
                      maybe-off maybe-off maybe-off maybe-off
                      maybe-on
                      unknown unknown unknown unknown unknown unknown unknown unknown unknown unknown))
            |#
          
          #| check-try :
            see whether a try fits with the existing row information (curried)
            (tally-row -> (try-row -> boolean))
            |#
          
          (define (check-try tally-list)
            (lambda (try-list)
              (andmap (lambda (tally try)
                        (or (eq? try 'unknown)
                            (case tally
                              ((off) (eq? try 'maybe-off))
                              ((on) (eq? try 'maybe-on))
                              (else #t))))
                      tally-list
                      try-list)))
          
          #|
            (equal? ((check-try '(unknown off on unknown unknown unknown))
                     '(maybe-on maybe-on maybe-on maybe-off maybe-off maybe-off))
                    #f)
            
            (equal? ((check-try '(unknown off on unknown unknown unknown))
                     '(maybe-off maybe-off maybe-on maybe-on maybe-on maybe-off))
                    #t)
            
            (equal? ((check-try '(unknown off on unknown unknown unknown))
                     '(unknown unknown unknown unknown unknown unknown))
                    #t)
            |#
          
          #| choose : like math.  as in, "9 choose 3"
            (num num -> num)
            |#
          
          (define (factorial a)
            (if (<= a 1)
                1
                (* a (factorial (- a 1)))))
          
          (define (choose a b)
            (if (> b a)
                (error 'choose "(choose ~v ~v): ~v is greater than ~v" a b b a)
                (let ([b (max b (- a b))])
                  (/ (let loop ([x a])
                       (if (= x (- a b))
                           1
                           (* x (loop (- x 1))))) 
                     (factorial b)))))
          
          #|
            (and
             (= (choose 0 0) 1)
             (= (choose 10 10) 1)
             (= (choose 10 1) 10)
             (= (choose 10 2) 45)
             (= (choose 10 8) 45))
            |#
          
          #| initial-num-possibilities :
            given a list of block lengths, calculate the number of ways they could fit
            into a row of the given length.  The easiest way to model this is to imagine
            inserting blocks at given locations in a fixed set of spaces
            
            (listof num) num -> num
            
            |#
          
          (define (initial-num-possibilities blocks size)
            (choose (+ 1 (- size (apply + blocks))) (length blocks)))
          
          #|
            (= (initial-num-possibilities '(2 3 3 4) 40) 
               (choose 29 4))
            |#
          
          #| build-possibles:
            builds a list of the possible rows.  given a number of spaces, and a number
            of bins to put the spaces in, and a row-formulator, and a line-checker predicate,
            build-possibles makes a list of every possible row which passes the predicate.
            If the number of possibilities grows larger than the threshold, the search is
            aborted.
            
            (num num ((list-of num) -> try-row) (try-row -> bool) num -> (union (list-of try-row) #f))
            |#
          
          (define (build-possibles things total-bins row-formulator line-checker threshold)
            (let/ec escape
              (let* ([built-list null]
                     [list-length 0]
                     [add-to-built-list
                      (lambda (new) 
                        (if (= list-length threshold)
                            (escape #f)
                            (begin (set! built-list (cons new built-list))
                                   (set! list-length (+ list-length 1)))))])
                (let tree-traverse ([things things]
                                    [bins total-bins]
                                    [so-far-rev null])
                  (let* ([this-try-rev (cons things so-far-rev)]
                         [formulated (row-formulator (reverse this-try-rev))])
                    ;(when (= debug-counter 0)
                    ;  (printf "~v\n~v\n" formulated (line-checker formulated)))
                    (when (or (= bins total-bins) (line-checker formulated))
                      (if (= bins 1)
                          (add-to-built-list formulated)
                          (let try-loop ([in-this-bin (if (= bins total-bins)
                                                          0
                                                          1)])
                            (unless (> (+ in-this-bin (- bins 2)) things)
                              (tree-traverse (- things in-this-bin)
                                             (- bins 1)
                                             (cons in-this-bin so-far-rev))
                              (try-loop (+ in-this-bin 1))))))))
                built-list)))
          
          
          #| 
            ;build-possibles test case
             (let* ([row-formulator-one (make-row-formulator '(2))]
                    [line-checker (check-try '(unknown unknown unknown on unknown unknown))]
                    [test-one (build-possibles 4 2 row-formulator-one line-checker 10000)]
                    [row-formulator-two (make-row-formulator '(1 1))]
                    [test-two (build-possibles 4 3 row-formulator-two line-checker 10000)])
               (and 
                (equal? test-one
                        '((maybe-off maybe-off maybe-off maybe-on maybe-on maybe-off)
                          (maybe-off maybe-off maybe-on maybe-on maybe-off maybe-off)))
                (equal? test-two
                        '((maybe-off maybe-off maybe-off maybe-on maybe-off maybe-on)
                          (maybe-off maybe-on maybe-off maybe-on maybe-off maybe-off)
                          (maybe-on maybe-off maybe-off maybe-on maybe-off maybe-off)))))
            |#
          
          #| spare-spaces:
            calculates the number of spare spaces in a line. In other words,
            line-length - sum-of-all-blocks
            
            ((list-of num) num -> num)
            |#
          
          (define (spare-spaces block-list line-length)
            (let* ([black-spaces (apply + block-list)]
                   [spare-spaces (- line-length black-spaces)])
              spare-spaces))
          
          ; first-pass:
          ; generates the information about row contents which can be inferred directly
          ; from the block info and nothing else (i.e., uses no information from an existing
          ; board.
          ; ((list-of (list-of num)) num -> (list-of (list-of (union 'on 'unknown))))
          
          (define (first-pass info-list line-length)
            (let ((row-pass
                   (lambda (block-list)
                     (let* ([spares (- (spare-spaces block-list line-length) (max 0 (- (length block-list) 1)))]
                            [shortened-blocks
                             (map (lambda (block-length) (- block-length spares))
                                  block-list)]
                            [all-but-start
                             (foldr append null
                                    (let build-row-loop ([blocks-left shortened-blocks])
                                      (if (null? blocks-left)
                                          null
                                          (let ([extra-pad (if (null? (cdr blocks-left)) 0 1)])
                                            (if (> (car blocks-left) 0)
                                                (cons (build-list (car blocks-left) (lambda (x) 'on))
                                                      (cons (build-list (+ spares extra-pad) (lambda (x) 'unknown))
                                                            (build-row-loop (cdr blocks-left))))
                                                (cons (build-list (+ spares extra-pad (car blocks-left))
                                                                  (lambda (x) 'unknown))
                                                      (build-row-loop (cdr blocks-left))))))))]
                            [whole-row (append (build-list spares (lambda (x) 'unknown))
                                               all-but-start)])
                       whole-row))))
              (map row-pass info-list)))
          
          #|
            (let ([test-result (first-pass '((4 3) (5 1)) 10)])
              (equal? test-result '((unknown unknown on on unknown unknown unknown on unknown unknown)
                                    (unknown unknown unknown on on unknown unknown unknown unknown unknown))))
            |#
          
          #| unify-passes:
            unify the result of running first-pass on both the rows and the columns
            (let ([BOARD (list-of (list-of (union 'unknown 'on)))])
              (BOARD BOARD -> BOARD))
            |#
          
          (define (unify-passes board-a board-b)
            (let ([unify-rows
                   (lambda (row-a row-b)
                     (map (lambda (cell-a cell-b)
                            (case cell-a
                              ((on) 'on)
                              (else cell-b)))
                          row-a row-b))])
              (map unify-rows board-a board-b)))
          
          #|
            (let* ([board-a '((unknown unknown on) (on unknown unknown))]
                   [board-b '((unknown on unknown) (on on unknown))]
                   [test-result (unify-passes board-a board-b)])
              (equal? test-result '((unknown on on) (on on unknown))))
            |#
          
          #| whole-first-pass:
            take a set of row descriptions and the board dimensions and generate the 
            merged first-pass info
            ((list-of (list-of num)) (list-of (list-of num)) num num -> 
             (list-of board-row))
            |#
          
          (define (whole-first-pass row-info col-info width height)
            (unify-passes (first-pass row-info width)
                          (transpose (first-pass col-info height))))
          
          #| memoize-tries:
            given the black block widths and the line length and some initial board
            and a progress-bar updater, calculate all possibilities for each row.
            If skip-unknowns is #t, rows whose content is entirely unknown will be 
            skipped, and #f returned for that row.
            effect: updates the progress bar
            ((list-of (list-of num)) num (list-of board-row) (-> void) boolean -> (union (list-of try-row) #f))
            |#
          
          (define (memoize-tries info-list line-length board-rows old-tries threshold)
            (let* ([unmemoized (filter number? old-tries)])
              (if (null? unmemoized)
                  old-tries
                  (let* ([least-difficult
                          (apply min unmemoized)])
                    ;(eprintf "guessed tries: ~v\n" least-difficult)
                    (map (lambda (old-try-set block-list board-row)
                           (cond [(and (number? old-try-set) (= old-try-set least-difficult))
                                  (let ([spaces (spare-spaces block-list line-length)]
                                        [bins (+ (length block-list) 1)]
                                        [row-formulator (make-row-formulator block-list)]
                                        [line-checker (check-try board-row)])
                                    (or (build-possibles spaces bins row-formulator line-checker threshold)
                                        (* 2 old-try-set)))]
                                 [else old-try-set]))
                         old-tries
                         info-list
                         board-rows)))))
          
          #|
            (equal? (memoize-tries '((4) (1 3)) 
                                   6 
                                   '((unknown on unknown unknown unknown unknown)
                                     (unknown off unknown unknown unknown unknown))
                                   void)
                    '(((maybe-on maybe-on maybe-on maybe-on maybe-off maybe-off)
                       (maybe-off maybe-on maybe-on maybe-on maybe-on maybe-off))
                      ((maybe-on maybe-off maybe-on maybe-on maybe-on maybe-off)
                       (maybe-on maybe-off maybe-off maybe-on maybe-on maybe-on))))
            |#
          
          #| batch-try: 
            take a board-line list and a list of possibles, and trim it down by 
            checking each try-list against the appropriate board-line
            
            ((list-of board-row) (list-of (union (list-of try-row) #f)) -> (list-of (union (list-of try-row) #f)))
            |#
          
          (define (batch-try board-line-list try-list-list-list)
            (map (lambda (line try-list-list)
                   (if (not (number? try-list-list))
                       (filter ; filter-rev
                        (let ([f (check-try line)])
                          (lambda (try-list) (f try-list)))
                        try-list-list)
                       try-list-list))
                 board-line-list
                 try-list-list-list))
          
          #|
            (equal? (batch-try '((unknown unknown unknown off)
                                 (unknown on unknown unknown))
                               '(((maybe-on maybe-on maybe-on maybe-off)
                                  (maybe-off maybe-on maybe-on maybe-on))
                                 ((maybe-on maybe-on maybe-off maybe-off)
                                  (maybe-off maybe-on maybe-on maybe-off)
                                  (maybe-off maybe-off maybe-on maybe-on))))
                    '(((maybe-on maybe-on maybe-on maybe-off))
                      ((maybe-off maybe-on maybe-on maybe-off)
                       (maybe-on maybe-on maybe-off maybe-off))))
            |#
          
          ; tabulate-try : take one possibility, and merge it with the row possibles
          ; (tally-list try-list) -> tally-list
          
          (define (tabulate-try tally-list try-list)
            (map (lambda (tally try)
                   (case tally
                     ((off on mixed) tally)
                     ((unknown) try)
                     ((maybe-off maybe-on) (if (eq? try tally)
                                               try
                                               'mixed))
                     (else (error "unknown cell type during tabulate-try: ~a" tally))))
                 tally-list
                 try-list))
          
          
          #|
            (equal? (tabulate-try '(on off maybe-off maybe-off maybe-on maybe-on maybe-on)
                                  '(on off mixed maybe-on maybe-on mixed maybe-off))
                    '(on off mixed mixed maybe-on mixed mixed))
             |#
          
          ; batch-tabulate : take a board-line-list and a list of sets of tries which check with the board
          ;  and tabulate them all to produce a new board line list (before rectification)
          ; (board-line-list try-list-list-opt-list) -> tally-list
          (define (batch-tabulate board-line-list try-list-list-opt-list)
            (map (lambda (board-line try-list-list-opt)
                   (if (not (number? try-list-list-opt))
                       (foldl (lambda (x y) (tabulate-try y x)) board-line try-list-list-opt)
                       board-line))
                 board-line-list
                 try-list-list-opt-list))
          
          
          ;  (equal? (batch-tabulate '((unknown unknown unknown off)
          ;                       (unknown unknown on unknown))
          ;                     '(((maybe-on maybe-on maybe-off maybe-off)
          ;                        (maybe-off maybe-on maybe-on maybe-off))
          ;                       ((maybe-off maybe-on maybe-on maybe-off)
          ;                        (maybe-off maybe-off maybe-on maybe-on))))
          ;     '((mixed maybe-on mixed off)
          ;       (maybe-off mixed on mixed)))
          
          (define (print-board board)
            (for-each (lambda (row)
                        (for-each (lambda (cell)
                                    (printf (case cell
                                              ((off) " ")
                                              ((unknown) ".")
                                              ((on) "#"))))
                                  row)
                        (printf "\n"))
                      (extract-rows board)))
          
          ; animate-changes takes a board and draws it on the main screen
          (define (animate-changes board draw-thunk outer-size inner-size)
            (let outer-loop ([outer-index 0])
              (if (= outer-index outer-size)
                  null
                  (let inner-loop ([inner-index 0])
                    (if (= inner-index inner-size)
                        (begin
                          (pause)
                          (outer-loop (+ outer-index 1)))
                        (begin
                          (draw-thunk board outer-index inner-index)
                          (inner-loop (+ inner-index 1))))))))
          
          (define (draw-rows-thunk board row col)
            (set-entry col row (board-ref board row col)))
          
          (define (draw-cols-thunk board col row)
            (set-entry col row (board-ref board row col)))
          
          ;  (print-board '((on on unknown off)
          ;                 (on on unknown unknown)
          ;                 (unknown unknown on on)
          ;                 (off unknown on on)))
          
          ; do-lines takes a board-line-list and a try-list-list-list and returns two things: a tally-list-list
          ; and a new try-list-list-list
          ; (board-line-list try-list-list-opt-list) -> (tally-list-list try-list-list-opt-list)
          (define do-lines
            (contract
             (->* (any/c try-batch?)
                  (values (listof (listof any/c))  try-batch?))
             (lambda (board-line-list try-list-list-opt-list)
               (let ([new-tries (batch-try board-line-list try-list-list-opt-list)])
                 (values (batch-tabulate board-line-list new-tries)
                         new-tries)))
             'do-lines
             'caller))
          
          ; full-set takes a board and a pair of try-list-list-lists and returns a new board, a new pair
          ; of try-list-list-lists, and a boolean (whether it's changed)
          (define full-set
            (contract
             (->* (any/c try-batch? try-batch?)
                  (values any/c try-batch? try-batch? boolean?))
             (lambda (board row-try-list-list-opt-list  col-try-list-list-opt-list)
               (let*-values ([(board-rows new-row-tries)
                              (do-lines (extract-rows board) row-try-list-list-opt-list)]
                             [(row-changed)
                              (ormap check-changed board-rows)]
                             [(new-board)
                              (reassemble-rows (map rectify board-rows))]
                             [( _ )
                              (when row-changed
                                (animate-changes new-board draw-rows-thunk 
                                                 (board-height new-board)
                                                 (board-width new-board)))]
                             [(board-cols new-col-tries)
                              (do-lines (extract-cols new-board) col-try-list-list-opt-list)]
                             [(col-changed)
                              (ormap check-changed board-cols)]
                             [(final-board)
                              (reassemble-cols (map rectify board-cols))]
                             [( _ )
                              (when col-changed
                                (animate-changes final-board draw-cols-thunk
                                                 (board-width final-board)
                                                 (board-height final-board)))])
                 (values final-board new-row-tries new-col-tries (or row-changed col-changed))))
             'full-set
             'caller))
          
          ; on 2002-10-17, I wrapped another layer of looping around the inner loop.
          ; the purpose of this outer loop is to allow the solver to ignore rows (or
          ; columns) about which the solver knows nothing for as long as possible.
          
          (define (local-solve row-info col-info)
            (let* ([rows (length row-info)]
                   [cols (length col-info)]
                   [initial-board (whole-first-pass row-info col-info cols rows)]
                   [_ (animate-changes initial-board draw-cols-thunk
                                       (board-width initial-board)
                                       (board-height initial-board))])
              (let outer-loop ([outer-board initial-board]
                               [skip-threshold initial-threshold]
                               [old-row-tries (map (lambda (info)
                                                     (initial-num-possibilities info (board-width initial-board))) 
                                                   row-info)]
                               [old-col-tries (map (lambda (info)
                                                     (initial-num-possibilities info (board-height initial-board))) 
                                                   col-info)])
                (let* ([row-try-list-list-opt-list (memoize-tries row-info cols outer-board old-row-tries skip-threshold)]
                       [col-try-list-list-opt-list (memoize-tries col-info rows (transpose outer-board) old-col-tries skip-threshold)])
                  (let loop ([board outer-board] 
                             [row-tries row-try-list-list-opt-list]
                             [col-tries col-try-list-list-opt-list]
                             [changed #t])
                    (if changed
                        (call-with-values (lambda () (full-set board row-tries col-tries))
                                          loop)
                        (if (finished? board)
                            board
                            (if (equal? outer-board board)
                                (outer-loop board (next-threshold skip-threshold) row-tries col-tries)
                                (outer-loop board skip-threshold row-tries col-tries)))))))))
          
          ]
    (local-solve row-info col-info)))


; test case:

;(require solve)
;
;(let* ([test-board (build-vector 20 (lambda (x) (make-vector 20 'bad-value)))]
;       [set-board! (lambda (col row val)
;                     (vector-set! (vector-ref test-board row) col val))])
;  (solve `((9 9) (6 10) (5 11) (4 3 5) (2 1 3) (2 4 2) (1 3 6) (5 1 1 1) (2 2 1 3 1) (7 4 1) (7 4 2) (1 3 9) (1 2 4 6) (1 6 9) (1 4 7) (2 1 4 2) (5 3 4) (5 7) (5 10) (5 11))
;         `((1 8) (2 4 4) (4 1 2 4) (8 2 4) (6 1 3 7) (4 8 4) (3 7 1) (1 2) (1 2) (2 2 2 1) (3 2 1 1 1 2) (7 8 2) (3 7 4 2) (3 1 1 2 3 3) (3 4 6 3) (4 1 4 3) (4 1 4 4) (5 1 4 4) (7 4 5) (7 6 5))
;         set-board!
;         (lambda (x) (void)))
;  (equal? (map (lambda (row) 
;                 (apply string-append
;                        (map (lambda (x)
;                               (case x
;                                 [(off) " "]
;                                 [(on) "x"])) 
;                             row)))
;               (apply map list (map vector->list (vector->list test-board))))
;          
;          `("x       xxxxxxxx    "
;           "xx     xxxx    xxxx "
;           "xxxx   x xx     xxxx"
;           "xxxxxxxx xx     xxxx"
;           "xxxxxx x xxx xxxxxxx"
;           "xxxx   xxxxxxxx xxxx"
;           "xxx     xxxxxxx    x"
;           "x            xx     "
;           "x           xx      "
;           "  xx    xx  xx     x"
;           " xxx  xx x  x   x xx"
;           "xxxxxxx  xxxxxxxx xx"
;           "xxx  xxxxxxx xxxx xx"
;           "xxx  x  x xx xxx xxx"
;           "xxx  xxxx xxxxxx xxx"
;           "xxxx  x    xxxx  xxx"
;           "xxxx  x    xxxx xxxx"
;           "xxxxx x    xxxx xxxx"
;           "xxxxxxx   xxxx xxxxx"
;           "xxxxxxx xxxxxx xxxxx")))
