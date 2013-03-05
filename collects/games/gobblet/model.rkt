#lang racket
(require "sig.rkt"
         racket/unit)

(provide model-unit@)

;; Most tests are in test-model.rkt, but for better coverage,
;; uncomment the tests below for unexported functions when running
;; the test suite.

(define-unit model-unit@
  (import config^)
  (export model^)
  
  (define JR? (= BOARD-SIZE 3))
  (define SIZES (if (= BOARD-SIZE 3)
                    '(0 1 2)
                    '(0 1 2 3)))
  (define PIECE-COUNT (sub1 BOARD-SIZE))
  
  ;; A piece is
  ;;  (make-piece num sym hash-table)
  ;;  where the sym is 'red or 'yellow
  ;; 
  ;; The hash table maps a stack without this
  ;; piece to a stack with this piece on top.
  (define-struct piece (size color gobble-table))  
  
  (define red-pieces (map (lambda (sz) (make-piece sz 'red (make-hasheq))) SIZES))
  (define yellow-pieces (map (lambda (sz) (make-piece sz 'yellow (make-hasheq))) SIZES))
  
  ;; Fill in stacks for pieces. By building each possible
  ;;  stack once, we avoid allocating redudant stacks, and
  ;;  we get a value we can eq-hash for canonicalization.
  (define all-stacks
    (let loop ([red-pieces red-pieces]
               [yellow-pieces yellow-pieces]
               [prev-stacks (list null)])
      (if (null? red-pieces)
          ;; Return all unique stacks:
          prev-stacks
          ;; Add stacks to first pieces' tables:
          (loop (cdr red-pieces)
                (cdr yellow-pieces)
                (apply
                 append
                 prev-stacks
                 (map (lambda (p)
                        (map (lambda (stack)
                               (let ([new-stack (cons p stack)])
                                 (hash-set! (piece-gobble-table p) stack new-stack)
                                 new-stack))
                             prev-stacks))
                      (list (car red-pieces)
                            (car yellow-pieces))))))))
  
  ;; A board is a 
  ;;  (vector (vector (list piece ...) ...) ...)
  
  (define empty-board
    (make-vector BOARD-SIZE (make-vector BOARD-SIZE null)))
  
  ;; board-ref : board num num -> piece
  (define (board-ref a i j)
    (vector-ref (vector-ref a i) j))
  
  ;; board-set : board num num piece -> board
  (define (board-set a i j p)
    ;; We implement functional update by copying two vectors
    ;;  and modifying them.
    (let ([a2 (copy-vector a)]
          [r2 (copy-vector (vector-ref a i))])
      (vector-set! a2 i r2)
      (vector-set! r2 j p)
      a2))
  
  ;; copy-vector : vector -> vector
  (define (copy-vector v)
    (list->vector (vector->list v)))
  
  ;; Utilities ------------------------------
  
  ;; fold-rowcol : (num alpha -> alpha) alpha -> alpha
  (define (fold-rowcol f v)
    (let iloop ([i 0][v v])
      (if (= i BOARD-SIZE)
          v
          (iloop (add1 i) (f i v)))))
  
  ;; fold-board : (num num alpha -> alpha) alpha -> alpha
  (define (fold-board f v)
    (fold-rowcol (lambda (i v)
                   (fold-rowcol (lambda (j v)
                                  (f i j v))
                                v))
                 v))
  
  ;; other : sym -> sym
  (define (other c)
    (if (eq? c 'red) 'yellow 'red))
  
  ;; Model ------------------------------
  
  ;; move : board piece num-or-#f num-or-#f num num (board -> alpha) (-> alpha)
  ;;        -> alpha
  ;; Given a board, piece, current location of the piece (or #f if
  ;; not on the board), and target location for the piece, either
  ;; allows the move and calls the continuation k with the new
  ;; board, or disallows and calls the failure continuation.
  ;; The given piece and its source must be correct and ok to move.
  (define (move board p from-i from-j to-i to-j k fail-k)
    (let ([pl (board-ref board to-i to-j)])
      ;; The move is allowed if the target space is empty or the
      ;;  top piece is smaller than this one:
      (if (or (null? pl)
              (and (< (piece-size (car pl)) (piece-size p))
                   (or from-i
                       JR?
                       ;; In 4x4 game, can't move onto board on top
                       ;;  of piece unless it's part of 3-in-a-row
                       (and (not (eq? (piece-color (car pl)) (piece-color p)))
                            (3-in-a-row? board to-i to-j (piece-color (car pl)))))))
          ;; Allowed:
          (let ([new-board (if from-i
                               ;; Remove the piece from the old spot:
                               (board-set board from-i from-j 
                                          (cdr (board-ref board from-i from-j)))
                               ;; Wasn't on the board, yet:
                               board)])
            ;; Add the piece at its new spot, and continue
            (k (board-set new-board to-i to-j (gobble p pl))))
          ;; Not allowed; fail
          (fail-k))))
  
  ;; gobble : piece (listof piece) -> (listof piece)
  (define (gobble p l)
    (hash-ref (piece-gobble-table p) l))
  
  ;; - - - - - - - - - - - - - - - - - -
  
  ;; winner? : board sym -> bool
  ;;  Checks whether the given color has BOARD-SIZE in a row
  (define (winner? board c)
    (or (n-in-a-diag? BOARD-SIZE board c backslash-diag-flip)
        (n-in-a-diag? BOARD-SIZE board c slash-diag-flip)
        ;; Rows and columns:
        (fold-rowcol (lambda (i v)
                       (or v
                           (and 
                            ;; Before we count in all directions,
                            ;;  check the target square.
                            (let ([pl (board-ref board i i)])
                              (and (pair? pl)
                                   (eq? c (piece-color (car pl)))))
                            ;; Target square matches, so on to expensive check
                            (n-in-a-row/col? BOARD-SIZE board i i c))))
                     #f)))
  
  ;; 3-in-a-row? : board num num color -> bool
  (define (3-in-a-row? board i j c)
    (or (n-in-a-row/col? 3 board i j c)
        (and (= i j)
             (n-in-a-diag? 3 board c backslash-diag-flip))
        (and (= i (- BOARD-SIZE 1 j))
             (n-in-a-diag? 3 board c slash-diag-flip))))
  
  ;; n-in-a-row/col? : num board num num color -> bool
  (define (n-in-a-row/col? n board i j c)
    (let ([row/col?
           (lambda (board-ref)
             (= n
                (fold-rowcol (lambda (z v)
                               (+ v
                                  (let ([pl (board-ref z)])
                                    (if (and (pair? pl)
                                             (eq? c (piece-color (car pl))))
                                        1
                                        0))))
                             0)))])
      (or (row/col? (lambda (z) (board-ref board i z)))
          (row/col? (lambda (z) (board-ref board z j))))))
  
  ;; n-in-a-diag? : num board color (num -> num) -> bool
  (define (n-in-a-diag? n board c flip)
    (= n
       (fold-rowcol (lambda (i v)
                      (+ v
                         (let ([pl (board-ref board i (flip i))])
                           (if (and (pair? pl)
                                    (eq? c (piece-color (car pl))))
                               1
                               0))))
                    0)))
  (define backslash-diag-flip (lambda (x) x))
  (define slash-diag-flip (lambda (x) (- BOARD-SIZE 1 x)))
  
  ;; Tests for unexported helpers:
  #;
  (let* ([one-red-board (move empty-board (car red-pieces) #f #f 0 0 values void)]
         [two-red-board (move (move one-red-board (car red-pieces) #f #f 0 2 values void)
                              (car yellow-pieces) #f #f 2 2 values void)]
         [three-red-board (move two-red-board (cadr red-pieces) #f #f 1 1 values void)])
    (define (test x y)
      (unless (equal? x y) 
        (error 'test "failure!: ~s ~s\n" x y)))
    (test #f (n-in-a-row/col? 1 empty-board 0 0 'red))
    (test #t (n-in-a-row/col? 1 one-red-board 0 0 'red))
    (test #t (n-in-a-row/col? 2 two-red-board 0 0 'red))
    (test #t (n-in-a-row/col? 1 two-red-board 2 2 'red))
    (test #f (n-in-a-row/col? 2 two-red-board 2 2 'red))
    (test #t (n-in-a-row/col? 2 two-red-board 0 1 'red))
    (test #t (n-in-a-row/col? 1 three-red-board 0 1 'red))
    (test #t (n-in-a-row/col? 2 three-red-board 0 1 'red))
    
    (test #f (n-in-a-diag? 1 one-red-board 'red slash-diag-flip))
    (test #t (n-in-a-diag? 1 one-red-board 'red backslash-diag-flip))
    (test (= BOARD-SIZE 3) (n-in-a-diag? 1 two-red-board 'red slash-diag-flip))
    (test #f (n-in-a-diag? 2 two-red-board 'red slash-diag-flip))
    (test #t (n-in-a-diag? 1 two-red-board 'red backslash-diag-flip))
    (test (= BOARD-SIZE 3) (n-in-a-diag? 2 three-red-board 'red slash-diag-flip))
    (test #t (n-in-a-diag? 2 three-red-board 'red backslash-diag-flip)))
  
  
  ;; - - - - - - - - - - - - - - - - - -
  
  ;; available-off-board : board sym -> (list-of (list-of num))
  ;;  Returns pieces available to move onto the board. The pieces
  ;;  are grouped where moving one piece is disallowed or
  ;;  not sensible until another piece (earlier in the same set)
  ;;  has been moved onto the board.
  (define (available-off-board board c)
    (let ([counts (make-vector BOARD-SIZE 0)])
      (fold-board (lambda (i j v)
                    (for-each (lambda (p)
                                (when (eq? c (piece-color p))
                                  (vector-set! counts (piece-size p)
                                               (add1 (vector-ref counts (piece-size p))))))
                              (board-ref board i j)))
                  (void))
      (reverse
       (if JR?
           ;; Can move any piece onto board
           (let loop ([counts (vector->list counts)][sizes SIZES])
             (cond
               [(null? counts) null]
               [((car counts) . < . PIECE-COUNT)
                (cons (vector->list (make-vector (- PIECE-COUNT (car counts))
                                                 (car sizes)))
                      (loop (cdr counts) (cdr sizes)))]
               [else (loop (cdr counts) (cdr sizes))]))
           ;; Can only move pieces that are not covered by others:
           (let-values ([(l cnt)
                         (let loop ([counts (vector->list counts)][sizes SIZES])
                           (cond
                             [(null? counts) (values null 0)]
                             [else (let-values ([(l cnt) (loop (cdr counts) (cdr sizes))])
                                     (let ([gone (+ cnt (car counts))])
                                       (if (gone . < . PIECE-COUNT)
                                           (values (append (vector->list
                                                            (make-vector
                                                             (- PIECE-COUNT gone)
                                                             (let sloop ([sz (car sizes)])
                                                               (if (negative? sz)
                                                                   null
                                                                   (cons sz
                                                                         (sloop (sub1 sz)))))))
                                                           l)
                                                   (+ cnt (- PIECE-COUNT gone)))
                                           (values l cnt))))]))])
             l)))))
  
  ;; Canonicalization of boards ------------------------------
  
  ;; Xforms for finding canonical forms. Seven transforms
  ;;  (including the identity) are equivalent. We generate
  ;;  them all and hash when a new board is encountered.
  (define xforms
    (if (= BOARD-SIZE 3)
        '(#(0 1 2 3 4 5 6 7 8)
          #(0 3 6 1 4 7 2 5 8)
          #(2 5 8 1 4 7 0 3 6)
          #(8 5 2 7 4 1 6 3 0)
          #(6 3 0 7 4 1 8 5 2)
          #(2 1 0 5 4 3 8 7 6)
          #(8 7 6 5 4 3 2 1 0)
          #(6 7 8 3 4 5 0 1 2))
        '(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          #(0 4 8 12 1 5 9 13 2 6 10 14 3 7 11 15)
          #(12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3)
          #(3 2 1 0 7 6 5 4 11 10 9 8 15 14 13 12)
          #(15 11 7 3 14 10 6 2 13 9 5 1 12 8 4 0)
          #(12 8 4 0 13 9 5 1 14 10 6 2 15 11 7 3)
          #(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
          #(3 7 11 15 2 6 10 14 1 5 9 13 0 4 8 12))))
  
  ;; Procedure form of the above xforms, effectively
  ;;  unrolloing a loop over the vector.
  (define xform-procs
    (if (= BOARD-SIZE 3)
        (list 
         (lambda (v) v)
         (lambda (v) (bytes (bytes-ref v 0) (bytes-ref v 3) (bytes-ref v 6)
                            (bytes-ref v 1) (bytes-ref v 4)
                            (bytes-ref v 7) (bytes-ref v 2) (bytes-ref v 5) (bytes-ref v 8)))
         (lambda (v) (bytes (bytes-ref v 2) (bytes-ref v 5) (bytes-ref v 8)
                            (bytes-ref v 1) (bytes-ref v 4)
                            (bytes-ref v 7) (bytes-ref v 0) (bytes-ref v 3) (bytes-ref v 6)))
         (lambda (v) (bytes (bytes-ref v 8) (bytes-ref v 5) (bytes-ref v 2)
                            (bytes-ref v 7) (bytes-ref v 4)
                            (bytes-ref v 1) (bytes-ref v 6) (bytes-ref v 3) (bytes-ref v 0)))
         (lambda (v) (bytes (bytes-ref v 6) (bytes-ref v 3) (bytes-ref v 0)
                            (bytes-ref v 7) (bytes-ref v 4)
                            (bytes-ref v 1) (bytes-ref v 8) (bytes-ref v 5) (bytes-ref v 2)))
         (lambda (v) (bytes (bytes-ref v 2) (bytes-ref v 1) (bytes-ref v 0)
                            (bytes-ref v 5) (bytes-ref v 4)
                            (bytes-ref v 3) (bytes-ref v 8) (bytes-ref v 7) (bytes-ref v 6)))
         (lambda (v) (bytes (bytes-ref v 8) (bytes-ref v 7) (bytes-ref v 6)
                            (bytes-ref v 5) (bytes-ref v 4)
                            (bytes-ref v 3) (bytes-ref v 2) (bytes-ref v 1) (bytes-ref v 0)))
         (lambda (v) (bytes (bytes-ref v 6) (bytes-ref v 7) (bytes-ref v 8)
                            (bytes-ref v 3) (bytes-ref v 4)
                            (bytes-ref v 5) (bytes-ref v 0) (bytes-ref v 1) (bytes-ref v 2))))
        (list
         (lambda (v) v)
         (lambda (v) (bytes (bytes-ref v 0) (bytes-ref v 4) (bytes-ref v 8)
                            (bytes-ref v 12) (bytes-ref v 1)
                            (bytes-ref v 5) (bytes-ref v 9) (bytes-ref v 13)
                            (bytes-ref v 2) (bytes-ref v 6)
                            (bytes-ref v 10) (bytes-ref v 14)
                            (bytes-ref v 3) (bytes-ref v 7) (bytes-ref v 11)
                            (bytes-ref v 15)))
         (lambda (v) (bytes (bytes-ref v 12) (bytes-ref v 13) (bytes-ref v 14)
                            (bytes-ref v 15) (bytes-ref v 8) 
                            (bytes-ref v 9) (bytes-ref v 10) (bytes-ref v 11)
                            (bytes-ref v 4) (bytes-ref v 5)
                            (bytes-ref v 6) (bytes-ref v 7)
                            (bytes-ref v 0) (bytes-ref v 1) (bytes-ref v 2)
                            (bytes-ref v 3)))
         (lambda (v) (bytes (bytes-ref v 3) (bytes-ref v 2) (bytes-ref v 1)
                            (bytes-ref v 0) (bytes-ref v 7) 
                            (bytes-ref v 6) (bytes-ref v 5) (bytes-ref v 4)
                            (bytes-ref v 11) (bytes-ref v 10)
                            (bytes-ref v 9) (bytes-ref v 8)
                            (bytes-ref v 15) (bytes-ref v 14) (bytes-ref v 13)
                            (bytes-ref v 12)))
         (lambda (v) (bytes (bytes-ref v 15) (bytes-ref v 11) (bytes-ref v 7)
                            (bytes-ref v 3) (bytes-ref v 14)
                            (bytes-ref v 10) (bytes-ref v 6) (bytes-ref v 2)
                            (bytes-ref v 13) (bytes-ref v 9)
                            (bytes-ref v 5) (bytes-ref v 1)
                            (bytes-ref v 12) (bytes-ref v 8) (bytes-ref v 4)
                            (bytes-ref v 0)))
         (lambda (v) (bytes (bytes-ref v 12) (bytes-ref v 8) (bytes-ref v 4)
                            (bytes-ref v 0) (bytes-ref v 13) 
                            (bytes-ref v 9) (bytes-ref v 5) (bytes-ref v 1)
                            (bytes-ref v 14) (bytes-ref v 10)
                            (bytes-ref v 6) (bytes-ref v 2) 
                            (bytes-ref v 15) (bytes-ref v 11) (bytes-ref v 7)
                            (bytes-ref v 3)))
         (lambda (v) (bytes (bytes-ref v 15) (bytes-ref v 14) (bytes-ref v 13)
                            (bytes-ref v 12) (bytes-ref v 11)
                            (bytes-ref v 10) (bytes-ref v 9) (bytes-ref v 8)
                            (bytes-ref v 7) (bytes-ref v 6)
                            (bytes-ref v 5) (bytes-ref v 4)
                            (bytes-ref v 3) (bytes-ref v 2) (bytes-ref v 1)
                            (bytes-ref v 0)))
         (lambda (v) (bytes (bytes-ref v 3) (bytes-ref v 7) (bytes-ref v 11)
                            (bytes-ref v 15) (bytes-ref v 2)
                            (bytes-ref v 6) (bytes-ref v 10) (bytes-ref v 14)
                            (bytes-ref v 1) (bytes-ref v 5)
                            (bytes-ref v 9) (bytes-ref v 13)
                            (bytes-ref v 0) (bytes-ref v 4) (bytes-ref v 8)
                            (bytes-ref v 12))))))
  
  ;; Generates the compact representation of a board, which is
  ;; good for hashing, but bad for applying moves
  (define flatten-board
    (if (= BOARD-SIZE 3)
        (lambda (board stack-ids)
          (bytes (hash-ref stack-ids (board-ref board 0 0))
                 (hash-ref stack-ids (board-ref board 1 0))
                 (hash-ref stack-ids (board-ref board 2 0))
                 (hash-ref stack-ids (board-ref board 0 1))
                 (hash-ref stack-ids (board-ref board 1 1))
                 (hash-ref stack-ids (board-ref board 2 1))
                 (hash-ref stack-ids (board-ref board 0 2))
                 (hash-ref stack-ids (board-ref board 1 2))
                 (hash-ref stack-ids (board-ref board 2 2))))
        (lambda (board stack-ids)
          (bytes (hash-ref stack-ids (board-ref board 0 0))
                 (hash-ref stack-ids (board-ref board 1 0))
                 (hash-ref stack-ids (board-ref board 2 0))
                 (hash-ref stack-ids (board-ref board 3 0))
                 (hash-ref stack-ids (board-ref board 0 1))
                 (hash-ref stack-ids (board-ref board 1 1))
                 (hash-ref stack-ids (board-ref board 2 1))
                 (hash-ref stack-ids (board-ref board 3 1))
                 (hash-ref stack-ids (board-ref board 0 2))
                 (hash-ref stack-ids (board-ref board 1 2))
                 (hash-ref stack-ids (board-ref board 2 2))
                 (hash-ref stack-ids (board-ref board 3 2))
                 (hash-ref stack-ids (board-ref board 0 3))
                 (hash-ref stack-ids (board-ref board 1 3))
                 (hash-ref stack-ids (board-ref board 2 3))
                 (hash-ref stack-ids (board-ref board 3 3))))))
  
  
  ;; Generate a numerical ID for each stack. This numerical
  ;;  ID must stay constant for all of time, because we
  ;;  record boards in compact form using these numbers.
  ;;  (For example, see "plays-3x3.rkt".)
  (define red-stack-ids (make-hasheq))
  (define yellow-stack-ids (make-hasheq))
  (for-each (lambda (s)
              (hash-set! red-stack-ids s (hash-count red-stack-ids)))
            all-stacks)
  (for-each (lambda (s)
              (let ([inverse 
                     (let loop ([s s])
                       (if (null? s)
                           null
                           (hash-ref (piece-gobble-table
                                      (if (eq? (piece-color (car s)) 'red)
                                          (list-ref yellow-pieces (piece-size (car s)))
                                          (list-ref red-pieces (piece-size (car s)))))
                                     (loop (cdr s)))))])
                (hash-set! yellow-stack-ids s (hash-ref red-stack-ids inverse))))
            all-stacks)
  
  ;; Applies an appropriate flattener
  (define (compact-board board who)
    (flatten-board board
                   (if (eq? who 'red) red-stack-ids yellow-stack-ids)))
  
  ;; make-canonicalize : -> (union (board sym -> (cons compact xform))
  ;;                               (compact #f -> (cons compact xform)))
  ;;  The resulting procedure embeds a table for mapping a compact
  ;;  board to its canonical compact board. The result includes an
  ;;  xform for getting from the given board's locations to
  ;;  locations in the canonical board.
  (define (make-canonicalize)
    (let ([memory (make-hash)])
      ;; Convert the board into a byte string, normalizing player:
      (lambda (board who)
        (let ([v (if who
                     (compact-board board who)
                     board)])
          ;; Find canonical mapping.
          (hash-ref 
           memory v
           (lambda ()
             (let* ([pr (cons v (car xforms))])
               (hash-set! memory v pr)
               ;; Add each equivalent to table:
               (for-each (lambda (xform xform-proc)
                           (hash-set! memory (xform-proc v) (cons v xform)))
                         (cdr xforms) (cdr xform-procs))
               pr)))))))
  
  ;; apply-xform : xform num num -> num
  ;;  Returns a position in a canonical board
  (define (apply-xform xform i j)
    (vector-ref xform (+ (* j BOARD-SIZE) i)))
  ;; unapply-xform : xform num -> (values num num)
  ;;  Maps a canonical-board position to a position in
  ;;  a specific board.
  (define (unapply-xform xform v)
    (let loop ([i 0])
      (if (= (vector-ref xform i) v)
          (values (modulo i BOARD-SIZE) (quotient i BOARD-SIZE))
          (loop (add1 i)))))
  
  ;; Printing boards ------------------------------
  
  ;; helper
  (define (board->string depth b)
    (let jloop ([j 0])
      (if (= j BOARD-SIZE)
          ""
          (string-append
           (make-string depth #\space)
           (let iloop ([i 0])
             (if (= i BOARD-SIZE)
                 ""
                 (string-append (stack->string (board-ref b i j))
                                " "
                                (iloop (add1 i)))))
           "\n"
           (jloop (add1 j))))))
  
  (define (stack->string s)
    (let ([s (apply string-append 
                    "...."
                    (map (lambda (p)
                           (list-ref (if (eq? 'red (piece-color p))
                                         '("_" "i" "I" "|")
                                         '("=" "o" "O" "0"))
                                     (piece-size p)))
                         s))])
      (substring s (- (string-length s) BOARD-SIZE)))))
