#lang racket
;; Some tests for the model

(require "sig.rkt"
         "model.rkt"
         "test.rkt")

;; Test basic procs:
(define (test-folding n)
  
  (define BOARD-SIZE n)
  (define-values/invoke-unit model-unit@ (import config^) (export model^))
  (test 'red (other 'yellow))
  (test 'yellow (other 'red))
  (test (if (= n 3)
            '(0 1 2)
            '(0 1 2 3))
        (sort (fold-rowcol (lambda (i v) (cons i v)) null) <))
  (test (if (= n 3)
            '((0 . 0) (0 . 1) (0 . 2)
                      (1 . 0) (1 . 1) (1 . 2)
                      (2 . 0) (2 . 1) (2 . 2))
            '((0 . 0) (0 . 1) (0 . 2) (0 . 3)
                      (1 . 0) (1 . 1) (1 . 2) (1 . 3)
                      (2 . 0) (2 . 1) (2 . 2) (2 . 3)
                      (3 . 0) (3 . 1) (3 . 2) (3 . 3)))
        (sort (fold-board (lambda (i j v) (cons (cons i j) v)) null)
              (lambda (a b)
                (if (= (car a) (car b))
                    (< (cdr a) (cdr b))
                    (< (car a) (car b)))))))
(test-folding 3)
(test-folding 4)

;; Test available-off-board for 3x3:
(let ()
  (define BOARD-SIZE 3)
  (define-values/invoke-unit model-unit@ (import config^) (export model^))
  (test '((2 2) (1 1) (0 0)) (available-off-board empty-board 'red))
  (test '((2 2) (1 1) (0)) (available-off-board 
                            (move empty-board (list-ref red-pieces 0) #f #f 1 1 values void)
                            'red))
  (let ([b2 (move empty-board (list-ref red-pieces 2) #f #f 1 1 values void)])
    (test '((2) (1 1) (0 0)) (available-off-board b2 'red))
    (let ([b3 (move b2 (list-ref yellow-pieces 1) #f #f 2 2 values void)])
      (test '((2) (1 1) (0 0)) (available-off-board b3 'red))
      (test '((2 2) (1) (0 0)) (available-off-board b3 'yellow))
      (let ([b4 (move b3 (list-ref red-pieces 2) #f #f 0 1 values void)])
        (test '((1 1) (0 0)) (available-off-board b4 'red))
        (test '((2 2) (1) (0 0)) (available-off-board b4 'yellow))))))

;; Test available-off-board for 4x4:
(let ()
  
  (define BOARD-SIZE 4)
  (define-values/invoke-unit model-unit@ (import config^) (export model^))
  (test '((3 2 1 0) (3 2 1 0) (3 2 1 0)) (available-off-board empty-board 'red))
  (let ([b2 (move empty-board (list-ref red-pieces 3) #f #f 1 1 values void)])
    (test '((3 2 1 0) (3 2 1 0) (2 1 0)) (available-off-board b2 'red))
    (let ([b3 (move b2 (list-ref yellow-pieces 3) #f #f 2 2 values void)])
      (test '((3 2 1 0) (3 2 1 0) (2 1 0)) (available-off-board b3 'red))
      (test '((3 2 1 0) (3 2 1 0) (2 1 0)) (available-off-board b3 'yellow))
      (let ([b4 (move b3 (list-ref red-pieces 3) #f #f 0 1 values void)])
        (test '((3 2 1 0) (2 1 0) (2 1 0)) (available-off-board b4 'red))
        (let ([b5 (move b4 (list-ref red-pieces 2) #f #f 0 3 values void)])
          (test '((3 2 1 0) (2 1 0) (1 0)) (available-off-board b5 'red)))
        (let ([b5 (move b4 (list-ref red-pieces 3) #f #f 0 3 values void)])
          (test '((2 1 0) (2 1 0) (2 1 0)) (available-off-board b5 'red))))
      (let ([b4 (move b3 (list-ref red-pieces 2) #f #f 0 1 values void)])
        (test '((3 2 1 0) (3 2 1 0) (1 0)) (available-off-board b4 'red))
        (let ([b5 (move b4 (list-ref red-pieces 3) #f #f 0 3 values void)])
          (test '((3 2 1 0) (2 1 0) (1 0)) (available-off-board b5 'red))
          (let ([b6 (move b5 (list-ref red-pieces 2) #f #f 3 3 values void)])
            (test '((3 2 1 0) (1 0) (1 0)) (available-off-board b6 'red))))))))

(define x-table (make-hash))
(define (testx id board)
  (test id (hash-ref x-table board
                     (lambda ()
                       (when (hash-ref x-table id (lambda () #f))
                         (error 'testx "id already mapped\n"))
                       (hash-set! x-table id #t)
                       (hash-set! x-table board id)
                       id))))

;; Given a canonicalize function, a board, the current player,
;;  and the model exports, check that the canonicalizer works
;;  on the board.
(define (canon-test BOARD-SIZE canonicalize board who 
                    fold-board board-ref move empty-board
                    yellow-pieces red-pieces piece-color piece-size other
                    apply-xform unapply-xform)
  (define (flip-stack stack)
    (map (lambda (p)
           (if (eq? (piece-color p) 'red)
               (list-ref yellow-pieces (piece-size p))
               (list-ref red-pieces (piece-size p))))
         stack))
  (define (board-xform board ijx flip-stack)
    (fold-board
     (lambda (i j b)
       (let ([stack (board-ref board i j)])
         (let loop ([stack (flip-stack stack)])
           (if (null? stack)
               b
               (let-values ([(i j) (ijx i j)])
                 (move (loop (cdr stack))
                       (car stack)
                       #f #f
                       i j
                       values void))))))
     empty-board))
  (let* ([key+xform (canonicalize board who)]
         ;; flip vert
         [board2 (board-xform board (lambda (i j)
                                      (values i (- BOARD-SIZE 1 j)))
                              values)]
         [key2+xform2 (canonicalize board2 who)]
         ;; flip horiz
         [board3 (board-xform board (lambda (i j)
                                      (values (- BOARD-SIZE 1 i) j))
                              values)]
         [key3+xform3 (canonicalize board3 who)]
         ;; flip colors
         [board4 (board-xform board (lambda (i j) (values i j))
                              flip-stack)]
         [key4+xform4 (canonicalize board4 (other who))])
    ;; Canoncal key should be the same for all boards:
    (test (car key+xform) (car key2+xform2))
    (test (car key+xform) (car key3+xform3))
    (test (car key+xform) (car key4+xform4))
    ;; Xforming coordinates should produce the same thing for each board:
    (fold-board (lambda (i j v)
                  (let ([pos (apply-xform (cdr key+xform) i j)]
                        [s (board-ref board i j)])
                    (let-values ([(i j) (unapply-xform (cdr key2+xform2) pos)])
                      (test s (board-ref board2 i j))
                      (test pos (apply-xform (cdr key2+xform2) i j)))
                    (let-values ([(i j) (unapply-xform (cdr key3+xform3) pos)])
                      (test s (board-ref board3 i j))
                      (test pos (apply-xform (cdr key3+xform3) i j)))
                    (let-values ([(i j) (unapply-xform (cdr key4+xform4) pos)])
                      (test (flip-stack s) (board-ref board4 i j))
                      (test pos (apply-xform (cdr key4+xform4) i j)))))
                (void))
    (car key+xform)))

;; Test canonicalization, 3x3
(let ()  
  (define BOARD-SIZE 3)
  (define-values/invoke-unit model-unit@ (import config^) (export model^))
  (let ([c (let ([canonicalize (make-canonicalize)])
             (lambda (b who)
               (canon-test 3 canonicalize b who 
                           fold-board board-ref move empty-board
                           yellow-pieces red-pieces piece-color piece-size other
                           apply-xform unapply-xform)))])
    (testx 0 (c empty-board 'red))
    (testx 0 (c empty-board 'yellow))
    (let ([b1 (move empty-board (list-ref red-pieces 2) #f #f 1 1 values void)])
      (testx 1 (c b1 'red))
      (testx 2 (c b1 'yellow))
      (testx 1 (c b1 'red))
      (let ([b2 (move b1 (list-ref red-pieces 2) #f #f 2 2 values void)])
        (testx 3 (c b2 'red))
        (testx 7 (c b2 'yellow)))
      (let ([b2 (move b1 (list-ref red-pieces 2) #f #f 0 0 values void)])
        (testx 3 (c b2 'red))
        (testx 7 (c b2 'yellow))
        (let ([b3 (move b2 (list-ref yellow-pieces 1) #f #f 1 0 values void)])
          (testx 11 (c b3 'red))
          (testx 19 (c b3 'yellow))
          (let ([b4 (move b3 (list-ref red-pieces 2) #f #f 1 0 values void)])
            (testx 27 (c b4 'red))
            (testx 35 (c b4 'yellow))
            (let ([b5 (move b4 (list-ref red-pieces 2) 0 0 2 0 values void)])
              (testx 27 (c b5 'red))
              (testx 35 (c b5 'yellow)))))))))

(set! x-table (make-hash))

;; Test canonicalization, 4x4

(define BOARD-SIZE 4)
(define-values/invoke-unit model-unit@ (import config^) (export model^))
(let ([c (let ([canonicalize (make-canonicalize)])
           (lambda (b who)
             (canon-test 4 canonicalize b who 
                         fold-board board-ref move empty-board
                         yellow-pieces red-pieces piece-color piece-size other
                         apply-xform unapply-xform)))])
  (testx 0 (c empty-board 'red))
  (testx 0 (c empty-board 'yellow))
  (let ([b1 (move empty-board (list-ref red-pieces 0) #f #f 1 1 values void)])
    (testx 1 (c b1 'red))
    (testx 5 (c b1 'yellow))
    (testx 1 (c b1 'red))
    (let ([b1.1 (move b1 (list-ref red-pieces 0) #f #f 2 2 values void)])
      (let ([b2 (move b1.1 (list-ref red-pieces 2) #f #f 3 3 values void)])
        (testx 9 (c b2 'red))
        (testx 13 (c b2 'yellow)))
      (let ([b2 (move b1.1 (list-ref red-pieces 2) #f #f 0 0 values void)])
        (testx 9 (c b2 'red))
        (testx 13 (c b2 'yellow))
        (let ([b3 (move b2 (list-ref yellow-pieces 1) #f #f 1 0 values void)])
          (testx 17 (c b3 'red))
          (testx 25 (c b3 'yellow)))))))

(define (basic-tests size xform 4x4-finish-pos)
  ;; When xform is the identity, then we build toward
  ;;  _ _ Y -    _ = empty
  ;;  _ Y _ -    - = optional (3x3 vs 4x4)
  ;;  y R R -
  ;;  - - - -
  ;; The xform changes the cooridnate system so that we
  ;; test rows and columns in addition to this diagonal.
  
  (define BOARD-SIZE size)
  
  (define-values (i00 j00) (xform 0 0))
  (define-values (i11 j11) (xform 1 1))
  (define-values (i22 j22) (xform 2 2))
  (define-values (i12 j12) (xform 1 2))
  (define-values (i02 j02) (xform 0 2))
  (define-values (i20 j20) (xform 2 0))
  
  (define-values/invoke-unit model-unit@ (import config^) (export model^))
  
  ;; Empty board --------------------
  (define b empty-board)
  
  (test null (board-ref b i00 j00))
  (test null (board-ref b i22 j22))
  
  (test #f (winner? b 'red))
  (test #f (winner? b 'yellow))
  
  (define big-red (list-ref red-pieces 2))
  (define big-yellow (list-ref yellow-pieces 2))
  (define med-red (list-ref red-pieces 1))
  (define med-yellow (list-ref yellow-pieces 1))
  (define small-yellow (list-ref yellow-pieces 0))
  
  ;; Big red --------------------
  
  (define b1 (move b big-red #f #f i00 j00 values void))
  (test (list big-red) (board-ref b1 i00 j00))
  (test (void) (move b1 big-yellow #f #f i00 j00 values void))
  
  (test #f (winner? b1 'red))
  (test #f (winner? b1 'yellow))
  
  ;; Big red, big yellow --------------------
  
  (define b2 (move b1 big-yellow #f #f i11 j11 values void))
  (test (list big-red) (board-ref b2 i00 j00))
  (test (list big-yellow) (board-ref b2 i11 j11))
  
  (test #f (winner? b2 'red))
  (test #f (winner? b2 'yellow))
  
  (test (void) (move b2 big-red #f #f i11 j11 values void))
  (test (void) (move b2 big-red i00 j00 i11 j11 values void))
  
  ;; Big red moved, big yellow --------------------
  
  (define b3 (move b2 big-red i00 j00 i22 j22 values void))
  (test null (board-ref b3 i00 j00))
  (test (list big-yellow) (board-ref b3 i11 j11))
  (test (list big-red) (board-ref b3 i22 j22))
  
  (test #f (winner? b3 'red))
  (test #f (winner? b3 'yellow))
  
  ;; Big red, big yellow, med yellow --------------------
  
  (define b4 (move b3 med-yellow #f #f i02 j02 values void))
  (test (list big-yellow) (board-ref b4 i11 j11))
  (test (list big-red) (board-ref b4 i22 j22))
  (test (list med-yellow) (board-ref b4 i02 j02))
  
  (test #f (winner? b4 'red))
  (test #f (winner? b4 'yellow))
  
  (test (void) (move b4 med-red #f #f i02 j02 values void))
  
  ;; Big red gobble med yellow, big yellow --------------------
  ;;  --- Add big red
  (define b5.1 (move b4 big-red #f #f i02 j02 values void))
  (when (= size 4)
    ;; can't gobble yellow, since it's not in a 3-in-arow
    (test (void) b5.1) 
    ;; Generate board by cheating, giving red two turns...
    (set! b5.1 (move (move b4 big-red i22 j22 i02 j02 values void)
                     big-red #f #f i22 j22 values void)))
  (test (list big-yellow) (board-ref b5.1 i11 j11))
  (test (list big-red) (board-ref b5.1 i22 j22))
  (test (list big-red med-yellow) (board-ref b5.1 i02 j02))
  
  ;;  --- Move big red
  (define b5.2 (move b4 big-red i22 j22 i02 j02 values void))
  (test (list big-yellow) (board-ref b5.2 i11 j11))
  (test null (board-ref b5.2 i22 j22))
  (test (list big-red med-yellow) (board-ref b5.2 i02 j02))
  
  ;; Add small yellow ------------------------------
  ;;  --- with 2 big red
  (define b6.1 (move b5.1 small-yellow #f #f i20 j20 values void))
  (test (list big-yellow) (board-ref b6.1 i11 j11))
  (test (list big-red) (board-ref b6.1 i22 j22))
  (test (list big-red med-yellow) (board-ref b6.1 i02 j02))
  (test (list small-yellow) (board-ref b6.1 i20 j20))
  
  (test #f (winner? b6.1 'red))
  (test #f (winner? b6.1 'yellow))
  
  ;;  --- with 1 big red
  (define b6.2 (move b5.2 small-yellow #f #f i20 j20 values void))
  (test (list big-yellow) (board-ref b6.2 i11 j11))
  (test null (board-ref b6.2 i22 j22))
  (test (list big-red med-yellow) (board-ref b6.2 i02 j02))
  (test (list small-yellow) (board-ref b6.2 i20 j20))
  
  (test #f (winner? b6.2 'red))
  (test #f (winner? b6.2 'yellow))
  
  ;; Expose med yellow for 3-in-row ----------
  (define b7.1 (move b6.1 big-red i02 j02 i12 j12 values void))
  (test (list big-yellow) (board-ref b7.1 i11 j11))
  (test (list big-red) (board-ref b7.1 i22 j22))
  (test (list med-yellow) (board-ref b7.1 i02 j02))
  (test (list small-yellow) (board-ref b7.1 i20 j20))
  (test (list big-red) (board-ref b7.1 i12 j12))
  
  (test #f (winner? b7.1 'red))
  (test (= size 3) (winner? b7.1 'yellow))
  
  (define b7.2 (move b6.2 big-red i02 j02 i12 j12 values void))
  (test (list big-yellow) (board-ref b7.2 i11 j11))
  (test null (board-ref b7.2 i22 j22))
  (test (list med-yellow) (board-ref b7.2 i02 j02))
  (test (list small-yellow) (board-ref b7.2 i20 j20))
  (test (list big-red) (board-ref b7.2 i12 j12))
  
  (test #f (winner? b7.2 'red))
  (test (= size 3) (winner? b7.2 'yellow))
  
  (when (and (= size 4)
             4x4-finish-pos)
    ;; 4 x 4 game: now red can cover small yellow, because it's
    ;; part of 3 in a row
    
    (test #t (3-in-a-row? b7.2 i20 j20 'yellow))
    (test #f (3-in-a-row? b7.2 i20 j20 'red))
    
    (define b8.2 (move b7.2 med-red #f #f i20 j20 values void))
    (test (list big-yellow) (board-ref b8.2 i11 j11))
    (test null (board-ref b8.2 i22 j22))
    (test (list med-yellow) (board-ref b8.2 i02 j02))
    (test (list med-red small-yellow) (board-ref b8.2 i20 j20))
    (test (list big-red) (board-ref b8.2 i12 j12))
    
    (test #f (winner? b8.2 'red))
    (test #f (winner? b8.2 'yellow))
    
    (define b8.2x (move b7.2 med-yellow #f #f (car 4x4-finish-pos) (cdr 4x4-finish-pos) values void))
    (test #f (winner? b8.2x 'red))
    (test #t (winner? b8.2x 'yellow))))

(define (rotate i j)
  (case i
    [(0) (case j
           [(0) (values 1 0)]
           [(1) (values 0 0)]
           [(2) (values 0 1)])]
    [(1) (case j
           [(0) (values 2 0)]
           [(1) (values 1 1)]
           [(2) (values 0 2)])]
    [(2) (case j
           [(0) (values 2 1)]
           [(1) (values 2 2)]
           [(2) (values 1 2)])]
    [else (values i j)]))

(map (lambda (xform+?)
       (basic-tests 3 ((cdr xform+?) 3) (car xform+?))
       (basic-tests 4 ((cdr xform+?) 4) (car xform+?)))
     (list (cons #f (lambda (sz) (lambda (i j) (values i j))))
           (cons #f (lambda (sz) (lambda (i j) (values j i))))
           (cons #f (lambda (sz) (lambda (i j) (values i (- sz 1 j)))))
           (cons '(3 . 1) (lambda (sz) (lambda (i j) (rotate i j))))
           (cons '(1 . 3) (lambda (sz) (lambda (i j) (rotate i (- 3 1 j)))))))

;; Extra tests for 4 x 4 to get yellow 3-in-a-row on diagonals:
(basic-tests 4 (lambda (i j) (values i (+ j 1))) '(3 . 0))
(basic-tests 4 (lambda (i j) (values i (- 3 (+ j 1)))) '(3 . 3))

(report-test-results)


