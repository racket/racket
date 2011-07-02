(module board mzscheme
  (require "board-size.rkt") ; for n
  
  (provide x o none     ; cell values
	   new-board    ; n => board
                        ;  where n is the board size
                        ;  the board is empty
	   board-cell   ; board col row => cell-value
	                ;   cols/rows are numbered from 0
	   push         ; board side index value => board
	                ;   where side is one of
	                ;        'top 'bottom 'left 'right
	                ;   index is a number in [0, n-1]
                        ;   values is x or o
	   rotate-cw    ; board turns => board
	                ;   rotates 90 degrees x turns
	                ;   rotation affects board-cell and
                        ;    push, but not find-board-in-history

	   new-history             ; => history
                                   ;  the history is empty
	   find-board-in-history   ; board history => board or #f
	   extend-history          ; board history => history
	   extend-history!         ; board history => history
                                   ;   maybe mutates the input history
           )
  
  (define x #\x)
  (define o #\o)
  (define none #\space)
  
  (define-struct board (str n rotation))
  
  (define (new-board n)
    (make-board (make-string (add1 (* n n)) #\space) n 0))
  
  (define (dup b)
    (make-board (string-copy (board-str b)) (board-n b) (board-rotation b)))
  
  (define (unrotate-indices board row col)
    (let ([n (current-board-size)])
      (case (board-rotation board)
        [(0) (values row col)]
        [(1) (values (- (sub1 n) col) row)]
        [(2) (values (- (sub1 n) row) (- (sub1 n) col))]
        [(3) (values col (- (sub1 n) row))])))
    
  (define (board-cell board col row)
    (let-values ([(row col) (unrotate-indices board row col)]
                 [(n) (current-board-size)])
      (string-ref (board-str board) (+ col (* row n)))))
  
  (define (set-cell! board col row v)
    (let-values ([(row col) (unrotate-indices board row col)]
                 [(n) (current-board-size)])
      (string-set! (board-str board) (+ col (* row n)) v)))
  
  (define (xpush board c r inc-c inc-r piece)
    (let ([board (dup board)]
          [n (current-board-size)])
      (let loop ([c c][r r][old piece])
        (when (and (< -1 c n) (< -1 r n))
          (let ([v (board-cell board c r)])
            (set-cell! board c r old)
            (unless (eq? v none)
              (loop (inc-c c) (inc-r r) v)))))
      (string-set! (board-str board) (* n n) piece) ; last move indicator
      board))
  
  (define identity (lambda (x) x))
  
  (define push
    (lambda (board dir i piece)
      (let ([n (current-board-size)])
        (case dir
          [(left) (xpush board 0 i add1 identity piece)]
          [(right) (xpush board (sub1 n) i sub1 identity piece)]
          [(top) (xpush board i 0 identity add1 piece)]
          [(bottom) (xpush board i (sub1 n) identity sub1 piece)]
          [else (error 'push "bad directrion ~a" dir)]))))
  
  (define (rotate-cw board amt)
    (let* ([b (dup board)]
           [r (modulo (+ (board-rotation board) amt) 4)]
           [r2 (if (negative? r)
                   (+ r 4)
                   r)])
      (set-board-rotation! b r2)
      b))
  
  
  ;; In board.c, history is implemented with hash tables and fast
  ;; compying. Here we just use an assoc list.
  
  (define (new-history) 
    null)
  
  (define (find-board-in-history board h) 
    (let ([v (assoc (string->symbol (board-str board)) h)])
      (and v (cdr v))))
  
  
  (define (extend-history board h)
    (cons (cons (string->symbol (board-str board)) board) h))
  
  (define extend-history! extend-history))
