
(module board racket
  
  ;; color = (symbols 'blue 'green 'red 'yellow)
  ;; color : color
  ;; id : (union 0 1 2 3)
  (define-struct pawn (color id index) #:inspector (make-inspector))
  
  ;; v : (vectorof loc) length is always 16
  (define-struct board (v) #:inspector (make-inspector))
  
  ;; loc = (union 'start 'home number[main-loc] home-row-loc)
  (define-struct home-row-loc (num color) #:inspector (make-inspector))
  
  (define color (symbols 'red 'green 'blue 'yellow))
  
  (provide/contract 
   (get-enter-pos (color . -> . number?))
   (get-exit-pos (color . -> . number?))
   (pawn-id (pawn? . -> . (integer-in 0 4)))
   (pawn-color (pawn? . -> . color)))
  
  (provide (rename-out [build-pawn make-pawn])
           pawn?
           new-board
           for-each-pawn/loc
           
           (rename-out [make-old-style-board make-board])
           
           board-start
           
           board-main-i
           board-main-size
           
           board-home-row-i
           board-home-row-size
           
           board-home
           
           move-piece
           move-piece2
           
           safety?
           
           find-blockades/color
           make-blockade
           blockade-loc
           blockade-p1
           blockade-p2
           find-blockade/between
           
           make-home-row-loc
           home-row-loc-num
           home-row-loc-color
           home-row-loc?)
  
  ;; inline with a macro?
  (define (for-each-pawn/loc/proc board f)
    (let ([v (board-v board)])
      (let loop ([i 16])
        (unless (zero? i)
          (f (vector-ref all-pawns (- i 1)) (vector-ref v (- i 1)))
          (loop (- i 1))))))
  
  (define-syntax (for-each-pawn/loc stx)
    (syntax-case stx ()
      [(_ board (lambda (pawn loc) lam-body))
       (let loop ([i 0]
                  [lst '()])
         (cond
           [(= i 16) (with-syntax ([(bodies ...) lst])
                       (syntax
                        (let ([v (board-v board)])
                          bodies ...
                          (void))))]
           [else (loop (+ i 1)
                       (cons (with-syntax ([i i])
                               (syntax (let ([loc (vector-ref v i)]
                                             [pawn (vector-ref all-pawns i)])
                                         lam-body)))
                             lst))]))]
      [(_ board f) (syntax (for-each-pawn/loc/proc board f))]))
  
  (define (make-old-style-board start main home-rows home)
    (let* ([board (new-board)]
           [v (board-v board)])
      ;; can ignore start pawns
      
      ;; main pawns
      (let loop ([i 0])
        (cond
          [(= i (vector-length main)) '()]
          [else (for-each (lambda (pawn) (vector-set! v (pawn-index pawn) i))
                          (vector-ref main i))
                (loop (+ i 1))]))
      
      ;; home row pawns
      (for-each
       (lambda (hr)
         (let ([vec (cdr hr)])
           (let loop ([i 0])
             (cond
               [(= i (vector-length vec)) (void)]
               [else 
                (for-each (lambda (pawn) 
                            (vector-set! v 
                                         (pawn-index pawn) 
                                         (make-home-row-loc i (pawn-color pawn))))
                          (vector-ref vec i))
                     (loop (+ i 1))]))))
       home-rows)
      
      ;; home pawns
      (for-each (lambda (home-pawn) (vector-set! v (pawn-index home-pawn) 'home)) home)
      board))
  
  (define (new-board) (make-board (make-vector 16 'start)))
  (define board-home-row-size 7)
  (define board-main-size 68)
  
  ; (matching-pawns <board-exp> <pawn-id> <loc-id> <test-exp>)
  (define-syntax (matching-pawns stx)
    (syntax-case stx ()
      [(_ board pawn loc test)
       (and (identifier? (syntax pawn))
            (identifier? (syntax loc)))
       (let loop ([i 16]
                  [sofar '()])
         (cond
           [(zero? i) (with-syntax ([(body ...) sofar])
                        (syntax
                         (let ([result '()]
                               [v (board-v board)])
                           body ...
                           result)))]
           [else (loop (- i 1)
                       (cons
                        (with-syntax ([i (- i 1)])
                          (syntax
                           (let ([loc (vector-ref v i)]
                                 [pawn (vector-ref all-pawns i)])
                             (when test
                               (set! result (cons pawn result))))))
                        sofar))]))]))

  (define (board-main-i board i) (matching-pawns board pawn loc (equal? i loc)))
  (define (board-home-row-i board color i) 
    (matching-pawns board
                    pawn
                    loc
                    (and (home-row-loc? loc)
                         (= (home-row-loc-num loc) i)
                         (eq? (home-row-loc-color loc) color))))
  (define (board-start board) (matching-pawns board pawn loc (eq? loc 'start)))
  (define (board-home board) (matching-pawns board pawn loc (eq? loc 'home)))
  
  ;; move-piece : board pawn loc -> board
  (define (move-piece board pawn to)
    (let ([new-board (copy-board board)])
      (vector-set! (board-v new-board) (pawn-index pawn) to)
      new-board))
  
  ;; move-piece2 : board pawn loc pawn loc -> board
  (define (move-piece2 board pawn to pawn2 to2)
    (let ([new-board (copy-board board)])
      (vector-set! (board-v new-board) (pawn-index pawn) to)
      (vector-set! (board-v new-board) (pawn-index pawn2) to2)
      new-board))
  
  ;; copy-board : board -> board
  (define (copy-board board)
    (let ([v (board-v board)])
      (make-board (build-vector 16 (lambda (i) (vector-ref v i))))))
  
  ;; entry points for the four colors
  (define enters '((green . 5)
                   (red . 22)
                   (blue . 39)
                   (yellow . 56)))
  (define (get-enter-pos color) (cdr (assq color enters)))  
  
  ;; the points where the four colors go off into their
  ;; own sections of the board.
  (define exits '((green . 0)
                  (red . 17)
                  (blue . 34)
                  (yellow . 51)))
  (define (get-exit-pos color) (cdr (assq color exits)))
  
  (define safeties (append (map cdr enters)
                           (map cdr exits)
                           (list 12
                                 (+ 12 17)
                                 (+ 12 17 17)
                                 (+ 12 17 17 17))))
  (define (safety? i) (memq i safeties))
  
  ;; find-blockade/between : board loc loc -> (union loc #f)
  (define (find-blockade/between board start end)
    (find-blockade/cases (find-blockades board) start end))
  
  ;; find-blockades : board -> (listof loc)
  (define (find-blockades board)
    (let ([ht (make-hash)]
          [blockades '()])
      (for-each-pawn/loc
       board
       (lambda (pawn loc)
         (when (hash-ref ht
                               loc
                               (lambda ()
                                 (hash-set! ht loc #t)
                                 #f))
           (set! blockades (cons loc blockades)))))
      blockades))
  
  ;; find-blockade/cases : (listof loc) loc loc -> (union loc #f)
  (define (find-blockade/cases blockades start end)
    (cond
      [(and (number? start) (number? end))
       (if (<= start end)
           (find-blockade/between-main blockades start end)
           (or (find-blockade/between-main blockades start (- board-main-size 1))
               (find-blockade/between-main blockades 0 end)))]
      [(and (number? start) (home-row-loc? end))
       (or (find-blockade/cases blockades start (get-exit-pos (home-row-loc-color end)))
           (find-blockade/between-home-row blockades
                                           (home-row-loc-color end)
                                           0
                                           (home-row-loc-num end)))]
      [(and (home-row-loc? start) (home-row-loc? end))
       (find-blockade/between-home-row blockades
                                       (home-row-loc-color start)
                                       (home-row-loc-num start)
                                       (home-row-loc-num end))]
      [(not (loc<=? start end))
       (error 'find-blockade/between "expected locs in order, got ~e and ~e" start end)]
      [(or (eq? start 'home) (eq? end 'home))
       (error 'find-blockade/between "cannot accept 'home as argument, got ~e and ~e" start end)]
      [(or (eq? start 'start) (eq? end 'start))
       (error 'find-blockade/between "cannot accept 'start as argument, got ~e and ~e" start end)]
      [else 
       (error 'find-blockade/between "unknown arguments ~e and ~e" start end)]))
  
  (define (find-blockade/between-main blockades start end)
    (ormap (lambda (blockade) (and (number? blockade)
                                   (<= start blockade end)
                                   blockade))
           blockades))
  
  (define (find-blockade/between-home-row blockades color start end)
    (ormap (lambda (blockade) (and (home-row-loc? blockade)
                                   (eq? color (home-row-loc-color blockade))
                                   (<= start (home-row-loc-num blockade) end)
                                   blockade))
           blockades))
       
  ;; loc : loc
  ;; p1 : pawn
  ;; p2 : pawn
  ;; (pawn<=? p1 p2) is true
  (define-struct blockade (loc p1 p2) #:inspector (make-inspector))

  ;; find-blockades/color : board color -> (listof blockade)
  (define (find-blockades/color board color)
    (let ([ht (make-hash)]
          [v (board-v board)]
          [offset (find-pawn-index color 0)])
      (let loop ([i 0]
                 [blockades null])
        (cond
          [(= i 4) blockades]
          [else
           (let ([loc (vector-ref v (+ offset i))])
             (cond
               [(eq? loc 'start) (loop (+ i 1) blockades)]
               [(eq? loc 'home) (loop (+ i 1) blockades)]
               [(hash-ref ht loc (lambda ()
                                         (hash-set! ht loc i)
                                         #f))
                =>
                (lambda (old-i)
                  (loop (+ i 1)
                        (cons (make-blockade loc 
                                             (vector-ref all-pawns (+ offset old-i))
                                             (vector-ref all-pawns (+ offset i)))
                              blockades)))]
               [else (loop (+ i 1) blockades)]))]))))
  
  (define (loc<=? l1 l2) (<= (loc->id l1) (loc->id l2)))
  
  (define (loc->id loc)
    (cond
      [(eq? loc 'start) 0]
      [(number? loc) (+ loc 1)]
      [(eq? loc 'home) 1000]
      [(home-row-loc? loc) (+ 100
                              (* (color->int (home-row-loc-color loc))
                                 100)
                              (home-row-loc-num loc))]
      [else (error 'loc->id "expected a loc, got ~e" loc)]))
  
  (define (build-pawn color id) (make-pawn color id (find-pawn-index color id)))
  (define (find-pawn-index color id) (+ (* (color->int color) 4) id))
  
  (define (pawn<=? p1 p2)
    (if (eq? (pawn-color p1) (pawn-color p2))
        (<= (pawn-id p1) (pawn-id p2))
        (color<= (pawn-color p1) (pawn-color p2))))
  
  (define (color<= c1 c2)
    (<= (color->int c1) (color->int c2)))
  
  (define (color->int c)
    (case c
      [(blue) 0]
      [(green) 1]
      [(red) 2]
      [(yellow) 3]
      [else (error 'color->int "unknown color ~e" c)]))
  
  
  (define all-pawns
    (vector (build-pawn 'blue 0)
            (build-pawn 'blue 1)
            (build-pawn 'blue 2)
            (build-pawn 'blue 3)
            (build-pawn 'green 0)
            (build-pawn 'green 1)
            (build-pawn 'green 2)
            (build-pawn 'green 3)
            (build-pawn 'red 0)
            (build-pawn 'red 1)
            (build-pawn 'red 2)
            (build-pawn 'red 3)
            (build-pawn 'yellow 0)
            (build-pawn 'yellow 1)
            (build-pawn 'yellow 2)
            (build-pawn 'yellow 3)))
  
  (let loop ([i 0])
    (unless (= i 16)
      (unless (= i (pawn-index (vector-ref all-pawns i)))
        (error 'mismatch "~s ~s" i (vector-ref all-pawns i)))
      (loop (+ i 1)))))
