;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on a Python version:
;;   contributed by Olof Kraigher
;;    modified by Tupteq
;;   contributed by Matthew Flatt
;;   optimized by Eli Barzilay

(require racket/cmdline)

(define-type Board Integer)
(define-type Piece (Listof (Vectorof Integer)))

(define width 5)
(define height 10)
(define size (* width height))

(: valid-xy? (Integer Integer -> Boolean))
(define (valid-xy? x y)
  (and (0 . <= . x)
       (x . < . width)
       (0 . <= . y)
       (y . < . height)))

(: mover ((Integer Integer -> (values Integer Integer)) -> (Vectorof Integer)))
(define (mover fun)
  (let: ([t : (Vectorof Integer) (make-vector size)])
    (for ([p (in-range size)])
      (vector-set! t p (let*-values ([(y x) (quotient/remainder p width)]
                                     [(x y) (fun x y)])
                         (if (valid-xy? x y) (+ x (* y width)) -1))))
    t))

(define E
  (mover (lambda: ((x : Integer) (y : Integer)) (values (add1 x) y))))
(define W
  (mover (lambda: ((x : Integer) (y : Integer)) (values (sub1 x) y))))
(define NE
  (mover (lambda: ((x : Integer) (y : Integer)) (values (+ x (bitwise-and y 1)) (sub1 y)))))
(define NW
  (mover (lambda: ((x : Integer) (y : Integer)) (values (sub1 (+ x (bitwise-and y 1))) (sub1 y)))))
(define SE
  (mover (lambda: ((x : Integer) (y : Integer)) (values (+ x (bitwise-and y 1)) (add1 y)))))
(define SW
  (mover (lambda: ((x : Integer) (y : Integer)) (values (sub1 (+ x (bitwise-and y 1))) (add1 y)))))

(define-type Direction (Vectorof Integer))
(define rotate-list (list E NE NW W SW SE E))
(: rotate (Direction -> Direction))
(define (rotate dir)
  (cadr (assert (memq dir rotate-list))))

(define flip-alist (list (cons E W) (cons NE NW) (cons NW NE)
                         (cons W E) (cons SW SE) (cons SE SW)))
(: flip (Direction -> Direction))
(define (flip dir) (cdr (assert (assq dir flip-alist))))

(define movers (list E W NE NW SE SW))

(: valid? (Integer -> Boolean))
(define (valid? p)
  (p . >= . 0))

(: clear? (Board Integer -> Boolean))
(define (clear? board pos)
  (not (bitwise-bit-set? board pos)))
(: set (Board Integer -> Integer))
(define (set board pos)
  (bitwise-ior board (arithmetic-shift 1 pos)))

(: zero-count (Board -> Integer))
(define (zero-count board)
  (for/fold ([count 0]) ([i (in-range size)])
    (if (clear? board i) (add1 count) count)))

(: find-free-cell (Board -> (Option Integer)))
(define (find-free-cell board)
  (for/or ([p (in-range 0 size)])
    (and (clear? board p) p)))

(: flood-fill (Board Integer -> Board))
(define (flood-fill board p)
  (for/fold ([board (set board p)]) ([mover (in-list movers)])
    (let ([p (vector-ref mover p)])
      (if (and (valid? p) (clear? board p))
        (flood-fill board p)
        board))))

(: no-islands? (Board -> Boolean))
(define (no-islands? mask)
  (let ([zeros (zero-count mask)])
    (and (zeros . >= . 5)
         (let: loop : Boolean ([mask : Integer mask] [zeros : Integer zeros])
           (if (= mask #x3FFFFFFFFFFFF)
             #t
             (let* ([p (assert (find-free-cell mask))]
                    [mask (flood-fill mask p)]
                    [new-zeros (zero-count mask)])
               (and ((- zeros new-zeros) . >= . 5)
                    (loop mask new-zeros))))))))

(: get-bitmask (Integer Piece -> (Option Integer)))
(define (get-bitmask p piece)
  (let ([mask (arithmetic-shift 1 p)])
    (let loop ([p p] [cells piece] [mask mask])
      (if (null? cells)
        mask
        (let ([p (vector-ref (car cells) p)])
          (and (valid? p) (loop p (cdr cells) (set mask p))))))))

(: all-bitmasks (Piece Integer -> (Listof Integer)))
(define (all-bitmasks piece color)
  (let: ([pieces : (Listof Piece)
          (let-values ([(accum piece)
                        (for/fold: : (values (Listof Piece) Piece)
                                   ([accum : (Listof Piece) null]
                                    [piece : Piece piece])
                                  ([orientations : Integer (in-range 2)])
                          (let-values ([(accum piece)
                                        (for/fold: : (values (Listof Piece) Piece)
                                                   ([accum : (Listof Piece) accum]
                                                    [piece : Piece piece])
                                                  ([orientations : Integer (in-range (- 6 (* 3 (if (= color 4) 1 0))))])
                                          (values (cons piece accum)
                                                  (map rotate piece)))])
                            (values accum (map flip piece))))])
           accum)])
    (reverse
     (for*/fold: : (Listof Integer)
                 ([accum : (Listof Integer) null])
                 ([piece : Piece (in-list pieces)]
                 [p : Integer (in-range 0 size)])
       (let ([mask (get-bitmask p piece)])
         (if (and mask (no-islands? mask)) (cons mask accum) accum))))))

(: generate-bitmasks-pieces (Listof (Listof Direction)))
(define generate-bitmasks-pieces
  (list (list E  E  E  SE)
        (list SE SW W  SW)
        (list W  W  SW SE)
        (list E  E  SW SE)
        (list NW W  NW SE SW)
        (list E  E  NE W)
        (list NW NE NE W)
        (list NE SE E  NE)
        (list SE SE E  SE)
        (list E  NW NW NW)))
(: generate-bitmasks ( -> (Vectorof (Vectorof (Listof Board)))))
(define (generate-bitmasks)
  (let ([masks-at-cell
         (list->vector
          (for/list: : (Listof (Vectorof (Listof Board)))
                     ([i : Integer (in-range size)])
            (list->vector (for/list: : (Listof (Listof Board))
                            ([j : Integer (in-range 10)]) null))))])
    (for ([piece (in-list generate-bitmasks-pieces)]
          [color (in-naturals)])
      (let: loop : (Vectorof (Vectorof (Listof Board)))
            ([masks : (Listof Integer) ((inst sort Integer Integer) (all-bitmasks piece color) >)]
             [cell-bit : Integer (sub1 size)]
             [cell-counter : Integer (sub1 size)])
        (if (null? masks)
          masks-at-cell
          (if (bitwise-bit-set? (car masks) cell-bit)
            (let ([vec (vector-ref masks-at-cell cell-counter)])
              (vector-set! vec color (cons (car masks) (vector-ref vec color)))
              (loop (cdr masks) cell-bit cell-counter))
            (loop masks (sub1 cell-bit) (sub1 cell-counter))))))
    (for ([v (in-vector masks-at-cell)])
      (for ([j (in-naturals)]
            [val (in-vector v)])
        (vector-set! v j (reverse val))))
    masks-at-cell))

(define masks-at-cell (generate-bitmasks))

(: masks (Vectorof Integer))
(define masks (make-vector 10 0))
(: to-go Integer)
(define to-go 0)
(define-type Solution Bytes)
(: solutions (MPairof (Option Solution) (Option Solution)))
(define solutions (mcons #f #f)) ; keeps (min max) solutions

(: solve-cell! (Integer Board -> Void))
(define (solve-cell! cell board)
  (when (and (positive? to-go) (not (negative? cell)))
    ;; Need solutions and not off board
    (cond [(= board #x3FFFFFFFFFFFF)
           ;; Solved
           (add-solutions!)]
          [(not (clear? board cell))
           ;; Cell full, so try next
           (solve-cell! (sub1 cell) board)]
          [else
           ;; Recur
           (for*: ([color : Integer (in-range 10)]
                   #:when (zero? (vector-ref masks color))
                   [mask : Integer (in-list (vector-ref (vector-ref masks-at-cell cell)
                                                        color))]
                   #:when (zero? (bitwise-and mask board)))
             (vector-set! masks color mask)
             (solve-cell! (sub1 cell) (bitwise-ior board mask))
             (vector-set! masks color 0))])))

(: add-solutions! ( -> Void))
(define (add-solutions!)
  (: add! (Solution -> Void))
  (define (add! solution)
    (let ((head (mcar solutions)))
      (cond [(not head)
             (set-mcar! solutions solution)
             (set-mcdr! solutions solution)]
            [(bytes<? solution head)
             (set-mcar! solutions solution)]
            [(bytes>? solution (assert (mcdr solutions)))
             (set-mcdr! solutions solution)])))
  (let* ([s (list->bytes
             (for/list: : (Listof Integer) ([pos : Integer (in-range size)])
               (assert (for/or: : (Option Integer) ([color : Integer (in-range 10)])
                                (and (not (clear? (vector-ref masks color) pos))
                                     (+ color (char->integer #\0)))))))]
         [ns (make-bytes size)])
    ;; Inverse
    (for*: ([y : Integer (in-range height)]
            [x : Integer (in-range width)])
      (bytes-set! ns (+ x (* y width))
                  (bytes-ref s (+ (- width (+ x 1))
                                  (* width (- height (+ y 1)))))))
    ;; Keep first and last only
    (add! s)
    (add! ns)
    (set! to-go (- to-go 2))))

(: print-solution (Solution -> Void))
(define (print-solution solution)
  (let ([solution (bytes->string/utf-8 solution)])
    (for ([y (in-range height)])
      (when (odd? y) (display " "))
      (for ([x (in-range width)])
        (printf "~a " (string-ref solution (+ x (* y width)))))
      (printf "\n"))
    (newline)))

(: solve! (Integer -> Void))
(define (solve! n)
  (set! to-go n)
  (solve-cell! (sub1 size) 0))

(command-line #:args (n)
  (let ([n (assert (string->number (assert n string?)) exact-integer?)])
    (solve! n)
    (printf "~a solutions found\n\n" (- n to-go))
    (print-solution (assert (mcar solutions)))
    (print-solution (assert (mcdr solutions)))))
