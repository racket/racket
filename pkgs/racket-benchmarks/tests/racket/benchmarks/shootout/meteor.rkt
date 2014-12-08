#lang racket/base

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on a Python version:
;;   contributed by Olof Kraigher
;;    modified by Tupteq
;;   contributed by Matthew Flatt
;;   optimized by Eli Barzilay

(require racket/cmdline)

(define width 5)
(define height 10)
(define size (* width height))

(define (valid-xy? x y)
  (and (0 . <= . x)
       (x . < . width)
       (0 . <= . y)
       (y . < . height)))

(define (mover fun)
  (let ([t (make-vector size)])
    (for ([p (in-range size)])
      (vector-set! t p (let*-values ([(y x) (quotient/remainder p width)]
                                     [(x y) (fun x y)])
                         (if (valid-xy? x y) (+ x (* y width)) -1))))
    t))

(define E
  (mover (lambda (x y) (values (add1 x) y))))
(define W
  (mover (lambda (x y) (values (sub1 x) y))))
(define NE
  (mover (lambda (x y) (values (+ x (bitwise-and y 1)) (sub1 y)))))
(define NW
  (mover (lambda (x y) (values (sub1 (+ x (bitwise-and y 1))) (sub1 y)))))
(define SE
  (mover (lambda (x y) (values (+ x (bitwise-and y 1)) (add1 y)))))
(define SW
  (mover (lambda (x y) (values (sub1 (+ x (bitwise-and y 1))) (add1 y)))))

(define rotate-list (list E NE NW W SW SE E))
(define (rotate dir)
  (cadr (memq dir rotate-list)))

(define flip-alist (list (cons E W) (cons NE NW) (cons NW NE)
                         (cons W E) (cons SW SE) (cons SE SW)))
(define (flip dir) (cdr (assq dir flip-alist)))

(define movers (list E W NE NW SE SW))

(define (valid? p)
  (p . >= . 0))

(define (clear? board pos)
  (not (bitwise-bit-set? board pos)))
(define (set board pos)
  (bitwise-ior board (arithmetic-shift 1 pos)))

(define (zero-count board)
  (for/fold ([count 0]) ([i (in-range size)])
    (if (clear? board i) (add1 count) count)))

(define (find-free-cell board)
  (for/or ([p (in-range 0 size)])
    (and (clear? board p) p)))

(define (flood-fill board p)
  (for/fold ([board (set board p)]) ([mover (in-list movers)])
    (let ([p (vector-ref mover p)])
      (if (and (valid? p) (clear? board p))
        (flood-fill board p)
        board))))

(define (no-islands? mask)
  (let ([zeros (zero-count mask)])
    (and (zeros . >= . 5)
         (let loop ([mask mask] [zeros zeros])
           (if (= mask #x3FFFFFFFFFFFF)
             #t
             (let* ([p (find-free-cell mask)]
                    [mask (flood-fill mask p)]
                    [new-zeros (zero-count mask)])
               (and ((- zeros new-zeros) . >= . 5)
                    (loop mask new-zeros))))))))

(define (get-bitmask p piece)
  (let ([mask (arithmetic-shift 1 p)])
    (let loop ([p p] [cells piece] [mask mask])
      (if (null? cells)
        mask
        (let ([p (vector-ref (car cells) p)])
          (and (valid? p) (loop p (cdr cells) (set mask p))))))))

(define (all-bitmasks piece color)
  (let ([pieces
         (let-values ([(accum piece)
                       (for/fold ([accum null] [piece piece])
                                 ([orientations (in-range 2)])
                         (let-values ([(accum piece)
                                       (for/fold ([accum accum] [piece piece])
                                                 ([orientations (in-range (- 6 (* 3 (if (= color 4) 1 0))))])
                                         (values (cons piece accum)
                                                 (map rotate piece)))])
                           (values accum (map flip piece))))])
           accum)])
    (reverse
     (for*/fold ([accum null])
                ([piece (in-list pieces)]
                 [p (in-range 0 size)])
       (let ([mask (get-bitmask p piece)])
         (if (and mask (no-islands? mask)) (cons mask accum) accum))))))

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
(define (generate-bitmasks)
  (let ([masks-at-cell
         (list->vector
          (for/list ([i (in-range size)])
            (list->vector (for/list ([j (in-range 10)]) null))))])
    (for ([piece (in-list generate-bitmasks-pieces)]
          [color (in-naturals)])
      (let loop ([masks (sort (all-bitmasks piece color) >)]
                 [cell-bit (sub1 size)]
                 [cell-counter (sub1 size)])
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

(define masks (make-vector 10 0))
(define to-go 0)
(define solutions (mcons #f #f)) ; keeps (min max) solutions

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
           (for* ([color (in-range 10)]
                  #:when (zero? (vector-ref masks color))
                  [mask (in-list (vector-ref (vector-ref masks-at-cell cell)
                                             color))]
                  #:when (zero? (bitwise-and mask board)))
             (vector-set! masks color mask)
             (solve-cell! (sub1 cell) (bitwise-ior board mask))
             (vector-set! masks color 0))])))

(define (add-solutions!)
  (define (add! solution)
    (let ((head (mcar solutions)))
      (cond [(not head)
             (set-mcar! solutions solution)
             (set-mcdr! solutions solution)]
            [(bytes<? solution head)
             (set-mcar! solutions solution)]
            [(bytes>? solution (mcdr solutions))
             (set-mcdr! solutions solution)])))
  (let* ([s (list->bytes
             (for/list ([pos (in-range size)])
               (for/or ([color (in-range 10)])
                       (and (not (clear? (vector-ref masks color) pos))
                            (+ color (char->integer #\0))))))]
         [ns (make-bytes size)])
    ;; Inverse
    (for* ([y (in-range height)]
           [x (in-range width)])
      (bytes-set! ns (+ x (* y width))
                  (bytes-ref s (+ (- width (+ x 1))
                                  (* width (- height (+ y 1)))))))
    ;; Keep first and last only
    (add! s)
    (add! ns)
    (set! to-go (- to-go 2))))

(define (print-solution solution)
  (let ([solution (bytes->string/utf-8 solution)])
    (for ([y (in-range height)])
      (when (odd? y) (display " "))
      (for ([x (in-range width)])
        (printf "~a " (string-ref solution (+ x (* y width)))))
      (printf "\n"))
    (newline)))

(define (solve! n)
  (set! to-go n)
  (solve-cell! (sub1 size) 0))

(command-line #:args (n)
  (let ([n (string->number n)])
    (solve! n)
    (printf "~a solutions found\n\n" (- n to-go))
    (print-solution (mcar solutions))
    (print-solution (mcdr solutions))))
