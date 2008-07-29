;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on a Python version:
;;   contributed by Olof Kraigher
;;    modified by Tupteq

#lang scheme/base
(require scheme/cmdline)

(define width 5)
(define height 10)

(define (rotate dir)
  (case dir
    [(E) 'NE]
    [(NE) 'NW]
    [(NW) 'W]
    [(W) 'SW]
    [(SW) 'SE]
    [(SE) 'E]))

(define (flip dir)
  (case dir
    [(E) 'W]
    [(NE) 'NW]
    [(NW) 'NE]
    [(W) 'E]
    [(SW) 'SE]
    [(SE) 'SW]))

(define move
  (make-immutable-hash
   (list
    (cons 'E (lambda (x y) (values (add1 x) y)))
    (cons 'W (lambda (x y) (values (sub1 x) y)))
    (cons 'NE (lambda (x y) (values (+ x (bitwise-and y 1)) (sub1 y))))
    (cons 'NW (lambda (x y) (values (sub1 (+ x (bitwise-and y 1))) (sub1 y))))
    (cons 'SE (lambda (x y) (values (+ x (bitwise-and y 1)) (add1 y))))
    (cons 'SW (lambda (x y) (values (sub1 (+ x (bitwise-and y 1))) (add1 y)))))))

(define move-procs
  (hash-map move (lambda (k v) v)))

(define (valid? x y)
  (and (0 . <= . x)
       (x . < . width)
       (0 . <= . y)
       (y . < . height)))

(define (clear? board pos)
  (not (bitwise-bit-set? board pos)))
(define (set board pos)
  (bitwise-ior board (arithmetic-shift 1 pos)))

(define (zero-count board)
  (for/fold ([count 0])
            ([i (in-range (* width height))])
    (if (clear? board i)
        (add1 count)
        count)))

(define (find-free-cell board)
  (let yloop ([y 0])
    (let xloop ([x 0])
      (if (= x width)
          (yloop (add1 y))
          (if (clear? board (+ x (* width y)))
              (values x y)
              (xloop (add1 x)))))))

(define (flood-fill board x y)
  (if (valid? x y)
      (let ([pos (+ x (* y width))])
        (if (clear? board pos)
            (for/fold ([board (set board pos)])
                      ([move-proc move-procs])
              (let-values ([(x y) (move-proc x y)])
                (flood-fill board x y)))
            board))
      board))

(define (no-islands? mask)
  (let ([zeros (zero-count mask)])
    (if (zeros . < . 5)
        #f
        (let loop ([mask mask][zeros zeros])
          (if (= mask #x3FFFFFFFFFFFF)
              #t
              (let*-values ([(x y) (find-free-cell mask)]
                            [(mask) (flood-fill mask x y)]
                            [(new-zeros) (zero-count mask)])
                (if ((- zeros new-zeros) . < . 5)
                    #f
                    (loop mask new-zeros))))))))

(define (get-bitmask x y piece)
  (let ([mask (arithmetic-shift 1 (+ x (* y width)))])
    (let loop ([x x][y y][cells piece][mask mask])
      (if (null? cells)
          mask
          (let-values ([(x y) ((hash-ref move (car cells)) x y)])
            (if (valid? x y)
                (loop x y (cdr cells) (set mask (+ x (* width y))))
                #f))))))

(define (all-bitmasks piece color)
  (let ([pieces
         (let-values ([(accum piece)
                       (for/fold ([accum null] [piece piece])
                           ([orientations (in-range 2)])
                         (let-values ([(accum piece)
                                       (for/fold ([accum accum] [piece piece])
                                           ([orientations (in-range (- 6 (* 3 (if (= color 4)
                                                                                  1
                                                                                  0))))])
                                         (values (cons piece accum)
                                                 (map rotate piece)))])
                           (values accum (map flip piece))))])
           accum)])
    (for*/list ([piece (in-list pieces)]
                [y (in-range height)]
                [x (in-range width)]
                [mask (:do-in ([(mask) (get-bitmask x y piece)]) ; should be in-value
                              #t () #t () #t #f ())]
                #:when (and mask (no-islands? mask)))
      mask)))

(define (generate-bitmasks)
  (let ([pieces '((E E E SE)
                  (SE SW W SW)
                  (W W SW SE)
                  (E E SW SE)
                  (NW W NW SE SW)
                  (E E NE W)
                  (NW NE NE W)
                  (NE SE E NE)
                  (SE SE E SE)
                  (E NW NW NW))]
        [masks-at-cell
         (list->vector
          (for/list ([i (in-range (* width height))])
            (list->vector
             (for/list ([j (in-range 10)])
               null))))])
    (for ([piece (in-list pieces)]
          [color (in-naturals)])
      (let loop ([masks (sort (all-bitmasks piece color) >)]
                 [cell-bit (sub1 (* width height))]
                 [cell-counter (sub1 (* width height))])
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
(define solutions null)

(define (solve-cell! cell board)
  (when (and (positive? to-go)
             (not (negative? cell)))
    ;; Need solutions and not off board
    (cond
     [(= board #x3FFFFFFFFFFFF)
      ;; Solved
      (add-solutions!)]
     [(not (clear? board cell))
      ;; Cell full, so try next
      (solve-cell! (sub1 cell) board)]
     [else
      ;; Recur
      (for ([color (in-range 10)])
        (when (zero? (vector-ref masks color))
          (for ([mask (in-list (vector-ref
                                (vector-ref masks-at-cell cell)
                                color))])
            (when (zero? (bitwise-and mask board))
              (vector-set! masks color mask)
              (solve-cell! (sub1 cell) (bitwise-ior board mask))
              (vector-set! masks color 0)))))])))

(define (add-solutions!)
  (let ([digits
         (for/list ([pos (in-range (* width height))])
           (for/or ([color (in-range 10)])
                   (and (not (clear? (vector-ref masks color) pos))
                        color)))])
    (let ([s (list->string
              (map (lambda (digit)
                     (if digit
                         (integer->char (+ digit (char->integer #\0)))
                         #\.))
                   digits))]
          [ns (make-string (* width height))])
      ;; Inverse
      (for* ([y (in-range height)]
             [x (in-range width)])
            (string-set! ns (+ x (* y width))
                         (string-ref s (+ (- width (+ x 1))
                                          (* width (- height (+ y 1)))))))
      ;; Append
      (set! solutions (cons s solutions))
      (set! solutions (cons ns solutions))
      (set! to-go (- to-go 2)))))

(define (print-solution solution)
  (for ([y (in-range height)])
    (for ([x (in-range width)])
      (display (string-ref solution (+ x (* y width))))
      (display " "))
    (display "\n")
    (when (even? y)
      (display " ")))
  (newline))

(define (solve! n)
  (set! to-go n)
  (solve-cell! (sub1 (* width height)) 0))

(command-line #:args (n) (solve! (string->number n)))
(let ([solutions (sort solutions string<?)])
  (printf "~a solutions found\n\n" (length solutions))
  (print-solution (car solutions))
  (print-solution (list-ref solutions (sub1 (length solutions)))))
