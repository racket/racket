#lang frtime
(require (lifted (only-in texpict/mrpict colorize) colorize)
         (lifted (only-in texpict/mrpict vl-append) vl-append)
         (lifted (only-in texpict/mrpict vr-append) vr-append)
         (lifted (only-in texpict/mrpict text) text)
         (lifted (only-in texpict/mrpict cc-superimpose) cc-superimpose)
         (lifted (only-in texpict/mrpict hb-append) hb-append)
         (lifted (only-in texpict/mrpict ht-append) ht-append)
         (lifted (only-in texpict/mrpict pin-over) pin-over)
         (lifted (only-in texpict/mrpict blank) blank)
         (lifted (only-in texpict/mrpict dc-for-text-size) dc-for-text-size) 
         (lifted (only-in texpict/utils filled-rectangle) filled-rectangle)
         (lifted (only-in texpict/utils rectangle) rectangle)
         frtime/gui/fred mred
         (only-in frtime/core/frp do-in-manager do-in-manager-after))

;; TODO: layered drawing, pause, game over

(do-in-manager
 (dc-for-text-size (new bitmap-dc% [bitmap (make-object bitmap% 64 64)])))

(define size (new-cell 20))
(define row-width 12)

(do-in-manager-after '())

(define list-ref*
  (case-lambda
    [(lst idx) (list-ref lst idx)]
    [(lst idx . is) (apply list-ref* (list-ref lst idx) is)]))

(define (rotate matrix)
  (let ([rows (length matrix)]
        [columns (length (first matrix))])
    (build-list
     columns
     (lambda (i)
       (build-list rows (lambda (j) (list-ref* matrix j (- columns i 1))))))))

(define shapes
  (map (lambda (desc)
         (map (lambda (row)
                (map (lambda (cell) (if (zero? cell) #f (first desc))) ; color
                     row))
              (rest desc)))
       '(("tomato"     ; T shape
          (0 1 0)
          (1 1 1))
         ("orange"     ; S shape
          (0 1 1)
          (1 1 0))
         ("lightblue"  ; Z shape
          (1 1 0)
          (0 1 1))
         ("lightgreen" ; L shape
          (1 1 1)
          (1 0 0))
         ("gray"       ; reverse L shape
          (1 1 1)
          (0 0 1))
         ("lavender"   ; I shape
          (1 1 1 1))
         ("purple"     ; block shape
          (1 1)
          (1 1)))))

(define 1x1 (cc-superimpose (colorize (rectangle size size) "black")
                            (filled-rectangle (- size 2) (- size 2))))

(define (make-cell c)
  (if c (colorize 1x1 c) (blank size)))

(define (make-row lst)
  (apply hb-append (map make-cell lst)))

(define (make-shape lol)
  (apply vl-append (map make-row lol)))

(define frame (new ft-frame% [label "Tetris"] [shown #t]
                   [min-width (* size 20)] [min-height (* size 20)]))

(define (intersects grid shape h-pos v-pos)
  (ormap (lambda (shape-row cell-v)
           (ormap (lambda (shape-cell cell-h)
                    (and (value-now shape-cell) (value-now (list-ref* grid cell-v cell-h))))
                  shape-row
                  (build-list (length shape-row) (lambda (i) (+ i h-pos)))))
         shape
         (build-list (length shape) (lambda (i) (+ i v-pos)))))

(define empty-row (build-list row-width (lambda (j) (and (or (= j 0) (= j (sub1 row-width))) "black"))))

(define n-rows 20)

(define bottom-row
  (append (build-list row-width (lambda (_) "black")) (list #f)))

(define (replenish-rows grid)
  (append (build-list (- n-rows (length grid)) (lambda (_) empty-row)) grid))

(define (remove-completed-rows grid)
  (let ([new-grid (filter (lambda (row) (not (andmap identity row))) grid)])
    (list new-grid (case (- (length grid) (length new-grid))
                     [(0) 0]
                     [(1) 20]
                     [(2) 60]
                     [(3) 200]
                     [(4) 1000]))))

(define (add-shape grid shape h-pos v-pos)
  (map (lambda (row row-num)
         (map (lambda (cell col-num)
                (or cell (let ([shape-v-pos (- row-num v-pos)]
                               [shape-h-pos (- col-num h-pos)])
                           (and (< -1 shape-v-pos (length shape))
                                (< -1 shape-h-pos (length (first shape)))
                                (list-ref* shape shape-v-pos shape-h-pos)))))
              row (build-list (length row) identity)))
       grid (build-list (length grid) identity)))

(define (move direction grid shape h-pos v-pos new-shape score)
  (case direction
    [(left right rotate)
     (let ([new-h ((case direction
                     [(left) sub1]
                     [(right) add1]
                     [(rotate) identity]) h-pos)]
           [rshape (if (eq? direction 'rotate) (rotate shape) shape)])
       (cons grid
             (if (intersects grid rshape new-h v-pos)
                 (list shape h-pos v-pos new-shape score)
                 (list rshape new-h v-pos new-shape score))))]
    [(down)
     (if (intersects grid shape h-pos (add1 v-pos))
         (let ([new-grid/points (remove-completed-rows
                                 (add-shape grid shape h-pos v-pos))])
           (list (replenish-rows (first new-grid/points))
                 new-shape 5 0 (list-ref shapes (random (length shapes))) (+ (second new-grid/points) score)))
         (list grid shape h-pos (add1 v-pos) new-shape score))]
    [(drop) (let ([new-state (move 'down grid shape h-pos v-pos new-shape score)])
              (if (not (eq? (first new-state) grid))
                  (list grid shape h-pos v-pos new-shape score)
                  (move 'drop grid shape h-pos (add1 v-pos) new-shape score)))]
    [(reset) (list init-grid new-shape 5 0 (list-ref shapes (random (length shapes))) 0)]))

(define init-grid
  (append (build-list (sub1 n-rows) (lambda (i) empty-row)) (list bottom-row)))

(define-values (canvas state rate)
  (letrec ([canvas (new ft-canvas% [parent frame] [style '(no-autoclear)]
                        [pict anim])]
           [keys (send canvas get-key-events)]
           [left (keys 'left)] [right (keys 'right)]
           [up (keys 'up)] [down (keys 'down)]
           [space (keys #\space)]
           [reset (keys #\r)]
           [state (collect-b
                   (merge-e (left . -=> . 'left)
                            (right . -=> . 'right)
                            (up . -=> . 'rotate)
                            ((changes (quotient (modulo (inexact->exact (floor milliseconds)) 100000000)
                                                rate)) . -=> . 'down)
                            (down . -=> . 'down)
                            (space . -=> . 'drop)
                            (reset . -=> . 'reset))
                   (list init-grid (list-ref shapes (random (length shapes))) 5 0
                         (list-ref shapes (random (length shapes))) 0)
                   (lambda (direction old-state)
                     (apply move direction old-state)))]
           [grid (first state)]
           [shape (second state)]
           [h-pos (third state)]
           [v-pos (fourth state)]
           [new-shape (fifth state)]
           [score (sixth state)]
           [rate (inf-delay (+ 250 (quotient 75000 (+ 100 score))))]
           [anim (ht-append
                  (pin-over (make-shape grid)
                            (* size h-pos)
                            (* size v-pos)
                            (make-shape shape))
                  (vl-append
                   (blank size)
                   (cc-superimpose
                    (rectangle (* size 6) (* size 6))
                    (make-shape new-shape))
                   (blank size)
                   (text (format "Score: ~a" score))))])
    (values canvas state rate)))
