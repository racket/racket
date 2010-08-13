#lang mzscheme
(require mzlib/etc
         mzlib/class
         mzlib/unit
         mred)

(provide game@)

(define game@ (unit (import) (export)

(define (get-bitmap bitmap)
  (define f (make-object dialog% "Choose Size" #f #f #f #f #f '(resize-border)))
  (define bm-panel (make-object vertical-panel% f))
  (define bm-message (make-object message% bitmap bm-panel))
  (define size-message
    (make-object message% (format "Image size: ~a x ~a pixels"
                                  (send bitmap get-width)
                                  (send bitmap get-height))
                 bm-panel))
  (define wide-panel (make-object vertical-panel% f '(border)))
  (define sw (make-object slider% "Tiles (width)" 2 30 wide-panel
                          (lambda (_1 _2) (update-horizontal-cutoff))))
  (define tall-panel (make-object vertical-panel% f '(border)))
  (define sh (make-object slider% "Tiles (height)" 2 30 tall-panel
                          (lambda (_1 _2) (update-vertical-cutoff))))
  (define button-panel (make-object horizontal-panel% f))

  (define cancelled? #t)

  (define cancel (make-object button% "Cancel" button-panel
                              (lambda (_1 _2) (send f show #f))))
  (define ok (make-object button% "OK" button-panel
                          (lambda (_1 _2)
                            (set! cancelled? #f) 
                            (send f show #f)) '(border)))

  (define vertical-cutoff 0)
  (define vertical-cutoff-message (make-object message% "" tall-panel))

  (define horizontal-cutoff 0)
  (define horizontal-cutoff-message (make-object message% "" wide-panel))

  (define (update-vertical-cutoff)
    (set! vertical-cutoff (modulo (send bitmap get-height) (send sh get-value)))
    (send vertical-cutoff-message set-label 
          (if (= 0 vertical-cutoff)
            ""
            (format "Vertical cutoff ~a pixels" vertical-cutoff))))
  (define (update-horizontal-cutoff)
    (set! horizontal-cutoff (modulo (send bitmap get-width) (send sw get-value)))
    (send horizontal-cutoff-message set-label
          (if (= 0 horizontal-cutoff)
            ""
            (format "Horizontal cutoff ~a pixels" horizontal-cutoff))))

  (send horizontal-cutoff-message stretchable-width #t)
  (send vertical-cutoff-message stretchable-width #t)
  (update-vertical-cutoff)
  (update-horizontal-cutoff)
  (send button-panel set-alignment 'right 'center)
  (send button-panel stretchable-height #f)
  (send bm-panel set-alignment 'center 'center)
  (send wide-panel stretchable-height #f)
  (send tall-panel stretchable-height #f)
  (make-object grow-box-spacer-pane% button-panel)
  (send f show #t)

  (if cancelled?
    (values #f #f #f)
    (let* ([nb (make-object bitmap% 
                 (- (send bitmap get-width) horizontal-cutoff)
                 (- (send bitmap get-height) vertical-cutoff))]
           [bdc (make-object bitmap-dc% nb)])
      (send bdc draw-bitmap-section bitmap 0 0 0 0
            (- (send bitmap get-width) horizontal-cutoff)
            (- (send bitmap get-height) vertical-cutoff))
      (send bdc set-bitmap #f)
      (values nb (send sw get-value) (send sh get-value)))))

(define-struct loc (x y))
;; board = (vector-of (vector-of (union #f (make-loc n1 n2))))

(define (board-for-each board f)
  (let loop ([i (vector-length board)])
    (unless (zero? i)
      (let ([row (vector-ref board (- i 1))])
        (let loop ([j (vector-length row)])
          (unless (zero? j)
            (f (- i 1) (- j 1) (vector-ref row (- j 1)))
            (loop (- j 1)))))
      (loop (- i 1)))))

(define (move-one board from-i from-j to-i to-j)
  (let ([from-save (board-ref board from-i from-j)]
        [to-save (board-ref board to-i to-j)])
    (board-set! board from-i from-j to-save)
    (board-set! board to-i to-j from-save)))

(define (board-set! board i j v)
  (vector-set! (vector-ref board i) j v))
(define (board-ref board i j)
  (vector-ref (vector-ref board i) j))

(define (get-board-width board)
  (vector-length board))
(define (get-board-height board)
  (vector-length (vector-ref board 0)))

(define (randomize-board board hole-i hole-j)
  (let ([board-width (get-board-width board)]
        [board-height (get-board-height board)])
    (let loop ([no-good #f]
               [i (* 10 board-width board-height)]
               [m-hole-i hole-i]
               [m-hole-j hole-j])
      (cond
        [(zero? i) ;; move hole back to last spot
         (let ([i-diff (abs (- m-hole-i hole-i))])
           (let loop ([i 0])
             (unless (= i i-diff)
               (move-one board (+ m-hole-i i)
                         m-hole-j (+ m-hole-i i (if (< m-hole-i hole-i) +1 -1))
                         m-hole-j)
               (loop (+ i 1)))))
         (let ([j-diff (abs (- m-hole-j hole-j))])
           (let loop ([j 0])
             (unless (= j j-diff)
               (move-one board hole-i (+ m-hole-j j)
                         hole-i (+ m-hole-j j (if (< m-hole-j hole-j) +1 -1)))
               (loop (+ j 1)))))]
        [else
         (let ([this-dir (get-random-number 4 no-good)])
           (let-values ([(new-i new-j)
                         (case this-dir
                           ;; up
                           [(0) (values (- m-hole-i 1) m-hole-j)]
                           [(1) (values (+ m-hole-i 1) m-hole-j)]
                           [(2) (values m-hole-i (- m-hole-j 1))]
                           [(3) (values m-hole-i (+ m-hole-j 1))])])
             (if (and (<= 0 new-i)
                      (< new-i board-width)
                      (<= 0 new-j)
                      (< new-j board-height))
               (let ([next-no-good
                      (case this-dir [(0) 1] [(1) 0] [(2) 3] [(3) 2])])
                 (move-one board new-i new-j m-hole-i m-hole-j)
                 (loop next-no-good (- i 1) new-i new-j))
               (loop no-good (- i 1) m-hole-i m-hole-j))))]))))

(define (get-random-number bound no-good)
  (let ([raw (random (- bound 1))])
    (cond [(not no-good) raw]
          [(< raw no-good) raw]
          [else (+ raw 1)])))

(define line-brush
  (send the-brush-list find-or-create-brush "black" 'transparent))
(define line-pen (send the-pen-list find-or-create-pen "white" 1 'solid))
(define mistake-brush
  (send the-brush-list find-or-create-brush "black" 'transparent))
(define mistake-pen (send the-pen-list find-or-create-pen "red" 1 'solid))
(define pict-brush (send the-brush-list find-or-create-brush "black" 'solid))
(define pict-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
(define white-brush (send the-brush-list find-or-create-brush "white" 'solid))
(define white-pen (send the-pen-list find-or-create-pen "white" 1 'solid))

(define slidey-canvas%
  (class canvas% 
    (init-field bitmap board-width board-height)

    (define show-mistakes? #f)
    (define/public (show-mistakes nv)
      (set! show-mistakes? nv)
      (unless solved? (on-paint)))

    (define solved? #f)

    (define board
      (build-vector
       board-width
       (lambda (i) (build-vector board-height (lambda (j) (make-loc i j))))))
    (define hole-i (- board-width 1))
    (define hole-j (- board-height 1))
    (board-set! board hole-i hole-j #f)

    (define/override (on-paint)
      (if solved?
        (send (get-dc) draw-bitmap bitmap 0 0)
        (board-for-each board (lambda (i j v) (draw-cell i j)))))

    (define/override (on-event evt)
      (unless solved?
        (when (send evt button-down? 'left)
          (let-values ([(i j) (xy->ij (send evt get-x) (send evt get-y))])
            (slide i j)))))
    (inherit get-client-size get-dc)

    (define/private (check-end-condition)
      (let ([answer #t])
        (board-for-each
         board
         (lambda (i j v)
           (when v
             (unless (and (= i (loc-x v)) (= j (loc-y v)))
               (set! answer #f)))))
        (when answer (set! solved? #t))))

    (define/private (slide i j)
      (cond
        [(= j hole-j)
         (let loop ([new-hole-i hole-i])
           (unless (= new-hole-i i)
             (let ([next (if (< i hole-i) sub1 add1)])
               (move-one board (next new-hole-i) hole-j new-hole-i hole-j)
               (draw-cell new-hole-i hole-j)
               (draw-cell (next new-hole-i) hole-j)
               (loop (next new-hole-i)))))
         (set! hole-i i)
         (check-end-condition)
         (when solved? (on-paint))]
        [(= i hole-i)
         (let loop ([new-hole-j hole-j])
           (unless (= new-hole-j j)
             (let ([next (if (< j hole-j)
                           sub1
                           add1)])
               (move-one board hole-i (next new-hole-j) hole-i new-hole-j)
               (draw-cell hole-i new-hole-j)
               (draw-cell hole-i (next new-hole-j))
               (loop (next new-hole-j)))))
         (set! hole-j j)
         (check-end-condition)
         (when solved?
           (on-paint))]
        [else (void)]))

    (define/private (xy->ij x y)
      (let-values ([(w h) (get-client-size)])
        (values (inexact->exact (floor (* board-width (/ x w))))
                (inexact->exact (floor (* board-height (/ y h)))))))

    (define/private (ij->xywh i j)
      (let-values ([(w h) (get-client-size)])
        (let ([cell-w (/ w board-width)]
              [cell-h (/ h board-height)])
          (values (* i cell-w) (* j cell-h) cell-w cell-h))))
    (define/private (draw-cell draw-i draw-j)
      (let-values ([(xd yd wd hd) (ij->xywh draw-i draw-j)])
        (let* ([dc (get-dc)]
               [indices (board-ref board draw-i draw-j)])
          (if indices
            (let ([bm-i (loc-x indices)]
                  [bm-j (loc-y indices)])
              (let-values ([(xs ys ws hs) (ij->xywh bm-i bm-j)])
                (send dc set-pen pict-pen)
                (send dc set-brush pict-brush)
                (send dc draw-bitmap-section bitmap xd yd xs ys wd hd)
                (if (and show-mistakes?
                         (or (not (= draw-i bm-i))
                             (not (= draw-j bm-j))))
                  (begin (send dc set-pen mistake-pen)
                         (send dc set-brush mistake-brush))
                  (begin (send dc set-pen line-pen)
                         (send dc set-brush line-brush)))
                (send dc draw-rectangle xd yd wd hd)))
            (begin (send dc set-pen white-pen)
                   (send dc set-brush white-brush)
                   (send dc draw-rectangle xd yd wd hd))))))

    (inherit stretchable-width stretchable-height
             min-client-width min-client-height)
    (super-instantiate ())
    (randomize-board board hole-i hole-j)
    (stretchable-width #f)
    (stretchable-height #f)
    (min-client-width (send bitmap get-width))
    (min-client-height (send bitmap get-height))))

(define f (make-object frame% "Slidey"))
(define p (make-object horizontal-panel% f))
(send p set-alignment 'center 'center)
(define slidey-canvas
  (make-object slidey-canvas%
    (make-object bitmap%
      (build-path (collection-file-path "11.jpg" "games" "slidey")))
    6 6 p))
(define bp (make-object horizontal-panel% f))
(send bp stretchable-height #f)
(define show-mistakes
  (make-object check-box% "Show misplaced pieces" bp 
               (lambda ___ (send slidey-canvas show-mistakes (send show-mistakes get-value)))))
(make-object grow-box-spacer-pane% bp)

(define (change-bitmap)
  (let ([fn (get-file)])
    (when fn
      (let ([bm (make-object bitmap% fn)])
        (cond
          [(send bm ok?)
           (let-values ([(bitmap w h) (get-bitmap bm)])
             (when bitmap
               (send p change-children (lambda (l) null))
               (set! slidey-canvas (make-object slidey-canvas% bitmap w h p))))]
                [else (message-box "Slidey" (format "Unrecognized image format: ~a" fn))])))))

(define mb (make-object menu-bar% f))
(define file-menu (make-object menu% "File" mb))
(make-object menu-item% "Open Image" file-menu (lambda (_1 _2) (change-bitmap)) #\o)
(make-object menu-item% "Close Window" file-menu (lambda (_1 _2) (send f show #f)) #\w)

(send f show #t)

))
