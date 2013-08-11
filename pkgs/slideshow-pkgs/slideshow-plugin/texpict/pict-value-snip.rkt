#|

TODO: copying/pasting of pict snips doesn't preserve all of the information.
It only preserves the look, for now. The bounding boxes for children, in particular,
are gone.

When pressing or releasing the modifier keys, the snip doesn't update immediately;
the user has to move the mouse first.

|#

(module pict-value-snip mzscheme
  (require texpict/mrpict
           texpict/utils
           mzlib/class
           racket/gui/base
           mzlib/etc
           mzlib/list)
  
  (provide pict-value-snip%)

  (define pict-value-snip%
    (class snip%
      (init-field pict)
      (define top-align? #t)
      (define white? #f)
      (define pict-drawer (make-pict-drawer pict))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
	(let ([smoothing (send dc get-smoothing)])
	  (send dc set-smoothing 'aligned)
	  (pict-drawer dc x y)
	  (send dc set-smoothing smoothing))
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)])
          (send dc set-brush (send the-brush-list find-or-create-brush "black" 'transparent))
          (send dc set-pen (send the-pen-list find-or-create-pen
                                 (if white? "white" "black")
                                 1
                                 'solid))
          (for-each (lambda (child)
                      (send dc draw-rectangle
                            (+ x (hbox-x child))
                            (+ y (hbox-y child))
                            (hbox-w child)
                            (hbox-h child)))
                    highlight-children)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)))
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! w (pict-width pict))
        (set-box/f! h (pict-height pict))
        (cond
          [top-align?
           (set-box/f! descent (- (pict-height pict) (pict-ascent pict)))
           (set-box/f! space 0)]
          [else
           (set-box/f! descent (pict-descent pict))
           (set-box/f! space 0)])
        (set-box/f! lspace 0)
        (set-box/f! rspace 0))
      
      (define highlight-children '())
      
      (define/override (on-event dc x y editorx editory evt)
        (let* ([sx (- (send evt get-x) x)]
               [sy (- (send evt get-y) y)]
               [needs-update? #f]
               [update-children
                (lambda (children)
                  (unless (equal? children highlight-children)
                    (set! needs-update? #t)
                    (set! highlight-children children)))])
          
          (unless (equal? white? (send evt get-shift-down))
            (set! white? (send evt get-shift-down))
            (set! needs-update? #t))
          
          (cond
            [(send evt get-control-down)
             (update-children (all-boxes pict))]
            [(or (send evt get-alt-down)
                 (send evt get-meta-down))
             (update-children (included-boxes pict sx sy))]
            [else
             (update-children (minimal-boxes pict sx sy))])
          
          (when needs-update?
            (refresh))))
      
      ;; this doesn't work at all.
      (define/override (on-char dc x y editorx editory evt)
        (cond
          [(eq? (send evt get-key-code) 'shift)
           (set! white? (eq? 'press (send evt get-key-release-code)))
           (refresh)]
          [else (void)]))
      
      (define/override (adjust-cursor dc x y editorx editory event)
        arrow-cursor)
      
      (inherit get-admin)
      (define/private (refresh)
        (let ([admin (get-admin)])
          (when admin
            (send admin needs-update this 0 0 (pict-width pict) (pict-height pict)))))
          
      (define/override (copy) (new pict-value-snip% (pict pict)))
      
      (define/override (write in)
        (let ([w (inexact->exact (ceiling (pict-width pict)))]
              [h (inexact->exact (ceiling (pict-height pict)))])
          (send in put h)
          (cond
            [(or (= w 0)
                 (= h 0))
             (send in put 1 #"")]
            [else
             (let* ([bm (make-object bitmap% w h)]
                    [str (make-bytes (* w h 4))]
                    [bdc (make-object bitmap-dc% bm)])
               (send bdc clear)
               (send bdc set-smoothing 'aligned)
               (pict-drawer bdc 0 0)
               (send bdc get-argb-pixels 0 0 w h str)
               (send bdc set-bitmap #f)
               (send in put (+ 1 (bytes-length str)) str))])))
      
      (super-new)
      (inherit set-snipclass set-flags get-flags)
      (set-snipclass pict-snipclass)
      (set-flags (cons 'handles-events (get-flags)))))
  
  (define arrow-cursor (make-object cursor% 'arrow))

  (define pict-snip-class%
    (class snip-class%
      (define/override (read in)
        (define pict
          (let ([h (send in get-exact)]
                [bitmap-bstr (send in get-bytes)])
            (cond
              [(and (number? h)
                    (bytes? bitmap-bstr)
                    (or (zero? h)
                        (zero? (modulo (bytes-length bitmap-bstr) (* 4 h)))))
               (if (zero? h)
                   (blank)
                   (let* ([w (quotient (bytes-length bitmap-bstr) (* 4 h))]
                          [bm (make-object bitmap% w h)]
                          [bdc (make-object bitmap-dc% bm)])
                     (send bdc set-argb-pixels 0 0 w h bitmap-bstr)
                     (send bdc set-bitmap #f)
                     (bitmap bm)))]
              [else (x-out (ellipse 10 10))])))
        (new pict-value-snip% (pict pict)))
      (super-new)))
  
  (define (x-out p)
    (cc-superimpose
     p
     (dc (lambda (dc dx dy)
           (send dc draw-line dx dy (+ dx (pict-width p)) (+ dy (pict-height p)))
           (send dc draw-line dx (+ dy (pict-height p)) (+ dx (pict-width p)) dy))
         (pict-width p)
         (pict-height p)
         0
         0)))
  
  (define pict-snipclass (new pict-snip-class%))
  (send pict-snipclass set-classname "drscheme:pict-value-snip%")
  (send pict-snipclass set-version 0)
  (send (get-the-snip-class-list) add pict-snipclass)
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  (define (minimal-boxes pict x y)
    (minimal-boxes/a '() (included-boxes pict x y)))
  
  (define (minimal-boxes/a minimal included)
    (cond
      [(null? included) minimal]
      [else (minimal-boxes/a (adjust-minimal minimal (car included))
                             (cdr included))]))
  
  (define (adjust-minimal minimal candidate)
    (cond
      [(ormap (lambda (x) (inside? x candidate)) minimal)
       minimal]
      [(ormap (lambda (x) (inside? candidate x)) minimal)
       (cons
        candidate
        (filter (lambda (x) (not (inside? candidate x)))
                minimal))]
      [else (cons candidate minimal)]))
       
  (define (inside? c1 c2)
    (and (<= (hbox-x c2)
             (hbox-x c1)
             (+ (hbox-x c1) (hbox-w c1))
             (+ (hbox-x c2) (hbox-w c2)))
         (<= (hbox-y c2)
             (hbox-y c1)
             (+ (hbox-y c1) (hbox-h c1))
             (+ (hbox-y c2) (hbox-h c2)))))
  
  (define (test)
    
    (define (same-boxes? l1 l2)
      (and (list? l1)
           (list? l2)
           (andmap hbox? l1)
           (andmap hbox? l2)
           (andmap (lambda (e1) (memf (same-box? e1) l2)) l1)
           (andmap (lambda (e2) (memf (same-box? e2) l1)) l2)
           #t))
    (define ((same-box? b1) b2)
      (and (= (hbox-x b1) (hbox-x b2))
           (= (hbox-y b1) (hbox-y b2))
           (= (hbox-w b1) (hbox-w b2))
           (= (hbox-h b1) (hbox-h b2))))
    
    (define (chk l1 l2)
      (cond
        [(same-boxes? l1 l2) #t]
        [else
         (printf "got ~s expected ~s\n" (map conv l1) (map conv l2))
         #f]))
    
    (list (chk (minimal-boxes/a '() (list (make-hbox 1 1 10 10)))
               (list (make-hbox 1 1 10 10)))
          (chk (minimal-boxes/a '() (list (make-hbox 5 5 10 10)
                                          (make-hbox 0 0 20 20)))
               (list (make-hbox 5 5 10 10)))
          (chk (minimal-boxes/a '() (list (make-hbox 5 5 10 10)
                                          (make-hbox 6 6 10 10)))
               (list (make-hbox 5 5 10 10)
                     (make-hbox 6 6 10 10)))
          (chk (minimal-boxes/a '() (list (make-hbox 0 0 10 10)
                                          (make-hbox 0 0 20 20)))
               (list (make-hbox 0 0 10 10)))
          (chk (minimal-boxes/a '() (list (make-hbox 0 0 20 20)
                                          (make-hbox 0 0 10 10)))
               (list (make-hbox 0 0 10 10)))))

  (define (conv x)
    (list (hbox-x x) (hbox-y x) (hbox-w x) (hbox-h x)))
  
  
  (define (included-boxes pict x y)
    (filter (lambda (child)
              (and (<= (hbox-x child)
                       x
                       (+ (hbox-x child) (hbox-w child)))
                   (<= (hbox-y child)
                       y
                       (+ (hbox-y child) (hbox-h child)))))
            (all-boxes pict)))
    
  ;; y coordinates is from top going down (not bottom going up, which is what picts do)
  (define-struct hbox (x y w h))
  
  (define (make-hbox/conv orig-pict x y w h)
    (make-hbox x
               (- (pict-height orig-pict) y h)
               w
               h))
  
  ;; all-boxes : pict -> (listof hbox)
  (define (all-boxes orig-pict)
    (let loop ([pict orig-pict]
               [adx 0]
               [ady 0]
               [boxes (list (make-hbox 0 0 (pict-width orig-pict) (pict-height orig-pict)))])
      (let ([children (pict-children pict)])
        (let i-loop ([children children]
                     [boxes boxes])
          (cond
            [(null? children) boxes]
            [else 
             (let ([child (car children)])
               (i-loop (cdr children)
                       (loop (child-pict child)
                             (+ adx (child-dx child))
                             (+ ady (child-dy child))
                             (cons 
                              (make-hbox/conv
                               orig-pict
                               (+ adx (child-dx child))
                               (+ ady (child-dy child))
                               (pict-width (child-pict child))
                               (pict-height (child-pict child)))
                              boxes))))])))))

  #|
  (define (fish color open?)
    (standard-fish 30 20 'left color "black" open?))
  
  (define lunch-fish (fish "orange" #f))
  (define small-fish (fish "blue" #f))
  (define small-aaah-fish (fish "blue" #t))
  
  (define spacer (blank 10 0))
  
  (define fc 6)

  (define meal-fish 
    (build-list (- fc 1) 
                (lambda (i) (launder lunch-fish))))
  (define eating-fish 
    (build-list fc
                (lambda (i) (launder (scale small-fish (sqrt (+ i 1)))))))
  (define eating-aaah-fish 
    (build-list fc
                (lambda (i) (launder (scale small-aaah-fish (sqrt (+ i 1)))))))
  
  (define (on set pict) (if (memq pict set) pict (ghost pict)))

  (define (main-characters active)
    (foldl (lambda (meal-fish eating-fish eating-aaah-fish rest-of-scene)
             (lc-superimpose
              (on active eating-fish)
              (on active eating-aaah-fish)
              (hc-append (on active meal-fish)
                         spacer
                         rest-of-scene)))
           (cc-superimpose (on active (car eating-fish))
                           (on active (car eating-aaah-fish)))
           meal-fish
           (cdr eating-fish)
           (cdr eating-aaah-fish)))

  
  (dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1)))
  
  (define txt (vc-append (text "yXy" 'roman 12) (text "y" 'roman 12)))
  
  (all-boxes txt)
  
  (define t (new text%))
  (send t insert (new pict-snip% (pict (main-characters (append meal-fish eating-fish eating-aaah-fish)))))
  (send t copy #f 0 0 1)
  (send t insert "\n")
  (send t paste)
  (define f (new frame% (label "") (width 600) (height 200)))
  (define ec (new editor-canvas% (editor t) (parent f)))
  (send f show #t)

|#
  )
