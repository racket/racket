
(module text mzscheme
  (require (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "arrow.ss" "drscheme")
           (lib "framework.ss" "framework"))

  (provide text:drawings<%>
           text:mouse-drawings<%>
           text:arrows<%>

           text:drawings-mixin
           text:mouse-drawings-mixin
           text:arrows-mixin)

  (define (mean x y)
    (/ (+ x y) 2))

  (define-syntax with-saved-pen&brush
    (syntax-rules ()
      [(with-saved-pen&brush dc . body)
       (save-pen&brush dc (lambda () . body))]))

  (define (save-pen&brush dc thunk)
    (let ([old-pen (send dc get-pen)]
          [old-brush (send dc get-brush)])
      (begin0 (thunk)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))))

  (define-syntax with-saved-text-config
    (syntax-rules ()
      [(with-saved-text-config dc . body)
       (save-text-config dc (lambda () . body))]))

  (define (save-text-config dc thunk)
    (let ([old-font (send dc get-font)]
          [old-color (send dc get-text-foreground)]
          [old-background (send dc get-text-background)]
          [old-mode (send dc get-text-mode)])
      (begin0 (thunk)
              (send dc set-font old-font)
              (send dc set-text-foreground old-color)
              (send dc set-text-background old-background)
              (send dc set-text-mode old-mode))))

  (define text:drawings<%>
    (interface (text:basic<%>)
      add-drawings
      delete-drawings
      delete-all-drawings))

  (define text:mouse-drawings<%>
    (interface (text:drawings<%>)
      add-mouse-drawing
      delete-mouse-drawings))

  (define text:arrows<%>
    (interface (text:mouse-drawings<%>)
      add-arrow
      add-question-arrow))

  (define text:drawings-mixin
    (mixin (text:basic<%>) (text:drawings<%>)
      (define draw-table (make-hash-table))

      (define/public (add-drawings key draws)
        (hash-table-put! draw-table
                         key
                         (append draws (hash-table-get draw-table key (lambda () null)))))

      (define/public (delete-drawings key)
        (hash-table-remove! draw-table key))

      (define/public (delete-all-drawings)
        (for-each (lambda (key) (hash-table-remove! draw-table key))
                  (hash-table-map draw-table (lambda (k v) k))))

      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (unless before?
          (hash-table-for-each
           draw-table
           (lambda (k v)
             (for-each (lambda (d) (d this dc left top right bottom dx dy))
                       v)))))

      (super-new)))

  ;; A Drawing is (make-drawing number number (??? -> void))
  (define-struct drawing (start end draw) #f)

  (define text:mouse-drawings-mixin
    (mixin (text:drawings<%>) (text:mouse-drawings<%>)
      (inherit dc-location-to-editor-location
               find-position
               invalidate-bitmap-cache
               add-drawings
               delete-drawings)

      ;; lists of Drawings
      (field [inactive-list null]
             [active-list null])
      
      (define/public (add-mouse-drawing start end draw)
        (set! inactive-list 
              (cons (make-drawing start end draw)
                    inactive-list)))
      
      (define/public (delete-mouse-drawings)
        (set! inactive-list null))

      (define/override (delete-all-drawings)
        (super delete-all-drawings)
        (set! inactive-list null)
        (set! active-list null))

      (define/override (on-default-event ev)
        (define gx (send ev get-x))
        (define gy (send ev get-y))
        (define-values (x y) (dc-location-to-editor-location gx gy))
        (define pos (find-position x y))
        (super on-default-event ev)
        (case (send ev get-event-type)
          ((enter motion)
           (let ([new-active-annotations
                  (filter (lambda (rec)
                            (<= (drawing-start rec) pos (drawing-end rec)))
                          inactive-list)])
             (unless (equal? active-list new-active-annotations)
               (set! active-list new-active-annotations)
               (delete-drawings 'mouse-over)
               (add-drawings 'mouse-over (map drawing-draw active-list))
               (invalidate-bitmap-cache))))
          ((leave)
           (unless (null? active-list)
             (set! active-list null)
             (delete-drawings 'mouse-over)
             (invalidate-bitmap-cache)))))

      (super-new)))
  
  (define arrow-brush (send the-brush-list find-or-create-brush "white" 'solid))

  (define text:arrows-mixin
    (mixin (text:mouse-drawings<%>) (text:arrows<%>)
      (inherit position-location
               add-mouse-drawing
               find-wordbreak
               add-drawings
               delete-drawings
               get-canvas)
      (inherit-field active-list inactive-list)

      (define/public (add-arrow from1 from2 to1 to2 color)
        (internal-add-arrow from1 from2 to1 to2 color #f))

      (define/public (add-question-arrow from1 from2 to1 to2 color)
        (internal-add-arrow from1 from2 to1 to2 color #t))

      (define/private (internal-add-arrow from1 from2 to1 to2 color question?)
        (unless (and (= from1 to1) (= from2 to2))
          (let ([draw 
                 (lambda (text dc left top right bottom dx dy)
                   (let*-values ([(start1x start1y) (position->location from1)]
                                 [(start2x start2y) (position->location from2)]
                                 [(end1x end1y) (position->location to1)]
                                 [(end2x end2y) (position->location to2)]
                                 [(startx) (mean start1x start2x)]
                                 [(starty) (mean start1y start2y)]
                                 [(endx) (mean end1x end2x)]
                                 [(endy) (mean end1y end2y)]
                                 [(fw fh _d _v) (send dc get-text-extent "")])
                     (let ([starty (+ starty (/ fh 2))]
                           [endy (+ endy (/ fh 2))])
                       (with-saved-pen&brush dc
                         (with-saved-text-config dc
                           (send dc set-pen color 1 'solid)
                           (send dc set-brush arrow-brush)
                           (draw-arrow dc startx starty endx endy dx dy)
                           #;(send dc set-text-mode 'solid)
                           (when question?
                             (send dc set-font (?-font dc))
                             (send dc set-text-foreground 
                                   (send the-color-database find-color color))
                             (send dc draw-text "?" 
                                   (+ (+ startx dx) fw)
                                   (- (+ starty dy) fh))))))))])
            (add-mouse-drawing from1 from2 draw)
            (add-mouse-drawing to1 to2 draw))))

      (define/private (position->location p)
        (define xbox (box 0.0))
        (define ybox (box 0.0))
        (position-location p xbox ybox)
        (values (unbox xbox) (unbox ybox)))

      (define/override (on-event ev)
        (case (send ev get-event-type)
          ((right-down)
           (let ([arrows active-list])
             (if (pair? arrows)
                 (send (get-canvas) popup-menu
                       (make-tack/untack-menu)
                       (send ev get-x)
                       (send ev get-y))
                 (super on-event ev))))
          (else
           (super on-event ev))))

      (define/private (make-tack/untack-menu)
        (define menu (new popup-menu%))
        (new menu-item% (label "Tack arrows")
             (parent menu)
             (callback
              (lambda _ (tack-arrows))))
        (new menu-item% (label "Untack arrows")
             (parent menu)
             (callback
              (lambda _ (untack-arrows))))
        menu)

      (define/private (tack-arrows)
        (for-each (lambda (arrow) 
                    (add-drawings (drawing-draw arrow) (list (drawing-draw arrow))))
                  active-list))
      (define/private (untack-arrows)
        (for-each (lambda (arrow) (delete-drawings (drawing-draw arrow)))
                  active-list))

      (define/private (?-font dc)
        (let ([size (send (send dc get-font) get-point-size)])
          (send the-font-list find-or-create-font size 'default 'normal 'bold)))

      (super-new)))

  (define text:mouse-drawings%
    (text:mouse-drawings-mixin
     (text:drawings-mixin text:standard-style-list%)))

  (define text:arrows%
    (text:arrows-mixin text:mouse-drawings%))

  #;
  (begin
    (define f (new frame% (label "testing") (width 100) (height 100)))
    (define t (new text:crazy% (auto-wrap #t)))
    (define ec (new editor-canvas% (parent f) (editor t)))
    (send f show #t)
    (send t insert "this is the time to remember, because it will not last forever\n")
    (send t insert "these are the days to hold on to, but we won't although we'll want to\n")
    
    (send t add-dot 5)
    (send t add-arrow 25 8 "blue"))
  
  )
