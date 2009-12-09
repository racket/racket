
#lang scheme/base
(require scheme/list
         scheme/class
         scheme/gui
         drscheme/arrow
         framework/framework
         unstable/gui/notify)

(provide text:hover<%>
         text:hover-identifier<%>
         text:mouse-drawings<%>
         text:arrows<%>

         text:hover-mixin
         text:hover-identifier-mixin
         text:mouse-drawings-mixin
         text:tacking-mixin
         text:arrows-mixin)

(define arrow-brush
  (send the-brush-list find-or-create-brush "white" 'solid))
(define (tacked-arrow-brush color)
  (send the-brush-list find-or-create-brush color 'solid))

(define billboard-brush
  (send the-brush-list find-or-create-brush "white" 'solid))

(define white (send the-color-database find-color "white"))

;; A Drawing is (make-drawing number number (??? -> void) boolean boolean)
(define-struct drawing (start end draw visible? tacked?) #:mutable)

(define-struct idloc (start end id))

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

(define text:hover<%>
  (interface (text:basic<%>)
    update-hover-position))

(define text:hover-identifier<%>
  (interface ()
    get-hovered-identifier
    set-hovered-identifier
    listen-hovered-identifier))

(define text:mouse-drawings<%>
  (interface (text:basic<%>)
    add-mouse-drawing
    for-each-drawing
    delete-all-drawings))

(define text:arrows<%>
  (interface (text:mouse-drawings<%>)
    add-arrow
    add-question-arrow
    add-billboard))

(define text:hover-mixin
  (mixin (text:basic<%>) (text:hover<%>)
    (inherit dc-location-to-editor-location
             find-position)

    (define/override (on-default-event ev)
      (define gx (send ev get-x))
      (define gy (send ev get-y))
      (define-values (x y) (dc-location-to-editor-location gx gy))
      (define pos (find-position x y))
      (super on-default-event ev)
      (case (send ev get-event-type)
        ((enter motion leave)
         (update-hover-position pos))))

    (define/public (update-hover-position pos)
      (void))

    (super-new)))

(define text:hover-identifier-mixin
  (mixin (text:hover<%>) (text:hover-identifier<%>)
    (define-notify hovered-identifier (new notify-box% (value #f)))

    (define idlocs null)

    (define/public (add-identifier-location start end id)
      (set! idlocs (cons (make-idloc start end id) idlocs)))

    (define/public (delete-all-identifier-locations)
      (set! idlocs null)
      (set-hovered-identifier #f))

    (define/override (update-hover-position pos)
      (super update-hover-position pos)
      (let search ([idlocs idlocs])
        (cond [(null? idlocs) (set-hovered-identifier #f)]
              [(and (<= (idloc-start (car idlocs)) pos)
                    (< pos (idloc-end (car idlocs))))
               (set-hovered-identifier (idloc-id (car idlocs)))]
              [else (search (cdr idlocs))])))
    (super-new)))

(define text:mouse-drawings-mixin
  (mixin (text:hover<%>) (text:mouse-drawings<%>)
    (inherit dc-location-to-editor-location
             find-position
             invalidate-bitmap-cache)

    ;; list of Drawings
    (field [drawings-list null])

    (define/public add-mouse-drawing
      (case-lambda
        [(start end draw)
         (add-mouse-drawing start end draw (box #f))]
        [(start end draw tack-box)
         (set! drawings-list 
               (cons (make-drawing start end draw #f tack-box)
                     drawings-list))]))

    (define/public (delete-all-drawings)
      (set! drawings-list null))

    (define/public-final (for-each-drawing f)
      (for-each f drawings-list))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (for-each-drawing
         (lambda (d)
           (when (or (drawing-visible? d) (unbox (drawing-tacked? d)))
             ((drawing-draw d) this dc left top right bottom dx dy))))))

    (define/override (update-hover-position pos)
      (super update-hover-position pos)
      (let ([changed? (update-visible-drawings pos)])
        (when changed? (invalidate-bitmap-cache 0.0 0.0 +inf.0 +inf.0))))

    (define/private (update-visible-drawings pos)
      (let ([changed? #f])
        (for-each-drawing
         (lambda (d)
           (let ([vis? (<= (drawing-start d) pos (drawing-end d))])
             (unless (eqv? vis? (drawing-visible? d))
               (set-drawing-visible?! d vis?)
               (set! changed? #t)))))
        changed?))

    (super-new)))

(define text:tacking-mixin
  (mixin (text:basic<%> text:mouse-drawings<%>) ()
    (inherit get-canvas
             for-each-drawing)
    (inherit-field drawings-list)
    (super-new)

    (define/override (on-event ev)
      (case (send ev get-event-type)
        ((right-down)
         (if (ormap (lambda (d) (drawing-visible? d)) drawings-list)
             (send (get-canvas) popup-menu
                   (make-tack/untack-menu)
                   (send ev get-x)
                   (send ev get-y))
             (super on-event ev)))
        (else
         (super on-event ev))))

    (define/private (make-tack/untack-menu)
      (define menu (new popup-menu%))
      (new menu-item% (label "Tack")
           (parent menu)
           (callback
            (lambda _ (tack))))
      (new menu-item% (label "Untack")
           (parent menu)
           (callback
            (lambda _ (untack))))
      menu)

    (define/private (tack)
      (for-each-drawing
       (lambda (d)
         (when (drawing-visible? d)
           (set-box! (drawing-tacked? d) #t)))))
    (define/private (untack)
      (for-each-drawing
       (lambda (d)
         (when (drawing-visible? d)
           (set-box! (drawing-tacked? d) #f)))))))

(define text:arrows-mixin
  (mixin (text:mouse-drawings<%>) (text:arrows<%>)
    (inherit position-location
             add-mouse-drawing
             find-wordbreak)

    (define/public (add-arrow from1 from2 to1 to2 color)
      (internal-add-arrow from1 from2 to1 to2 color #f))

    (define/public (add-question-arrow from1 from2 to1 to2 color)
      (internal-add-arrow from1 from2 to1 to2 color #t))

    (define/public (add-billboard pos1 pos2 str color-name)
      (define color (send the-color-database find-color color-name))
      (let ([draw 
             (lambda (text dc left top right bottom dx dy)
               (let-values ([(x y) (range->mean-loc pos1 pos1)]
                            [(fw fh _d _v) (send dc get-text-extent "y")])
                 (with-saved-pen&brush dc
                   (with-saved-text-config dc
                     (send* dc
                       (set-pen color 1 'solid)
                       (set-brush billboard-brush)
                       (set-text-mode 'solid)
                       (set-font (billboard-font dc))
                       (set-text-foreground color))
                     (let-values ([(w h d v) (send dc get-text-extent str)]
                                  [(adj-y) fh]
                                  [(mini) _d])
                       (send* dc
                         (draw-rounded-rectangle
                          (+ x dx)
                          (+ y dy adj-y)
                          (+ w mini mini)
                          (+ h mini mini))
                         (draw-text str (+ x dx mini) (+ y dy mini adj-y))))))))])
        (add-mouse-drawing pos1 pos2 draw)))

    (define/private (internal-add-arrow from1 from2 to1 to2 color-name question?)
      (define color (send the-color-database find-color color-name))
      (define tack-box (box #f))
      (unless (and (= from1 to1) (= from2 to2))
        (let ([draw 
               (lambda (text dc left top right bottom dx dy)
                 (let-values ([(startx starty) (range->mean-loc from1 from2)]
                              [(endx endy) (range->mean-loc to1 to2)]
                              [(fw fh _d _v) (send dc get-text-extent "x")])
                   (with-saved-pen&brush dc
                     (with-saved-text-config dc
                       (send dc set-pen color 1 'solid)
                       (send dc set-brush
                             (if (unbox tack-box)
                                 (tacked-arrow-brush color)
                                 arrow-brush))
                       (draw-arrow dc startx
                                   (+ starty (/ fh 2))
                                   endx
                                   (+ endy (/ fh 2))
                                   dx dy)
                       (send dc set-text-mode 'transparent)
                       (when question?
                         (send dc set-font (?-font dc))
                         (send dc set-text-foreground color)
                         (send dc draw-text "?" 
                               (+ endx dx fw)
                               (- (+ endy dy) fh)))))))])
          (add-mouse-drawing from1 from2 draw tack-box)
          (add-mouse-drawing to1 to2 draw tack-box))))

    (define/private (position->location p)
      (define xbox (box 0.0))
      (define ybox (box 0.0))
      (position-location p xbox ybox)
      (values (unbox xbox) (unbox ybox)))

    (define/private (?-font dc)
      (let ([size (send (send dc get-font) get-point-size)])
        (send the-font-list find-or-create-font size 'default 'normal 'bold)))

    (define/private (billboard-font dc)
      (let ([size (send (send dc get-font) get-point-size)])
        (send the-font-list find-or-create-font size 'default 'normal)))

    (define/private (range->mean-loc pos1 pos2)
      (let*-values ([(loc1x loc1y) (position->location pos1)]
                    [(loc2x loc2y) (position->location pos2)]
                    [(locx) (mean loc1x loc2x)]
                    [(locy) (mean loc1y loc2y)])
        (values locx locy)))

    (super-new)))

(define text:mouse-drawings%
  (text:mouse-drawings-mixin
   (text:hover-mixin
    text:standard-style-list%)))

(define text:arrows%
  (text:arrows-mixin
   (text:tacking-mixin
    text:mouse-drawings%)))
