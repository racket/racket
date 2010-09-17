#lang racket/base
(require racket/list
         racket/class
         racket/gui/base
         drracket/arrow
         framework/framework
         data/interval-map
         "interfaces.rkt")

(provide text:hover<%>
         text:hover-drawings<%>
         text:arrows<%>

         text:hover-mixin
         text:hover-drawings-mixin
         text:tacking-mixin
         text:arrows-mixin)

(define arrow-brush
  (send the-brush-list find-or-create-brush "white" 'solid))
(define (tacked-arrow-brush color)
  (send the-brush-list find-or-create-brush color 'solid))

(define billboard-brush
  (send the-brush-list find-or-create-brush "white" 'solid))

(define white (send the-color-database find-color "white"))

;; A Drawing is (make-drawing number number (??? -> void) (box boolean))
(define-struct drawing (start end draw tacked?))

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

(define text:hover-drawings<%>
  (interface (text:basic<%>)
    add-hover-drawing
    get-position-drawings
    delete-all-drawings))

(define text:arrows<%>
  (interface (text:hover-drawings<%>)
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

(define text:hover-drawings-mixin
  (mixin (text:hover<%>) (text:hover-drawings<%>)
    (inherit dc-location-to-editor-location
             find-position
             invalidate-bitmap-cache)

    ;; interval-map of Drawings
    (define drawings-list (make-interval-map))

    (field [hover-position #f])

    (define/override (update-hover-position pos)
      (define old-pos hover-position)
      (super update-hover-position pos)
      (set! hover-position pos)
      (unless (same-drawings? old-pos pos)
        (invalidate-bitmap-cache 0.0 0.0 +inf.0 +inf.0)))

    (define/public (add-hover-drawing start end draw [tack-box (box #f)])
      (let ([drawing (make-drawing start end draw tack-box)])
        (interval-map-cons*! drawings-list
                             start (add1 end)
                             drawing
                             null)))

    (define/public (delete-all-drawings)
      (interval-map-remove! drawings-list -inf.0 +inf.0))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (for ([d (get-position-drawings hover-position)])
          ((drawing-draw d) this dc left top right bottom dx dy))))

    (define/public (get-position-drawings pos)
      (if pos (interval-map-ref drawings-list pos null) null))

    (define/private (same-drawings? old-pos pos)
      ;; relies on order drawings added & list-of-eq?-struct equality
      (equal? (get-position-drawings old-pos)
              (get-position-drawings pos)))

    (super-new)))

(define text:tacking-mixin
  (mixin (text:basic<%> text:hover-drawings<%>) ()
    (inherit get-canvas
             get-keymap
             get-position-drawings)
    (inherit-field hover-position)
    (super-new)

    (define tacked-table (make-hasheq))

    (define/override (on-event ev)
      (case (send ev get-event-type)
        ((right-down)
         (if (pair? (get-position-drawings hover-position))
             (send (get-canvas) popup-menu
                   (make-tack/untack-menu)
                   (send ev get-x)
                   (send ev get-y))
             (super on-event ev)))
        (else
         (super on-event ev))))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (for ([draw (in-hash-keys tacked-table)])
          (draw this dc left top right bottom dx dy))))

    (define/private (make-tack/untack-menu)
      (define menu (new popup-menu%))
      (define keymap (get-keymap))
      (new menu-item% (label "Tack")
           (parent menu)
           (callback (lambda _ (tack))))
      (new menu-item% (label "Untack")
           (parent menu)
           (callback (lambda _ (untack))))
      (when (is-a? keymap keymap/popup<%>)
        (new separator-menu-item% (parent menu))
        (send keymap add-context-menu-items menu))
      menu)

    (define/private (tack)
      (for ([d (get-position-drawings hover-position)])
        (hash-set! tacked-table (drawing-draw d) #t)
        (set-box! (drawing-tacked? d) #t)))
    (define/private (untack)
      (for ([d (get-position-drawings hover-position)])
        (hash-remove! tacked-table (drawing-draw d))
        (set-box! (drawing-tacked? d) #f)))))

(define text:arrows-mixin
  (mixin (text:hover-drawings<%>) (text:arrows<%>)
    (inherit position-location
             add-hover-drawing
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
        (add-hover-drawing pos1 pos2 draw)))

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
          (add-hover-drawing from1 from2 draw tack-box)
          (add-hover-drawing to1 to2 draw tack-box))))

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

(define text:hover-drawings%
  (text:hover-drawings-mixin
   (text:hover-mixin
    text:standard-style-list%)))

(define text:arrows%
  (text:arrows-mixin
   (text:tacking-mixin
    text:hover-drawings%)))


#|
(define text:hover-identifier<%>
  (interface ()
    get-hovered-identifier
    set-hovered-identifier
    listen-hovered-identifier))

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
|#
