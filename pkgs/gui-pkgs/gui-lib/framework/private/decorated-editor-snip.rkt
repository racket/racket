#lang scheme/base

(require scheme/gui/base
         racket/class)

(provide editor-snip:decorated%
         editor-snip:decorated-snipclass%
         editor-snip:decorated-mixin
         editor-snip:decorated<%>)

(require "../preferences.rkt")

(define editor-snip:decorated<%>
  (interface ((class->interface editor-snip%))
    get-corner-bitmap
    get-color
    get-menu
    get-position
    reset-min-sizes))

(define editor-snip:decorated-mixin
  (mixin ((class->interface editor-snip%)) (editor-snip:decorated<%>)
    
    (init [with-border? #t])
    (define draw-border? with-border?)
    
    ;; get-corner-bitmap : -> (union #f (is-a?/c bitmap%))
    ;; returns the bitmap to be shown in the top right corner.
    (define/public (get-corner-bitmap) #f)
    
    ;; get-color : -> (union string (is-a?/c color%))
    (define/public (get-color) (if (preferences:get 'framework:white-on-black?) "white" "black"))
    
    ;; get-menu : -> (union #f (is-a?/c popup-menu%))
    ;; returns the popup menu that should appear
    ;; when clicking in the top part of the snip.
    (define/public (get-menu) #f)
    
    ;; get-position : -> (union 'top-right 'left-top)
    ;; returns the location of the image and the clickable
    ;; region. 'top-right indicates top portion is clickable
    ;; and icon on right. 'left-top means left portion is
    ;; clickable and icon on top.
    (define/public (get-position) 'top-right)
    
    [define/private (get-pen) (send the-pen-list find-or-create-pen (get-color) 1 'solid)]
    [define/private (get-brush) (send the-brush-list find-or-create-brush "BLACK" 'transparent)]
    
    (inherit get-admin)
    (define/override (on-event dc x y editorx editory evt)
      (cond
        [(eq? (send evt get-event-type) 'right-down)
         (let ([sx (- (send evt get-x) x)]
               [sy (- (send evt get-y) y)]
               [bil (box 0)]
               [bit (box 0)]
               [bir (box 0)]
               [bib (box 0)]
               [bw (box 0)]
               [bh (box 0)]
               [bml (box 0)]
               [bmt (box 0)]
               [bmr (box 0)]
               [bmb (box 0)]
               [menu (get-menu)])
           (get-extent dc x y bw bh #f #f #f #f)
           (get-inset bil bit bir bib)
           (get-margin bml bmt bmr bmb)
           (let ([in-range
                  (case (get-position)
                    [(top-right)
                     (and (<= 0 sx (unbox bw))
                          (<= 0 sy (unbox bmt)))]
                    [(left-top)
                     (and (<= 0 sx (unbox bml))
                          (<= 0 sy (unbox bh)))]
                    [else #f])])
             (cond
               [(and menu in-range)
                (let ([admin (get-admin)])
                  (when admin
                    (send admin popup-menu menu this (+ sx 1) (+ sy 1))))]
               [else (super on-event dc x y editorx editory evt)])))]
        [else
         (super on-event dc x y editorx editory evt)]))
    
    (inherit get-extent get-inset)
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (let ([bm (get-corner-bitmap)]
            [bil (box 0)]
            [bit (box 0)]
            [bir (box 0)]
            [bib (box 0)]
            [bw (box 0)]
            [bh (box 0)]
            [bml (box 0)]
            [bmt (box 0)]
            [bmr (box 0)]
            [bmb (box 0)])
        (get-extent dc x y bw bh #f #f #f #f)
        (get-inset bil bit bir bib)
        (get-margin bml bmt bmr bmb)
        (super draw dc x y left top right bottom dx dy draw-caret)
        (let* ([old-pen (send dc get-pen)]
               [old-brush (send dc get-brush)]
               [white-on-black? (preferences:get 'framework:white-on-black?)])
          
          (send dc set-pen (send the-pen-list find-or-create-pen 
                                 (if white-on-black? "black" "white")
                                 1
                                 'transparent))
          (send dc set-brush (send the-brush-list find-or-create-brush 
                                   (if white-on-black? "black" "white")
                                   'solid))
          (case (get-position)
            [(top-right)
             (send dc draw-rectangle 
                   (+ x (unbox bml))
                   (+ y (unbox bit))
                   (max 0 (- (unbox bw) (unbox bml) (unbox bmr)))
                   (- (unbox bmt) (unbox bit)))]
            [(left-top)
             (send dc draw-rectangle 
                   (+ x (unbox bil))
                   (+ y (unbox bmt))
                   (- (unbox bml) (unbox bil))
                   (max 0 (- (unbox bh) (unbox bmt) (unbox bmb))))])
          
          (send dc set-pen (send the-pen-list find-or-create-pen
                                 (if white-on-black? "white" "black")
                                 1
                                 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush 
                                   (if white-on-black? "white" "black")
                                   'solid))
          
          (when bm
            (let ([bm-w (send bm get-width)]
                  [bm-h (send bm get-height)])
              (case (get-position)
                [(top-right)
                 (send dc draw-bitmap
                       bm
                       (+ x (max 0
                                 (- (unbox bw)
                                    (unbox bmr)
                                    bm-w)))
                       ;; leave two pixels above and two below (see super-instantiate below)
                       (+ y (unbox bit) 2))]
                [(left-top)
                 (send dc draw-bitmap
                       bm
                       ;; leave two pixels left and two right (see super-instantiate below)
                       (+ x (unbox bil) 2)
                       (+ y (unbox bmt)))])))
          
          (when draw-border?
            (send dc set-pen (get-pen))
            (send dc set-brush (get-brush))
            (send dc draw-rectangle
                  (+ x (unbox bil))
                  (+ y (unbox bit))
                  (max 0 (- (unbox bw) (unbox bil) (unbox bir)))
                  (max 0 (- (unbox bh) (unbox bit) (unbox bib)))))
          
          (send dc set-pen old-pen)
          (send dc set-brush old-brush))))
    
    (inherit set-min-width set-min-height get-margin)
    (define/public (reset-min-sizes)
      (let ([bm (get-corner-bitmap)])
        (when bm
          (case (get-position)
            [(top-right)
             (set-min-width (+ 4 (send bm get-width)))]
            [(left-top)
             (set-min-height (+ 4 (send bm get-height)))]))))
    
    (let ([top-margin 
           (case (get-position)
             [(top-right)
              (+ 4 
                 (let ([bm (get-corner-bitmap)])
                   (if bm
                       (send bm get-height)
                       0)))]
             [else 4])]
          [left-margin
           (case (get-position)
             [(left-top)
              (+ 4
                 (let ([bm (get-corner-bitmap)])
                   (if bm
                       (send bm get-width)
                       0)))]
             [else 4])])
      (super-new
       (with-border? #f)
       (top-margin top-margin)
       (left-margin left-margin)))
    
    (inherit use-style-background)
    (use-style-background #t)
    
    (reset-min-sizes)))

(define editor-snip:decorated%
  (class (editor-snip:decorated-mixin editor-snip%)
    (inherit get-editor get-style)
    
    ;; make-snip : -> this%
    ;; returns an instance of this class. used in the copy method
    (define/public (make-snip) (make-object editor-snip:decorated%))
    
    ;; make-editor : -> editor<%>
    ;; returns the editor to be used in this snip.
    (define/public (make-editor) (make-object text%))
    
    (define/override write
      (λ (stream-out)
        (send (get-editor) write-to-file stream-out 0 'eof)))
    
    (define/override (copy)
      (let ([snip (make-snip)])
        (send snip set-editor (send (get-editor) copy-self))
        (send snip set-style (get-style))
        snip))
    
    (super-new
     (editor (make-editor)))))

(define editor-snip:decorated-snipclass%
  (class snip-class%
    
    ;; make-snip : stream-in -> (is-a?/c snip%)
    ;; returns an unfilled version of the snip
    (define/public (make-snip stream-in) (make-object editor-snip:decorated%))
    
    (define/override (read stream-in)
      (let ([snip (make-snip stream-in)])
        (send (send snip get-editor) read-from-file stream-in #f)
        snip))
    (super-new)))
