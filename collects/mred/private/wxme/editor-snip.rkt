#lang racket/base
(require racket/class
         "../syntax.rkt"
         "private.rkt"
         racket/snip/private/private
         "const.rkt"
         racket/snip/private/snip
         racket/snip/private/snip-admin
         racket/snip/private/snip-flags
         "standard-snip-admin.rkt"
         "editor.rkt"
         "editor-admin.rkt"
         "editor-snip-class.rkt"
         "text.rkt"
         "pasteboard.rkt"
         "wx.rkt"
         (except-in "cycle.rkt"
                    text%
                    pasteboard%
                    editor-snip%
                    editor-snip-editor-admin%))

(provide editor-snip%
         editor-snip-editor-admin<%>)

;; FIXME: use "type"s
(define-syntax-rule (private-inits [[type id] val] ...)
  (begin
    (define-init id val)
    ...))
(define-syntax-rule (define-init id v)
  (begin
    (init [(init-tmp id) v])
    (define id init-tmp)))

;; see also "private.rkt"
(define-local-member-name
  with-dc
  do-get-left-margin do-get-right-margin do-get-bottom-margin do-get-top-margin
  do-get-extent)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass editor-snip% snip%
  (private-inits
   [[(make-or-false editor<%>) editor] #f]
   [[bool? with-border?] #t]
   [[exact-nonnegative-integer? left-margin] 5]
   [[exact-nonnegative-integer? top-margin] 5]
   [[exact-nonnegative-integer? right-margin] 5]
   [[exact-nonnegative-integer? bottom-margin] 5]
   [[exact-nonnegative-integer? left-inset] 1]
   [[exact-nonnegative-integer? top-inset] 1]
   [[exact-nonnegative-integer? right-inset] 1]
   [[exact-nonnegative-integer? bottom-inset] 1]
   [[(make-alts (symbol-in none) nonnegative-real?) min-width] 'none]
   [[(make-alts (symbol-in none) nonnegative-real?) max-width] 'none]
   [[(make-alts (symbol-in none) nonnegative-real?) min-height] 'none]
   [[(make-alts (symbol-in none) nonnegative-real?) max-height] 'none])

  (unless (symbol? min-width) (set! min-width (exact->inexact min-width)))
  (unless (symbol? max-width) (set! max-width (exact->inexact max-width)))
  (unless (symbol? min-height) (set! min-height (exact->inexact min-height)))
  (unless (symbol? max-height) (set! max-height (exact->inexact max-height)))

  (define align-top-line? #f)
  (define tight-fit? #f)
  (define use-style-bg? #f)

  (super-new)

  (inherit set-snipclass
           do-copy-to)
  (inherit-field s-admin
                 s-flags
                 s-style)

  (set-snipclass the-editor-snip-class)

  (when (and editor (send editor get-admin))
    (set! editor #f))
  (unless editor
    (set! editor (new extended-text%)))

  (define my-admin (new editor-snip-editor-admin% [owner this]))

  (set! s-flags (add-flag s-flags HANDLES-EVENTS))
  (when (no-permanent-filename? editor)
    (set! s-flags (add-flag s-flags USES-BUFFER-PATH)))

  (send editor own-caret #f)

  ;; ----------------------------------------

  (define/private (no-permanent-filename? editor)
    (let ([temp (box #f)])
      (let ([fn (send editor get-filename temp)])
        (or (not fn) (unbox temp)))))

  (def/override (set-admin [(make-or-false snip-admin%) a])

    (when (not (eq? a s-admin))
      (super set-admin a)
      (when editor
        (if a
            (begin
              (when (send editor get-admin)
                ;; traitor! - get rid of it
                (set! editor #f))
              (send editor set-admin my-admin))
            (send editor set-admin #f))))

    (when (and s-admin
               (has-flag? s-flags USES-BUFFER-PATH))
      ;; propagate a filename change:
      (if (and editor
               (no-permanent-filename? editor))
          (let ([b (send s-admin get-editor)])
            (when b
              (let ([fn (send b get-filename)])
                (when fn
                  (send editor set-filename fn #t)))))
          (set! s-flags (remove-flag s-flags USES-BUFFER-PATH)))) ;; turn off the flag; not needed
  
    (void))
  
  (def/public (set-editor [editor<%> b])
    (unless (eq? editor b)
      (when (and editor s-admin)
        (send editor set-admin #f))
      (set! editor b)
      (when b
        (cond
         [(send b get-admin)
          (set! editor #f)]
         [s-admin
          (send editor set-admin my-admin)]))
      (when s-admin
        (send s-admin resized this #t))))

  (def/public (get-editor)
    editor)

  (def/override (adjust-cursor [dc<%> dc] [real? x] [real? y] [real? ex] [real? ey] [mouse-event% event])
    (if (not editor)
        #f
        (send my-admin
              with-dc
              dc x y
              (lambda ()
                (send editor adjust-cursor event)))))

  (def/override (on-event [dc<%> dc] [real? x] [real? y] [real? ex] [real? ey] [mouse-event% event])
    (when editor
      (send my-admin
            with-dc
            dc x y
            (lambda ()
              (send editor on-event event)))))
  
  (def/override (on-char [dc<%> dc] [real? x] [real? y] [real? ex] [real? ey] [key-event% event])
    (when editor
      (send my-admin
            with-dc
            dc x y
            (lambda ()
              (send editor on-char event)))))

  (def/override (own-caret [bool? own?])
    (when editor
      (send editor own-caret own?)))

  (def/override (blink-caret [dc<%> dc] [real? x] [real? y])
    (when editor
      (send my-admin
            with-dc
            dc x y
            (lambda ()
              (send editor blink-caret)))))

  (def/override (do-edit-operation [symbol? op] [any? [recur? #t]] [exact-integer? [timestamp 0]])
    (when editor
      (send editor do-edit-operation op recur? timestamp)))

  (def/override (can-do-edit-operation? [symbol? op] [any? [recur? #t]])
    (and editor
         (send editor can-do-edit-operation? op recur?)))

  (def/override (match? [snip% s])
    #f)

  (def/override (size-cache-invalid)
    (when editor
      (send editor size-cache-invalid)))

  (def/override (get-text [exact-nonnegative-integer? offset] [exact-integer? num] 
                          [any? [flattened? #f]])
    (cond
     [(or (offset . >= . 1)
          (zero? num))
      ""]
     [(not flattened?)
      "."]
     [editor
      (send editor get-flattened-text)]
     [else ""]))

  (define/public (do-get-extent dc x y w h -descent -space lspace rspace)
    (send my-admin
          with-dc
          dc x y
          (lambda ()
            (let ([h2 (or h (box 0.0))])
              (if editor
                  (send editor get-extent w h2)
                  (begin
                    (when w (set-box! w 0.0))
                    (set-box! h2 0.0)))
              (let ([orig-h (if align-top-line?
                                (unbox h2)
                                0.0)])
                
                (when w
                  (when (editor . is-a? . text%)
                    (set-box!
                     w
                     (- (unbox w)
                        (if tight-fit?
                            CURSOR-WIDTH
                            1)))) ;; it still looks better to subtract 1
                  (when ((unbox w) . < . (if (symbol? min-width) -inf.0 min-width))
                    (set-box! w min-width))
                  (when ((unbox w) . > . (if (symbol? max-width) +inf.0 max-width))
                    (set-box! w max-width))
                  (set-box! w (+ (unbox w) (+ right-margin left-margin))))

                (when h
                  (when (editor . is-a? . text%)
                    (when tight-fit?
                      (set-box! h
                                (max 0.0
                                     (- (unbox h)
                                        (send editor get-line-spacing))))))
                  (when ((unbox h) . < . (if (symbol? min-height) -inf.0 min-height))
                    (set-box! h min-height))
                  (when ((unbox h) . > . (if (symbol? max-height) +inf.0 max-height))
                    (set-box! h max-height))
                  (set-box! h (+ (unbox h) (+ top-margin bottom-margin))))

                (let* ([descent (+ (if editor
                                       (send editor get-descent)
                                       0.0)
                                   bottom-margin)]
                       [descent
                        (if (editor . is-a? . text%)
                            (let ([descent (if align-top-line?
                                               (- orig-h
                                                  (+ (send editor get-top-line-base)
                                                     bottom-margin))
                                               descent)])
                              (if tight-fit?
                                  (max (- descent (send editor get-line-spacing)) 0.0)
                                  descent))
                            descent)]
                       [space (+ (if editor 
                                     (send editor get-space)
                                     0.0)
                                 top-margin)])
                  (let-values ([(space descent)
                                (if (and (not (symbol? max-height))
                                         ((+ descent space) . >= . (+ max-height top-margin bottom-margin)))
                                    ;; just give up on spaces in this case:
                                    (values top-margin bottom-margin)
                                    (values space descent))])
                    (when -descent (set-box! -descent descent))
                    (when -space (set-box! -space space))))

                (when lspace (set-box! lspace left-margin))
                (when rspace (set-box! rspace right-margin)))))))

  (def/override (get-extent [dc<%> dc] [real? x] [real? y] 
                            [maybe-box? [w #f]] [maybe-box? [h #f]]
                            [maybe-box? [-descent #f]] [maybe-box? [-space #f]]
                            [maybe-box? [lspace #f]] [maybe-box? [rspace #f]])
    (do-get-extent dc x y w h -descent -space lspace rspace))

  (def/override (draw [dc<%> dc] [real? x] [real? y] 
                      [real? left] [real? top] [real? right] [real? bottom] 
                      [real? dx] [real? dy] [caret-status? caret])
    (send my-admin
          with-dc
          dc x y
          (lambda ()
            (let-boxes ([w 0.0]
                        [h 0.0])
                (when editor
                  (send editor get-extent w h)
                  (when (editor . is-a? . text%)
                    (set-box! w (max 0.0
                                     (- (unbox w) 
                                        (if tight-fit?
                                            CURSOR-WIDTH
                                            1)))) ;; it still looks better to subtract 1
                    (when tight-fit?
                      (set-box! h (max 0.0
                                       (- (unbox h)
                                          (send editor get-line-spacing)))))))
              (let* ([w (min (max w (if (symbol? min-width) -inf.0 min-width))
                             (if (symbol? max-width) +inf.0 max-width))]
                     [h (min (max h (if (symbol? min-height) -inf.0 min-height))
                             (if (symbol? max-height) +inf.0 max-height))]
                     [orig-x x]
                     [orig-y y]
                     [x (+ x left-margin)]
                     [y (+ y top-margin)]
                     [r (+ x w)]
                     [b (+ y h)]
                     [l (max x left)]
                     [t (max y top)]
                     [r (min r right)]
                     [b (min b bottom)])

                (let ([bg-color
                       (cond
                        [(pair? caret) #f]
                        [(not use-style-bg?)
                         (make-object color% 255 255 255)]
                        [(send s-style get-transparent-text-backing)
                         #f]
                        [else
                         (let ([bg-color (send s-style get-background)])
                           (let ([l (+ orig-x left-inset)]
                                 [t (+ orig-y top-inset)]
                                 [r (+ l w left-margin right-margin 
                                       (- (+ left-inset right-inset))
                                       -1)]
                                 [b (+ t h top-margin bottom-margin 
                                       (- (+ top-inset bottom-inset))
                                       -1)])
                             (let ([trans-pen (send the-pen-list
                                                    find-or-create-pen
                                                    bg-color 0 'transparent)]
                                   [fill (send the-brush-list
                                               find-or-create-brush
                                               bg-color 'solid)]
                                   [savep (send dc get-pen)]
                                   [saveb (send dc get-brush)])
                               (send dc set-pen trans-pen)
                               (send dc set-brush fill)

                               (send dc draw-rectangle l t (- r l) (- b t))

                               (send dc set-brush saveb)
                               (send dc set-pen savep)))
                           bg-color)])])

                  (when editor
                    (send editor refresh
                          (- l x) (- t y) (max 0.0 (- r l)) (max 0.0 (- b t))
                          caret bg-color))

                  (when with-border?
                    (let ([pen (send dc get-pen)])
                      (when (and (pair? caret)
                                 (send s-admin get-selected-text-color))
                        (send dc set-pen (send s-admin get-selected-text-color) 1 'solid))
                      (let* ([l (+ orig-x left-inset)]
                             [t (+ orig-y top-inset)]
                             [r (+ l w left-margin right-margin 
                                   (- (+ left-inset right-inset))
                                   -1)]
                             [b (+ t h top-margin bottom-margin 
                                   (- (+ top-inset bottom-inset))
                                   -1)])
                        (let ([ml (max (min l right) left)]
                              [mr (max (min r right) left)]
                              [mt (max (min t bottom) top)]
                              [mb (max (min b bottom) top)])
                          (when (and (l . >= . left)
                                     (l . < . right)
                                     (mt . < . mb))
                            (send dc draw-line l mt l mb))
                          (when (and (r . >= . left)
                                     (r . < . right)
                                     (mt . < . mb))
                            (send dc draw-line r mt r mb))
                          (when (and (t . >= . top)
                                     (t . < . bottom)
                                     (ml . < . mr))
                            (send dc draw-line ml t mr t))
                          (when (and (b . >= . top)
                                     (b . < . bottom)
                                     (ml . < . mr))
                            (send dc draw-line ml b mr b))))
                      (when (pair? caret)
                        (send dc set-pen pen))))))))))

  (def/override (copy)
    (let* ([mb (and editor
                    (send editor copy-self))]
           [ms (make-object extended-editor-snip%
                            mb
                            with-border?
                            left-margin top-margin
                            right-margin bottom-margin
                            left-inset top-inset
                            right-inset bottom-inset
                            min-width max-width
                            min-height max-height)])
      (do-copy-to ms)

      (send ms do-set-graphics tight-fit? align-top-line? use-style-bg?)
      (when (not editor)
        (send ms set-editor #f))
      ms))

  (define/public (do-set-graphics tf? atl? usb?)
    (set! tight-fit? tf?)
    (set! align-top-line? atl?)
    (set! use-style-bg? usb?))

  (def/override (write [editor-stream-out% f])
    (send f put (if editor
                    (if (editor . is-a? . pasteboard%) 2 1)
                    0))
    (send f put (if with-border? 1 0))
    (send f put left-margin)
    (send f put top-margin)
    (send f put right-margin)
    (send f put bottom-margin)
    (send f put left-inset)
    (send f put top-inset)
    (send f put right-inset)
    (send f put bottom-inset)
    (send f put (if (symbol? min-width) -1.0 min-width))
    (send f put (if (symbol? max-width) -1.0 max-width))
    (send f put (if (symbol? min-height) -1.0 min-height))
    (send f put (if (symbol? max-height) -1.0 max-height))
    (send f put (if tight-fit? 1 0))
    (send f put (if align-top-line? 1 0))
    (send f put (if use-style-bg? 1 0))
    (when editor
      (send editor write-to-file f)))

  (define/private (resize-me)
    (when s-admin (send s-admin resized this #t)))

  (def/public (set-max-width [(make-alts (symbol-in none) nonnegative-real?) w])
    (set! max-width w)
    (resize-me))

  (def/public (set-min-width [(make-alts (symbol-in none) nonnegative-real?) w])
    (set! min-width w)
    (resize-me))

  (def/public (set-max-height [(make-alts (symbol-in none) nonnegative-real?) h])
    (set! max-height h)
    (resize-me))

  (def/public (set-min-height [(make-alts (symbol-in none) nonnegative-real?) h])
    (set! min-height h)
    (resize-me))

  (def/public (get-max-width) max-width)
  (def/public (get-min-width) min-width)
  (def/public (get-max-height) max-height)
  (def/public (get-min-height) min-height)

  (def/public (get-tight-text-fit)
    tight-fit?)
  (def/public (set-tight-text-fit [bool? t])
    (set! tight-fit? t)
    (resize-me))

  (def/public (get-align-top-line)
    align-top-line?)
  (def/public (set-align-top-line [bool? t])
    (set! align-top-line? t)
    (resize-me))

  (def/public (style-background-used?)
    use-style-bg?)
  (def/public (use-style-background [bool? u])
    (unless (eq? use-style-bg? u)
      (set! use-style-bg? u)
      (request-refresh)))

  (def/override (resize [real? w] [real? h])
    (let ([w (max 0.0 (- w (+ left-margin right-margin)))]
          [h (max 0.0 (- h (+ top-margin bottom-margin)))])
      (set! min-width w)
      (set! max-width w)
      (set! min-height h)
      (set! max-height h)

      (when editor
        (send editor set-max-width w)
        (send editor set-min-width w))

      (resize-me)
      #t))

  (define/private (request-refresh)
    (when s-admin
      (let ([dc (send s-admin get-dc)])
        (when dc
          (let-boxes ([w 0.0]
                      [h 0.0])
              (get-extent dc 0 0 w h)
            (send s-admin needs-update
                  this left-inset top-inset
                  (+ w (- right-margin right-inset))
                  (+ h (- bottom-margin bottom-inset))))))))

  (def/public (show-border [bool? show])
    (unless (eq? with-border? show)
      (set! with-border? show)
      (request-refresh)))
  (def/public (border-visible?)
    with-border?)

  (def/public (set-margin [exact-nonnegative-integer? lm]
                          [exact-nonnegative-integer? tm]
                          [exact-nonnegative-integer? rm]
                          [exact-nonnegative-integer? bm])
    (set! left-margin lm)
    (set! top-margin tm)
    (set! right-margin rm)
    (set! bottom-margin bm)
    (resize-me))

  (def/public (get-margin [box? lm] [box? tm] [box? rm] [box? bm])
    (set-box! lm left-margin)
    (set-box! tm top-margin)
    (set-box! rm right-margin)
    (set-box! bm bottom-margin))

  (def/public (set-inset [exact-nonnegative-integer? lm]
                         [exact-nonnegative-integer? tm]
                         [exact-nonnegative-integer? rm]
                         [exact-nonnegative-integer? bm])
    (set! left-margin lm)
    (set! top-margin tm)
    (set! right-margin rm)
    (set! bottom-margin bm)
    (request-refresh))

  (def/public (get-inset [box? lm] [box? tm] [box? rm] [box? bm])
    (set-box! lm left-inset)
    (set-box! tm top-inset)
    (set-box! rm right-inset)
    (set-box! bm bottom-inset))

  (def/override (get-num-scroll-steps)
    (if editor
        (if (send editor locked-for-read?)
            1
            (send editor num-scroll-lines))
        1))

  (def/override (find-scroll-step [real? y])
    (if editor
        (if (send editor locked-for-read?)
            0
            (send editor find-scroll-line (- y top-margin)))
        0))

  (def/override (get-scroll-step-offset [exact-integer? n])
    (if editor
        (if (send editor locked-for-read?)
            0
            (+ (send editor scroll-line-location n) top-margin))
        0))

  (def/override (set-unmodified)
    (when editor
      (send editor set-modified #f)))

  (def/public (do-get-left-margin) left-margin)
  (def/public (do-get-right-margin) right-margin)
  (def/public (do-get-bottom-margin) bottom-margin)
  (def/public (do-get-top-margin) top-margin))

(set-editor-snip%! editor-snip%)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct state (dc x y))

(defclass editor-snip-editor-admin% editor-admin%
  (init owner)
  (define snip owner)
  (define state #f)

  (super-new)

  (define/public (get-snip) snip)

  (define/public (with-dc dc x y thunk)
    (let* ([other (make-state dc 
                              (+ x (send snip do-get-left-margin))
                              (+ y (send snip do-get-top-margin)))]
           [swap (lambda ()
                   (let ([s state])
                     (set! state other)
                     (set! other s)))])
      (dynamic-wind swap thunk swap)))
  
  (def/override (get-dc [maybe-box? [x #f]] [maybe-box? [y #f]])
    (let-values ([(xv yv)
                  (if state
                      (values (- (state-x state))
                              (- (state-y state)))
                      (values 0 0))])
      (when x (set-box! x xv))
      (when y (set-box! y yv))
      (if state
          (state-dc state)
          (let ([sadmin (send snip get-admin)])
            (if sadmin
                (send sadmin get-dc)
                #f)))))

  (def/override (get-view [maybe-box? x] [maybe-box? y]
                          [maybe-box? w] [maybe-box? h]
                          [any? [full? #f]])
    (let ([sadmin (send snip get-admin)])
      (cond
       [(not sadmin)
        (when x (set-box! x 0.0))
        (when y (set-box! y 0.0))
        (when w (set-box! w 0.0))
        (when h (set-box! h 0.0))]
       [full?
        (send sadmin get-view x y w h #f)]
       [else
        (let-boxes ([sx 0.0]
                    [sy 0.0]
                    [sw 0.0]
                    [sh 0.0])
            (send sadmin get-view sx sy sw sh snip)
          (when x
            (set-box! x (max 0.0 (- sx (send snip do-get-left-margin)))))
          (when y
            (set-box! y (max 0.0 (- sy (send snip do-get-top-margin)))))
          (when (or w h)
            (if (or (positive? sw) (positive? sh))
                ;; w and h might be too big due to margins - but
                ;; they might be small enough already because 
                ;; part of the snip itself is not viewed
                (let-boxes ([rw 0.0]
                            [rh 0.0])
                    ;; we want the internal, non-overridden method:
                    (send snip do-get-extent (and state (state-dc state)) 0 0 rw rh #f #f #f #f)
                  
                  ;; remember: sx and sy are in snip coordinates

                  (when w
                    (let* ([left-margin (max 0.0 (- (send snip do-get-left-margin) sx))]
                           [sw (- sw left-margin)]
                           [rw (- rw (send snip do-get-left-margin))]
                           [right-margin (max 0.0 (- (send snip do-get-right-margin) (- rw sw)))]
                           [sw (max 0.0 (- sw right-margin))])
                      (set-box! w sw)))

                  (when h
                    (let* ([top-margin (max 0.0 (- (send snip do-get-top-margin) sy))]
                           [sh (- sh top-margin)]
                           [rh (- rh (send snip do-get-top-margin))]
                           [bottom-margin (max 0.0 (- (send snip do-get-bottom-margin) (- rh sh)))]
                           [sh (max 0.0 (- sh bottom-margin))])
                      (set-box! h sh))))
                
                (begin
                  (when w (set-box! w 0.0))
                  (when h (set-box! h 0.0))))))])))
  
  (def/override (scroll-to [real? localx] [real? localy] [real? w] [real? h] [any? [refresh? #t]] 
                           [(symbol-in start none end) [bias 'none]])
    (let ([sadmin (send snip get-admin)])
      (and sadmin
           (send sadmin scroll-to snip (+ localx (send snip do-get-left-margin))
                 (+ localy (send snip do-get-top-margin))
                 w h refresh? bias))))
  
  (def/override (grab-caret [(symbol-in immediate display global) dist])
    (let ([sadmin (send snip get-admin)])
      (when sadmin
        (send sadmin set-caret-owner snip dist))))

  (def/override (resized [any? redraw-now])
    (let ([sadmin (send snip get-admin)])
      (when sadmin
        (send sadmin resized snip redraw-now))))

  (def/override (needs-update [real? localx] [real? localy]
                              [nonnegative-real? w] [nonnegative-real? h])
    (let ([sadmin (send snip get-admin)])
      (when sadmin
        (send sadmin needs-update snip
              (+ localx (send snip do-get-left-margin))
              (+ localy (send snip do-get-top-margin))
              w h))))

  (def/override (update-cursor)
    (let ([sadmin (send snip get-admin)])
      (when sadmin
        (send sadmin update-cursor))))

  (def/override (popup-menu [popup-menu% m] [real? x] [real? y])
    (let ([sadmin (send snip get-admin)])
      (and sadmin
           (send sadmin popup-menu m snip
                 (+ x (send snip do-get-left-margin))
                 (+ y (send snip do-get-top-margin))))))
  
  (def/override (refresh-delayed?)
    (let ([sadmin (send snip get-admin)])
      (or (not sadmin)
          (and (sadmin . is-a? . standard-snip-admin%)
               (send (send sadmin get-editor) refresh-delayed?)))))

  (def/override (modified [any? mod?])
    (let ([sadmin (send snip get-admin)])
      (when sadmin
        (send sadmin modified snip mod?)))))

(set-editor-snip-editor-admin%! editor-snip-editor-admin%)

(define editor-snip-editor-admin<%> (class->interface editor-snip-editor-admin%))

