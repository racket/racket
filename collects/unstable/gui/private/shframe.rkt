#lang racket/base
(require racket/math
         racket/class
         racket/draw
         slideshow/pict
         "blur.rkt")
(provide shadow-frame
         arch)

;; ============================================================
;; Boxes with Keynote-style shadows

(define (shadow-frame #:background-color [background-color "white"]
                      #:frame-color [frame-color "gray"]
                      #:frame-line-width [frame-line-width 0]
                      #:shadow-side-length [s-side-len 4.0]
                      #:shadow-top-y-offset [s-top-dy 10.0]
                      #:shadow-bottom-y-offset [s-bot-dy 4.0]
                      #:shadow-descent [s-desc 40.0]
                      #:shadow-alpha-factor [s-alpha 3/4]
                      #:blur [blur-radius 20]
                      #:margin [margin-len 20]
                      #:sep [sep 5]
                      . picts)
  ;; shadow-alpha-factor:
  ;;   - default 3/4 good for a heavy shadow, if blur is enabled
  ;;   - about 1/4 or 1/5 good for light shadow w/o blur
  (let* ([pict (apply vl-append sep picts)]
         [pict (inset pict margin-len)]
         [w (pict-width pict)]
         [h (pict-height pict)]
         [main-box (frame (colorize (filled-rectangle w h) background-color)
                          #:color frame-color #:line-width frame-line-width)]
         [w* (+ w s-side-len s-side-len)]
         [shadow (arch w* w* (+ h (- s-bot-dy s-top-dy)) s-desc)]
         [shadow (brush/linear-gradient
                  shadow
                  (mk-shadow-grad-stops w* s-side-len s-alpha))]
         [shadow
          (cond [(zero? blur-radius) shadow]
                [#t ;; use-smart-blur?
                 (smart-blur shadow w h blur-radius
                             s-side-len s-top-dy s-bot-dy s-desc)]
                [else (blur shadow blur-radius)])]
         [result
          (pin-under (cc-superimpose main-box pict)
                     (- s-side-len) s-top-dy
                     shadow)]
         [result
          (inset result s-side-len 0
                 s-side-len (+ s-desc (- s-top-dy s-bot-dy)))])
    (inset result blur-radius)))

;; smart-blur: blur only visible edges
(define (smart-blur shadow w0 h0 blur-radius s-side-len s-top-dy s-bot-dy s-desc)
  (define (blur-part p x1 y1 x2 y2 lpad tpad rpad bpad)
    (let* ([p (viewport p (- x1 lpad) (- y1 tpad) (+ x2 rpad) (+ y2 bpad))]
           [p (blur p blur-radius #:pre-inset? #f)]
           [p (clip (inset p (- lpad) (- tpad) (- rpad) (- bpad)))])
      p))
  (define (viewport p x1 y1 x2 y2)
    (clip (pin-over (blank (- x2 x1) (- y2 y1)) (- x1) (- y1) p)))
  (let* ([shadow* (inset shadow blur-radius)]
         [w* (pict-width shadow*)]
         [h* (pict-height shadow*)]
         [BR blur-radius]

         [yTopBot (+ BR (- s-top-dy))]
         [yMidBot (+ yTopBot h0)]
         [xLeftRight (+ BR s-side-len)]

         [top-part
          (blur-part shadow*
                     0 0 w* yTopBot
                     0 0 0 BR)]
         [left-part
          (blur-part shadow*
                     0 yTopBot xLeftRight yMidBot
                     0 BR BR BR)]
         [right-part
          (blur-part shadow*
                     (- w* xLeftRight) yTopBot w* yMidBot
                     BR BR 0 BR)]
         [bot-part
          (blur-part shadow*
                     0 yMidBot w* h*
                     0 BR 0 0)]

         [result (blank w* h*)]
         [result (pin-over result 0 0 top-part)]
         [result (pin-over result 0 yTopBot left-part)]
         [result (pin-over result (- w* xLeftRight) yTopBot right-part)]
         [result (pin-over result 0 yMidBot bot-part)])
    (inset result (- blur-radius))))

(define (mk-shadow-grad-stops w s-side-len s-alpha)
  (let* ([epsA (/ s-side-len w)]
         [epsZ (- 1.0 epsA)]
         [alphaA (max 0 (min 1 (* s-alpha 0.16)))]
         [alphaB (max 0 (min 1 (* s-alpha 0.25)))]
         [alphaC (max 0 (min 1 (* s-alpha 1.00)))])
    (list (list 0.00 (make-object color% 0 0 0 alphaA))
          (list epsA (make-object color% 0 0 0 alphaB))
          (list 0.25 (make-object color% 0 0 0 alphaC))
          (list 0.75 (make-object color% 0 0 0 alphaC))
          (list epsZ (make-object color% 0 0 0 alphaB))
          (list 1.00 (make-object color% 0 0 0 alphaA)))))

;; ----

(define (arch outer-w inner-w solid-h leg-h)
  (dc (lambda (dc X Y)
        (draw-arch dc X Y outer-w inner-w solid-h leg-h))
      outer-w (+ solid-h leg-h)))

(define (draw-arch dc X Y outer-w inner-w solid-h leg-h)
  (cond [(zero? leg-h)
         (send dc draw-rectangle X Y outer-w solid-h)]
        [else
         (let ([path (new dc-path%)])
           (dc-path-arch path X Y outer-w inner-w solid-h leg-h)
           (send dc draw-path path))]))

;; closes path's current sub-path and draws the outline of an arch, clockwise
;; requires leg-h != 0
(define (dc-path-arch path X Y outer-w inner-w solid-h leg-h)
  (let* ([xA X]
         [xB (+ X outer-w)]
         [xMid (/ (+ xA xB) 2.0)]
         [ySolidEnd (+ Y solid-h)]
         [yEnd (+ Y solid-h leg-h)]
         [hdx (/ (- outer-w inner-w) 2.0)]
         [xAi (+ xA hdx)]
         [xBi (- xB hdx)]
         [radius (+ (/ leg-h 2) (/ (sqr inner-w) 8 leg-h))]
         [diameter (+ radius radius)]
         [theta (asin (/ (- radius leg-h) radius))])
    (send* path
      (move-to xA Y)
      (line-to xB Y)
      (line-to xB ySolidEnd)
      (line-to xB yEnd)
      (line-to xBi yEnd)
      (arc (- xMid radius) ySolidEnd
           diameter diameter
           theta
           (- pi theta))
      ;; ends at *roughly* xAi yEnd
      (line-to xAi yEnd)
      (line-to xA yEnd)
      (line-to xA ySolidEnd)
      (line-to xA Y))))

;; ====

(define no-pen (make-object pen% "BLACK" 1 'transparent))

(define (brush/linear-gradient p stops)
  (let* ([drawer (make-pict-drawer p)]
         [w (pict-width p)]
         [h (pict-height p)])
    (dc (lambda (dc X Y)
          (let* ([grad
                  (new linear-gradient%
                       ;; Apparently gradient handles scaling,
                       ;; rotation, etc automatically (???)
                       (x0 X) (y0 Y) (x1 (+ X w)) (y1 Y)
                       (stops stops))]
                 [new-brush (new brush% (gradient grad))]
                 [old-pen (send dc get-pen)]
                 [old-brush (send dc get-brush)])
            (send* dc
              (set-pen no-pen)
              (set-brush new-brush))
            (drawer dc X Y)
            (send* dc
              (set-pen old-pen)
              (set-brush old-brush))))
        w h)))

#|
;; FIXME:
;;   (arch ....) by itself draws outline
;;   (colorize (arch ....) "red") draws filled (no outline, or same color)

Problem: picts, colorize, etc not designed to inherit brush. See
texpict/utils: filled-rectangle, eg, makes new brush from pen color;
rectangle makes new transparent brush.

|#
