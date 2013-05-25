#lang racket/base

(require slideshow racket/gui/base racket/runtime-path)

(provide plt-title-background
         make-plt-title-background
         plt-red-color
         plt-blue-color
         plt-background-color
         plt-lambda-color
         plt-pen-color
         plt-pen-style)

(define plt-red-color (make-object color% 242 183 183))
(define plt-blue-color (make-object color% 183 202 242))
(define plt-background-color (make-object color% 209 220 248))
(define plt-lambda-color (send the-color-database find-color "white"))
(define plt-pen-color "black")
(define plt-pen-style 'transparent)

(define (with-dc-settings dc thunk)
  (define alpha     (send dc get-alpha))
  (define smoothing (send dc get-smoothing))
  (define pen       (send dc get-pen))
  (define brush     (send dc get-brush))
  (thunk)
  (send* dc (set-alpha alpha)
            (set-smoothing smoothing)
            (set-pen pen)
            (set-brush brush)))

(define (make-plt-title-background
         red-color blue-color background-color lambda-color pen-color pen-style
         #:clip? [clip? #t] #:edge-cleanup-pen [edge-cleanup-pen #f])
  (define-syntax-rule (make-path cmd ...)
    (let ([p (new dc-path%)]) (send* p cmd ...) p))
  (define left-lambda-path
    (make-path (move-to 153 44)
               (line-to 161.5 60)
               (curve-to 202.5 49 230 42 245 61)
               (curve-to 280.06 105.41 287.5 141 296.5 186)
               (curve-to 301.12 209.08 299.11 223.38 293.96 244)
               (curve-to 281.34 294.54 259.18 331.61 233.5 375)
               (curve-to 198.21 434.63 164.68 505.6 125.5 564)
               (line-to 135 572)))
  (define left-logo-path
    (make-path (append left-lambda-path)
               (arc 0 0 630 630 (* 235/360 2 pi) (* 121/360 2 pi) #f)))
  (define bottom-lambda-path
    (make-path (move-to 135 572)
               (line-to 188.5 564)
               (curve-to 208.5 517 230.91 465.21 251 420)
               (curve-to 267 384 278.5 348 296.5 312)
               (curve-to 301.01 302.98 318 258 329 274)
               (curve-to 338.89 288.39 351 314 358 332)
               (curve-to 377.28 381.58 395.57 429.61 414 477)
               (curve-to 428 513 436.5 540 449.5 573)
               (line-to 465 580)
               (line-to 529 545)))
  (define bottom-logo-path
    (make-path (append bottom-lambda-path)
               (arc 0 0 630 630 (* 314/360 2 pi) (* 235/360 2 pi) #f)))
  (define right-lambda-path
    (make-path (move-to 153 44)
               (curve-to 192.21 30.69 233.21 14.23 275 20)
               (curve-to 328.6 27.4 350.23 103.08 364 151)
               (curve-to 378.75 202.32 400.5 244 418 294)
               (curve-to 446.56 375.6 494.5 456 530.5 537)
               (line-to 529 545)))
  (define right-logo-path
    (make-path (append right-lambda-path)
               (arc 0 0 630 630 (* 314/360 2 pi) (* 121/360 2 pi) #t)))
  (define lambda-path ;; the lambda by itself (no circle)
    (let ([p (new dc-path%)])
      (send p append left-lambda-path)
      (send p append bottom-lambda-path)
      (let ([t (make-object dc-path%)])
        (send t append right-lambda-path)
        (send t reverse)
        (send p append t))
      (send p close)
      p))

  ;; (define lambda-path
  ;;   (make-path (append left-lambda-path)
  ;;              (append bottom-lambda-path)
  ;;              (append right-lambda-path)))

  ;; This function draws the paths with suitable colors:
  (define (paint-plt dc dx dy)
    (send dc set-smoothing 'aligned)
    (define old-pen   (send dc get-pen))
    (define old-brush (send dc get-brush))
    (define old-clip  (send dc get-clipping-region))
    (send dc set-pen pen-color 0 pen-style)
    (cond [(procedure? lambda-color)
           (with-dc-settings dc
             (λ () (lambda-color dc)
                   (send dc draw-path lambda-path dx dy)))]
          [lambda-color
           (send* dc (set-brush lambda-color 'solid)
                     (draw-path lambda-path dx dy))]
          [else (void)])
    ;; Draw red regions
    (cond [(is-a? red-color bitmap%)
           (define rgn1 (new region% [dc dc]))
           (define rgn2 (new region% [dc dc]))
           (send rgn1 set-path left-logo-path dx dy #;(- dx 150) #;(- dy 20))
           (send rgn2 set-path bottom-logo-path dx dy #;(- dx 150) #;(- dy 20))
           (send rgn2 union rgn1)
           (send dc set-clipping-region rgn2)
           ;; the left and top values of the bounding box seem to change over
           ;; time, so I've just put reasonable numbers below.
           (let-values ([(sw sh) (send dc get-scale)])
             (send* dc (set-scale 1 1)
                       (draw-bitmap red-color 220 100)
                       (set-scale sw sh)))
           (send dc set-clipping-region old-clip)
           (cleanup-edges left-logo-path dc dx dy)
           (cleanup-edges bottom-logo-path dc dx dy)]
          [(procedure? red-color)
           (with-dc-settings dc
             (λ () (red-color dc)
                   (send* dc (draw-path left-logo-path dx dy)
                             (draw-path bottom-logo-path dx dy))))]
          [else (send* dc (set-brush red-color 'solid)
                          (draw-path left-logo-path dx dy)
                          (draw-path bottom-logo-path dx dy))])
    ;; Draw blue region
    (cond [(is-a? blue-color bitmap%)
           (define rgn (new region% [dc dc]))
           (send rgn set-path right-logo-path dx dy #;(- dx 150) #;(- dy 20))
           (send dc set-clipping-region rgn)
           ;; the left and top values of the bounding box seem to change over
           ;; time, so I've just put reasonable numbers below.
           (let-values ([(sw sh) (send dc get-scale)])
             (send* dc (set-scale 1 1)
                    (draw-bitmap blue-color 430 50)
                    (set-scale sw sh)))
           (send dc set-clipping-region old-clip)
           (cleanup-edges right-logo-path dc dx dy)]
          [(procedure? blue-color)
           (with-dc-settings dc
             (λ () (blue-color dc)
                   (send dc draw-path right-logo-path dx dy)))]
          [else (send* dc (set-brush blue-color 'solid)
                          (draw-path right-logo-path dx dy))])
    (send* dc (set-pen old-pen)
              (set-brush old-brush)
              (set-clipping-region old-clip)))
  (define (cleanup-edges path dc dx dy)
    (when edge-cleanup-pen
      (define pen   (send dc get-pen))
      (define brush (send dc get-brush))
      (define alpha (send dc get-alpha))
      (send* dc (set-pen edge-cleanup-pen)
                (set-brush "black" 'transparent)
                (set-alpha .8)
                (draw-path path dx dy)
                (set-pen pen)
                (set-brush brush)
                (set-alpha alpha))))
  (define image (pin-over
                 (if background-color
                   (colorize (filled-rectangle client-w client-h)
                             background-color)
                   (blank client-w client-h))
                 320 50
                 (scale (dc paint-plt 630 630 0 0) 12/10)))
  (if clip? (clip image) image))

(define plt-title-background
  (make-plt-title-background plt-red-color
                             plt-blue-color
                             plt-background-color
                             plt-lambda-color
                             plt-pen-color
                             plt-pen-style))

(define-runtime-path arrow.png "128x128-arrow.png")
(define blue-arrow (read-bitmap arrow.png))

(define result.png "racket-rising.png")

(define size 1)
(define bmp (make-bitmap (round (* 1024 size 2/3)) (* 768 size 1/2)))
(define bdc (make-object bitmap-dc% bmp))
(draw-pict (scale plt-title-background size) bdc -100 0)
(void (send bdc draw-bitmap
            blue-arrow
            (/ (- (send bmp get-width) (send blue-arrow get-width)) 2)
            (/ (- (send bmp get-height) (send blue-arrow get-height)) 2)))
(when (send bmp save-file result.png 'png)
  (printf "wrote ~a\n" result.png))
