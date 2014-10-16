#lang racket/base
(require pict
         racket/contract/base
         racket/gui/base
         racket/class
         racket/math)

(provide
 (contract-out
  [make-plt-title-background (-> real? real? pict?)]))

;; xxx document and provide
;; (provide make-plt-title-background
;;          make-plt-title-background*
;;          plt-red-color
;;          plt-blue-color
;;          plt-background-color
;;          plt-lambda-color
;;          plt-pen-color
;;          plt-pen-style)

(define plt-red-color (make-object color% 242 183 183))
(define plt-blue-color (make-object color% 183 202 242))
(define plt-background-color (make-object color% 209 220 248))
(define plt-lambda-color (send the-color-database find-color "white"))
(define plt-pen-color "black")
(define plt-pen-style 'transparent)

(define (with-dc-settings dc thunk)
  (let ([alpha (send dc get-alpha)]
        [smoothing (send dc get-smoothing)]
        [pen (send dc get-pen)]
        [brush (send dc get-brush)])
    (thunk)
    (send dc set-alpha alpha)
    (send dc set-smoothing smoothing)
    (send dc set-pen pen)
    (send dc set-brush brush)))

(define (make-plt-title-background*
         w h
         plt-red-color plt-blue-color plt-background-color plt-lambda-color
         plt-pen-color plt-pen-style
         #:edge-cleanup-pen [edge-cleanup-pen #f])
  (let ()
    (define left-lambda-path
      (let ([p (new dc-path%)])
        (send p move-to 153 44)
        (send p line-to 161.5 60)
        (send p curve-to 202.5 49 230 42 245 61)
        (send p curve-to 280.06 105.41 287.5 141 296.5 186)
        (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
        (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
        (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
        (send p line-to 135 572)
        p))

    (define left-logo-path
      (let ([p (new dc-path%)])
        (send p append left-lambda-path)
        (send p arc 0 0 630 630 (* 235/360 2 pi) (* 121/360 2 pi) #f)
        p))

    (define bottom-lambda-path
      (let ([p (new dc-path%)])
        (send p move-to 135 572)
        (send p line-to 188.5 564)
        (send p curve-to 208.5 517 230.91 465.21 251 420)
        (send p curve-to 267 384 278.5 348 296.5 312)
        (send p curve-to 301.01 302.98 318 258 329 274)
        (send p curve-to 338.89 288.39 351 314 358 332)
        (send p curve-to 377.28 381.58 395.57 429.61 414 477)
        (send p curve-to 428 513 436.5 540 449.5 573)
        (send p line-to 465 580)
        (send p line-to 529 545)
        p))

    (define bottom-logo-path
      (let ([p (new dc-path%)])
        (send p append bottom-lambda-path)
        (send p arc 0 0 630 630 (* 314/360 2 pi) (* 235/360 2 pi) #f)
        p))

    (define right-lambda-path
      (let ([p (new dc-path%)])
        (send p move-to 153 44)
        (send p curve-to 192.21 30.69 233.21 14.23 275 20)
        (send p curve-to 328.6 27.4 350.23 103.08 364 151)
        (send p curve-to 378.75 202.32 400.5 244 418 294)
        (send p curve-to 446.56 375.6 494.5 456 530.5 537)
        (send p line-to 529 545)
        p))

    (define right-logo-path
      (let ([p (new dc-path%)])
        (send p append right-lambda-path)
        (send p arc 0 0 630 630 (* 314/360 2 pi) (* 121/360 2 pi) #t)
        p))

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

    ;; This function draws the paths with suitable colors:
    (define (paint-plt dc dx dy)
      (send dc set-smoothing 'aligned)
      (let ([old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)]
            [old-clip (send dc get-clipping-region)])

        (send dc set-pen plt-pen-color 0 plt-pen-style)

        (cond
          [(procedure? plt-lambda-color)
           (with-dc-settings
            dc
            (λ ()
              (plt-lambda-color dc)
              (send dc draw-path lambda-path dx dy)))]
          [plt-lambda-color
           (send dc set-brush plt-lambda-color 'solid)
           (send dc draw-path lambda-path dx dy)]
          [else
           (void)])

        ;; Draw red regions
        (cond
          [(is-a? plt-red-color bitmap%)
           (let ([rgn1 (new region% [dc dc])]
                 [rgn2 (new region% [dc dc])])
             (send rgn1 set-path left-logo-path dx dy)
             (send rgn2 set-path bottom-logo-path dx dy)
             (send rgn2 union rgn1)
             (send dc set-clipping-region rgn2)

             ;; the left and top values of the bounding box seem to change over time,
             ;; so I've just put reasonable numbers below.
             (let-values ([(sw sh) (send dc get-scale)])
               (send dc set-scale 1 1)
               (send dc draw-bitmap plt-red-color 220 100)
               (send dc set-scale sw sh)))
           (send dc set-clipping-region old-clip)
           (cleanup-edges left-logo-path dc dx dy)
           (cleanup-edges bottom-logo-path dc dx dy)]
          [(procedure? plt-red-color)
           (with-dc-settings
            dc
            (λ ()
              (plt-red-color dc)
              (send dc draw-path left-logo-path dx dy)
              (send dc draw-path bottom-logo-path dx dy)))]
          [else
           (send dc set-brush plt-red-color 'solid)
           (send dc draw-path left-logo-path dx dy)
           (send dc draw-path bottom-logo-path dx dy)])

        ;; Draw blue region
        (cond
          [(is-a? plt-blue-color bitmap%)
           (let ([rgn (new region% [dc dc])])
             (send rgn set-path right-logo-path dx dy)
             (send dc set-clipping-region rgn)

             ;; the left and top values of the bounding box seem to change over time,
             ;; so I've just put reasonable numbers below.
             (let-values ([(sw sh) (send dc get-scale)])
               (send dc set-scale 1 1)
               (send dc draw-bitmap plt-blue-color 430 50)
               (send dc set-scale sw sh))
             (send dc set-clipping-region old-clip)
             (cleanup-edges right-logo-path dc dx dy))]
          [(procedure? plt-blue-color)
           (with-dc-settings
            dc
            (λ ()
              (plt-blue-color dc)
              (send dc draw-path right-logo-path dx dy)))]
          [else
           (send dc set-brush plt-blue-color 'solid)
           (send dc draw-path right-logo-path dx dy)])

        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-clipping-region old-clip)))

    (define (cleanup-edges path dc dx dy)
      (when edge-cleanup-pen
        (let ([pen (send dc get-pen)]
              [brush (send dc get-brush)]
              [alpha (send dc get-alpha)])
          (send dc set-pen edge-cleanup-pen)
          (send dc set-brush "black" 'transparent)
          (send dc set-alpha .8)
          (send dc draw-path path dx dy)
          (send dc set-pen pen)
          (send dc set-brush brush)
          (send dc set-alpha alpha))))

    (clip
     (pin-over
      (if plt-background-color
        (colorize (filled-rectangle w h)
                  plt-background-color)
        (blank w h))
      320
      50
      (scale (dc paint-plt 630 630 0 0) 12/10)))))

(define (make-plt-title-background w h)
  (make-plt-title-background* w h
                              plt-red-color
                              plt-blue-color
                              plt-background-color
                              plt-lambda-color
                              plt-pen-color
                              plt-pen-style))
