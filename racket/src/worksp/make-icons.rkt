#lang racket/base
(require racket/class
         racket/draw
         racket/math
         file/ico
         images/icons/style
         images/flomap)

;; ----------------------------------------
;; The logo code from the manual

(define (brush c)
  (define (l v) (min 255 (floor (* #e1.1 v))))
  (define (d v) (floor (* #e0.8 v)))
  (define darker (make-color (d (send c red))
                             (d (send c green))
                             (d (send c blue))))
  (define much-darker (make-color (d (send darker red))
                                  (d (send darker green))
                                  (d (send darker blue))))
  (new brush% [gradient (make-object radial-gradient%
                                     230 230 250
                                     230 230 400
                                     (list (list 0.0 darker)
                                           (list 1.0 much-darker)))]))
       

(define red-brush (brush (make-object color% 255 36 32)))
(define blue-brush (brush (make-object color% 32 36 255)))
 
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
    (send p arc 0 0 630 630 (* 47/72 2 pi) (* 121/360 2 pi) #f)
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
    (send p arc 0 0 630 630 (* 157/180 2 pi) (* 47/72 2 pi) #f)
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
    (send p arc 0 0 630 630 (* 157/180 2 pi) (* 121/360 2 pi) #t)
    p))
 
(define lambda-path
  (let ([p (new dc-path%)])
    (send p append left-lambda-path)
    (send p append bottom-lambda-path)
    (let ([t (new dc-path%)])
        (send t append right-lambda-path)
        (send t reverse)
        (send p append t))
    (send p close)
    p))
 
(define (paint-racket dc)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush "white" 'solid)
  (send dc draw-path lambda-path)
 
  (send dc set-pen "black" 4 'solid)
 
  (send dc set-brush red-brush)
  (send dc draw-path left-logo-path)
  (send dc draw-path bottom-logo-path)
 
  (send dc set-brush blue-brush)
  (send dc draw-path right-logo-path))

(define (draw dc)
  (send dc scale (/ 265 170) (/ 256 170))
  (send dc translate 3 6)
  (send dc scale 0.25 0.25)
  (paint-racket dc))
 
(define (go [scale 1])
  (define racket-logo (make-bitmap (* scale 256) (* scale 256)))
  (define dc (new bitmap-dc% [bitmap racket-logo]))
  
  (send dc scale scale scale)
  (send dc set-smoothing 'smoothed)
  
  (draw dc)
  
  racket-logo)

;; --------------------------------------------------------------------------------

(define (scale-bm sz bm)
  (define bm2 (make-platform-bitmap sz sz))
  (define dc (send bm2 make-dc))
  (send dc scale (/ sz (send bm get-width)) (/ sz (send bm get-height)))
  (send dc set-smoothing 'smoothed)
  (send dc draw-bitmap bm 0 0)
  bm2)

(define (maybe-render-icon bm)
  (scale-bm
   256
   (if #f
       (bitmap-render-icon bm 0 plastic-icon-material)
       bm)))

; (define logo-bm (render-icon (go 2)))
; (define logo-bm (go))
(define logo-bm (read-bitmap (collection-file-path "plt-logo-red-diffuse.png" "icons")))

(define cmdline-bm
  (let ([bm (make-platform-bitmap 256 256)])
    (define dc (send bm make-dc))
    (send dc set-brush (new brush%
                            [gradient
                             (make-object linear-gradient%
                                          0 0 100 256
                                          (list (list 0.0 (make-color 250 250 250))
                                                (list 1.0 (make-color 220 220 220))))]))
    (send dc set-pen (make-pen #:color "gray" #:width 4))
    (send dc draw-rectangle 2 24 252 208)
    (send dc set-pen (make-pen #:style 'transparent))    
    (send dc set-smoothing 'smoothed)
    (send dc set-font (make-font #:size 64 #:size-in-pixels? #t #:weight 'bold #:family 'modern))
    (send dc set-text-foreground "black")
    (define-values (w h d a) (send dc get-text-extent ">"))
    (send dc draw-text ">" 14 32)
    (define s 0.65)
    (send dc scale s s)
    (send dc draw-bitmap logo-bm (* s 45/100 256) (* s 40/100 256))
    bm))

(define com-bm
  (let ([bm (make-platform-bitmap 256 256)])
    (define dc (send bm make-dc))
    (send dc draw-bitmap logo-bm 0 0)
    (send dc set-font (make-font #:face "Courier New" #:weight 'bold #:size 96))
    (define-values (w h d a) (send dc get-text-extent "COM"))
    (send dc set-text-foreground "white")
    (send dc set-brush (make-brush #:color (make-color 0 0 0 0.25)))
    (send dc set-pen (make-pen #:style 'transparent))    
    (define x (/ (- 256 w) 2))
    (define y (- 256 h))
    (send dc draw-rectangle (- x 20) (+ a y) (+ w 40) (- h a (/ d 2)))
    (send dc draw-text "COM" x y)
    bm))

(define starter-bm
  (maybe-render-icon
   (let ([bm (make-platform-bitmap 512 512)])
     (define dc (send bm make-dc))
     (send dc scale 2 2)
     (send dc set-smoothing 'smoothed)
     (send dc set-pen (make-pen #:style 'transparent))
     (send dc set-brush (make-color 40 200 40) 'solid)
     (define x 156)
     (define y 156)
     (send dc draw-ellipse x y 100 100)
     (send dc set-pen (make-pen #:cap 'round #:width 10 #:color "white"))
     (send dc draw-line (+ x 20) (+ y 50) (+ x 80) (+ y 50))
     (send dc draw-line (+ x 60) (+ y 35) (+ x 80) (+ y 50))
     (send dc draw-line (+ x 60) (+ y 65) (+ x 80) (+ y 50))
     bm)))

(define (add-starter-arrow orig-bm)
  (let ([bm (make-platform-bitmap 256 256)])
    (define dc (send bm make-dc))
    (send dc draw-bitmap orig-bm 0 0)
    (send dc draw-bitmap starter-bm 0 0)
    bm))

(define (generate dest bm)
  (define (make-ico sz)
    (define bm2 (scale-bm sz bm))
    (define bstr (make-bytes (* 4 sz sz)))
    (send bm2 get-argb-pixels 0 0 sz sz bstr)
    (argb->ico sz sz bstr))

  (define o (open-output-bytes))
  (send bm save-file o 'png)
  (define png-bytes (get-output-bytes o))
  
  (when (file-exists? dest)
    (delete-file dest))

  (write-icos
   (cons
    (png-bytes->ico png-bytes)
    (for/list ([sz '(16 32 48)])
      (make-ico sz)))
   dest))

(generate "gracket/gracket.ico" logo-bm)
(generate "racket/racket.ico" cmdline-bm)
(generate "mzcom/mzcom.ico" com-bm)
(generate "starters/mrstart.ico" (add-starter-arrow logo-bm))
(generate "starters/mzstart.ico" (add-starter-arrow cmdline-bm))

