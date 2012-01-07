#lang racket/base

(require racket/draw racket/class
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/renderfx.rkt"
         "arrow.rkt"
         "style.rkt")

(provide (all-defined-out))

(define (floppy-disk-icon-flomap* color height material)
  (define scale (/ height 32))
  
  (define metal-fm
    (let* ([fm  (draw-icon-flomap
                 18 11 (λ (dc)
                         (send dc set-background "lightgray")
                         (define outer-path (new dc-path%))
                         (send outer-path rounded-rectangle 0.5 0.5 13 12 1)
                         (define inner-path (new dc-path%))
                         (send inner-path rectangle 2.5 2.5 4 6)
                         (define outer-rgn (new region%))
                         (send outer-rgn set-path outer-path)
                         (define inner-rgn (new region%))
                         (send inner-rgn set-path inner-path)
                         (send outer-rgn subtract inner-rgn)
                         (send dc set-clipping-region outer-rgn)
                         (send dc clear))
                 scale)]
           [dfm  (flomap->deep-flomap fm)]
           [dfm  (deep-flomap-icon-style dfm)]
           [dfm  (deep-flomap-scale-z dfm 1/16)])
      (deep-flomap-render-icon dfm metal-material)))
  
  (define bottom-indent-fm
    (draw-icon-flomap
     20 11 (λ (dc)
             (send dc set-alpha 1/4)
             (send dc set-pen "black" 1 'transparent)
             (send dc set-brush "black" 'solid)
             (send dc draw-rounded-rectangle 1.5 0.5 18 11 1))
     scale))
  
  (define label-fm
    (let* ([fm  (draw-icon-flomap
                 22 20 (λ (dc)
                         (send dc set-pen "black" 1 'transparent)
                         (send dc set-brush "black" 'solid)
                         (send dc draw-rounded-rectangle -0.5 -3.5 22 21 3)
                         (send dc set-brush "lemonchiffon" 'solid)
                         (send dc draw-rounded-rectangle 0.5 -3.5 20 20 2)
                         (send dc set-brush "chocolate" 'solid)
                         (send dc draw-rectangle 0.5 -0.5 20 4)
                         (send dc set-brush "navy" 'solid)
                         (for ([i  (in-range 5.5 15 3)])
                           (send dc draw-rectangle 2.5 i 16 1)))
                 scale)]
           [dfm  (flomap->deep-flomap fm)]
           [dfm  (deep-flomap-bulge-vertical dfm (* 4 scale))])
      (deep-flomap-render-icon dfm matte-material)))
  
  (define top-indent-fm
    (draw-icon-flomap
     22 19 (λ (dc)
             (send dc set-alpha 1)
             (send dc set-pen "black" 1 'transparent)
             (send dc set-brush "black" 'solid)
             (send dc draw-rounded-rectangle -0.5 -2.5 22 20 2.5))
     scale))
  
  (define case-fm
    (draw-icon-flomap
     32 32 (λ (dc)
             (send dc set-brush color 'solid)
             (send dc draw-polygon (list '(0 . 3) '(3 . 0)
                                         '(28 . 0) '(31 . 3)
                                         '(31 . 28) '(28 . 31)
                                         '(3 . 31) '(0 . 28))))
     scale))
  
  (define disk-fm
    (let* ([dfm  (deep-flomap-ct-superimpose
                  (deep-flomap-cb-superimpose
                   (flomap->deep-flomap case-fm)
                   (deep-flomap-raise (flomap->deep-flomap bottom-indent-fm) (* -4 scale))
                   #:z-mode 'add)
                  (deep-flomap-raise (flomap->deep-flomap top-indent-fm) (* -1 scale))
                  #:z-mode 'add)]
           [dfm  (deep-flomap-icon-style dfm)])
      (deep-flomap-render-icon dfm material)))
  
  (let* ([fm  (flomap-cb-superimpose disk-fm metal-fm)]
         [fm  (flomap-ct-superimpose fm label-fm)])
    fm))

(define-icon-flomap-proc floppy-disk-icon-flomap floppy-disk-icon-flomap* 32 color)

(define (save-icon-flomap arrow-color color [height  (default-icon-height)]
                          [material  (default-icon-material)])
  (flomap-hc-append (right-arrow-icon-flomap arrow-color (* 3/4 height) material)
                    (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
                    (floppy-disk-icon-flomap color height material))) 

(define (load-icon-flomap arrow-color color [height  (default-icon-height)]
                          [material  (default-icon-material)])
  (flomap-hc-append (floppy-disk-icon-flomap color height material)
                    (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
                    (right-arrow-icon-flomap arrow-color (* 3/4 height) material)))

(define (small-save-icon-flomap arrow-color color [height  (default-icon-height)]
                                [material  (default-icon-material)])
  (flomap-pin* 0 0 11/16 0
               (floppy-disk-icon-flomap color height material)
               (right-arrow-icon-flomap arrow-color (* 3/4 height) material)))

(define (small-load-icon-flomap arrow-color color [height  (default-icon-height)]
                                [material  (default-icon-material)])
  (flomap-pin* 1 1 5/16 1
               (floppy-disk-icon-flomap color height material)
               (right-arrow-icon-flomap arrow-color (* 3/4 height) material)))

(define floppy-disk-icon (compose flomap->bitmap floppy-disk-icon-flomap))
(define save-icon (compose flomap->bitmap save-icon-flomap))
(define load-icon (compose flomap->bitmap load-icon-flomap))
(define small-save-icon (compose flomap->bitmap small-save-icon-flomap))
(define small-load-icon (compose flomap->bitmap small-load-icon-flomap))
