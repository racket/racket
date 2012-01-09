#lang racket/base

(require racket/class
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (all-defined-out))

(define (flat-right-arrow-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (send dc draw-polygon (list '(0 . 9) '(15 . 9) '(14 . 0)
                                       '(31 . 15.5)
                                       '(14 . 31) '(15 . 22) '(0 . 22))))
   (/ height 32)))

(define (flat-right-over-arrow-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (draw-path-commands
            dc 0 0 '((m 0 15)
                     (c 9 -14 19.5 -8 24 -2)
                     (l 5 -7 2 20 -20 -2 7 -5)
                     (c -2.5 -4 -8 -8.5 -14 0)
                     (l -4 -4))))
   (/ height 32)))

(define (flomap-render-short-icon fm material)
  (define scale (/ (flomap-height fm) 32))
  (define dfm
    (let* ([dfm  (flomap->deep-flomap fm)]
           [dfm  (deep-flomap-icon-style dfm)]
           [dfm  (deep-flomap-raise dfm (* -12 scale))])
      dfm))
  (deep-flomap-render-icon dfm material))

(define (right-arrow-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (flomap-render-short-icon (flat-right-arrow-flomap color height) material)))

(define (up-arrow-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (flomap-render-icon (flomap-cw-rotate (flat-right-arrow-flomap color height)) material)))

(define (down-arrow-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (flomap-render-icon (flomap-ccw-rotate (flat-right-arrow-flomap color height)) material)))

(define (right-over-arrow-flomap color
                                 [height (default-icon-height)]
                                 [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (flomap-render-short-icon (flat-right-over-arrow-flomap color height) material)))

(define (right-under-arrow-flomap color
                                  [height (default-icon-height)]
                                  [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (flomap-render-short-icon
    (flomap-flip-vertical (flat-right-over-arrow-flomap color height)) material)))

(define left-arrow-flomap (compose flomap-flip-horizontal right-arrow-flomap))
(define left-over-arrow-flomap (compose flomap-flip-horizontal right-over-arrow-flomap))
(define left-under-arrow-flomap (compose flomap-flip-horizontal right-under-arrow-flomap))

(define right-arrow-icon (compose flomap->bitmap right-arrow-flomap))
(define left-arrow-icon (compose flomap->bitmap left-arrow-flomap))
(define up-arrow-icon (compose flomap->bitmap up-arrow-flomap))
(define down-arrow-icon (compose flomap->bitmap down-arrow-flomap))

(define right-over-arrow-icon (compose flomap->bitmap right-over-arrow-flomap))
(define left-over-arrow-icon (compose flomap->bitmap left-over-arrow-flomap))
(define right-under-arrow-icon (compose flomap->bitmap right-under-arrow-flomap))
(define left-under-arrow-icon (compose flomap->bitmap left-under-arrow-flomap))
