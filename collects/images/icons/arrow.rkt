#lang racket/base

(require racket/class
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (all-defined-out))

(define (right-arrow-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (send dc draw-polygon (list '(0 . 9) '(15 . 9) '(14 . 0)
                                       '(31 . 15.5)
                                       '(14 . 31) '(15 . 22) '(0 . 22))))
   (/ height 32)))

(define (right-over-arrow-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (draw-path-commands
            dc 0 15 '((c (9 . -14) (19.5 . -8) (24 . -2))
                      (l (5 . -7) (2 . 20) (-20 . -2) (7 . -5))
                      (c (-2.5 . -4) (-8 . -8.5) (-14 . 0))
                      (l (-4 . -4)))))
   (/ height 32)))

(define (flomap-render-short-icon fm material)
  (define scale (/ (flomap-height fm) 32))
  (define dfm
    (let* ([dfm  (flomap->deep-flomap fm)]
           [dfm  (deep-flomap-icon-style dfm)]
           [dfm  (deep-flomap-raise dfm (* -12 scale))])
      dfm))
  (deep-flomap-render-icon dfm material))

(define (right-arrow-icon-flomap* color height material)
  (flomap-render-short-icon (right-arrow-flomap color height) material))

(define (up-arrow-icon-flomap* color height material)
  (flomap-render-icon (flomap-cw-rotate (right-arrow-flomap color height)) material))

(define (down-arrow-icon-flomap* color height material)
  (flomap-render-icon (flomap-ccw-rotate (right-arrow-flomap color height)) material))

(define (right-over-arrow-icon-flomap* color height material)
  (flomap-render-short-icon (right-over-arrow-flomap color height) material))

(define (right-under-arrow-icon-flomap* color height material)
  (flomap-render-short-icon (flomap-flip-vertical (right-over-arrow-flomap color height)) material))

(define-icon-flomap-proc right-arrow-icon-flomap right-arrow-icon-flomap* 32 color)
(define-icon-flomap-proc up-arrow-icon-flomap up-arrow-icon-flomap* 32 color)
(define-icon-flomap-proc down-arrow-icon-flomap down-arrow-icon-flomap* 32 color)
(define-icon-flomap-proc right-over-arrow-icon-flomap right-over-arrow-icon-flomap* 32 color)
(define-icon-flomap-proc right-under-arrow-icon-flomap right-under-arrow-icon-flomap* 32 color)

(define left-arrow-icon-flomap (compose flomap-flip-horizontal right-arrow-icon-flomap))
(define left-over-arrow-icon-flomap (compose flomap-flip-horizontal right-over-arrow-icon-flomap))
(define left-under-arrow-icon-flomap (compose flomap-flip-horizontal right-under-arrow-icon-flomap))

(define right-arrow-icon (compose flomap->bitmap right-arrow-icon-flomap))
(define left-arrow-icon (compose flomap->bitmap left-arrow-icon-flomap))
(define up-arrow-icon (compose flomap->bitmap up-arrow-icon-flomap))
(define down-arrow-icon (compose flomap->bitmap down-arrow-icon-flomap))

(define right-over-arrow-icon (compose flomap->bitmap right-over-arrow-icon-flomap))
(define left-over-arrow-icon (compose flomap->bitmap left-over-arrow-icon-flomap))
(define right-under-arrow-icon (compose flomap->bitmap right-under-arrow-icon-flomap))
(define left-under-arrow-icon (compose flomap->bitmap left-under-arrow-icon-flomap))
