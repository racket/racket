#lang racket/base

(require racket/class racket/draw unstable/parameter-group
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         (for-syntax racket/base syntax/parse))

(provide light-metal-icon-color
         metal-icon-color
         dark-metal-icon-color
         syntax-icon-color
         halt-icon-color
         run-icon-color
         plastic-icon-material
         rubber-icon-material
         glass-icon-material
         metal-icon-material
         bitmap-render-icon
         (activate-contract-out
          default-icon-height
          toolbar-icon-height
          default-icon-material
          icon-color->outline-color)
         (only-doc-out (all-defined-out)))

(defthing light-metal-icon-color (or/c string? (is-a?/c color%)) #:document-value
  "azure")

(defthing metal-icon-color (or/c string? (is-a?/c color%)) #:document-value
  "lightsteelblue")

(defthing dark-metal-icon-color (or/c string? (is-a?/c color%)) #:document-value
  "steelblue")

(defthing syntax-icon-color (or/c string? (is-a?/c color%)) #:document-value
  (make-object color% 76 76 255))

(defthing halt-icon-color (or/c string? (is-a?/c color%)) #:document-value
  (make-object color% 255 32 24))

(defthing run-icon-color (or/c string? (is-a?/c color%)) #:document-value
  "lawngreen")

(defparam default-icon-height (and/c rational? (>=/c 0)) 24)
(defparam toolbar-icon-height (and/c rational? (>=/c 0)) 16)

(defthing plastic-icon-material deep-flomap-material-value?
  (deep-flomap-material-value
   'cubic-zirconia 1.0 0.0 1.0
   1.0 0.3 1.0
   0.8 0.2 0.0
   0.0))

(defthing rubber-icon-material deep-flomap-material-value?
  (deep-flomap-material-value
   'cubic-zirconia 2.0 0.0 1.0
   1.5 0.25 1.0
   0.25 0.5 0.0
   0.03))

(defthing glass-icon-material deep-flomap-material-value?
  (deep-flomap-material-value
   'cubic-zirconia 1.0 0.75 0.2
   1.0 0.2 1.0
   0.2 0.4 0.25
   0.08))

(defthing metal-icon-material deep-flomap-material-value?
  (deep-flomap-material-value
   2.0 2.0 0.0 1.0
   0.7 0.15 0.0
   0.2 0.8 0.0
   0.0))

(defparam default-icon-material deep-flomap-material-value? plastic-icon-material)

(defproc (bitmap-render-icon [bitmap (is-a?/c bitmap%)]
                             [z-ratio (and rational? (>=/c 0)) 5/8]
                             [material deep-flomap-material-value? (default-icon-material)]
                             ) (is-a?/c bitmap%)
  (let* ([fm  (bitmap->flomap bitmap)]
         [dfm  (flomap->deep-flomap fm)]
         [dfm  (deep-flomap-icon-style dfm (* 32 z-ratio))])
    (flomap->bitmap (deep-flomap-render-icon dfm material))))

(defproc (icon-color->outline-color [color (or/c string? (is-a?/c color%))]) (is-a?/c color%)
  (cond [(string? color)  (define c (send the-color-database find-color color))
                          (cond [c  (icon-color->outline-color c)]
                                [else  (icon-color->outline-color "black")])]
        [else
         (define r (send color red))
         (define g (send color green))
         (define b (send color blue))
         (make-object color% (quotient r 2) (quotient g 2) (quotient b 2))]))

;; ===================================================================================================
;; Unpublished so far

(provide deep-flomap-render-icon
         deep-flomap-icon-style
         draw-icon-flomap
         flomap-render-icon
         draw-rendered-icon-flomap
         flomap-render-thin-icon
         draw-short-rendered-icon-flomap
         define-icon-wrappers
         (activate-contract-out
          set-icon-pen))

(defproc (set-icon-pen [dc (is-a?/c dc<%>)]
                       [color (or/c string? (is-a?/c color%))]
                       [width (>=/c 0)]
                       [style symbol?]) void?
  (send dc set-pen (make-object pen% color width style 'projecting 'miter)))

(define icon-lighting
  (deep-flomap-lighting-value
   '(0.0 -1.0 1.0)
   '(1.0 1.0 1.0)
   '(1.0 1.0 1.0)
   '(1.0 1.0 1.0)))

(define (deep-flomap-render-icon dfm material [background-fm #f])
  ;(printf "rendering~n")
  (parameterize/group ([deep-flomap-material  material]
                       [deep-flomap-lighting  icon-lighting])
    (deep-flomap-render dfm background-fm)))

(define (deep-flomap-icon-style dfm [height 20])
  (define s (/ (deep-flomap-height dfm) 32))
  (let* ([dfm  (deep-flomap-emboss dfm (* s 2) (* s 2))]
         [dfm  (deep-flomap-bulge-round dfm (* s 6))]
         [dfm  (deep-flomap-raise dfm (* s height))])
    dfm))

(define (draw-icon-flomap draw-proc w h scale)
  (draw-flomap (λ (dc)
                 (send dc set-scale scale scale)
                 (send dc set-smoothing 'smoothed)
                 (send dc set-origin (* 0.5 scale) (* 0.5 scale))
                 (set-icon-pen dc "black" 10 'solid)
                 (draw-proc dc))
               (inexact->exact (ceiling (* w scale)))
               (inexact->exact (ceiling (* h scale)))))

(define (flomap-render-icon fm material)
  (deep-flomap-render-icon (deep-flomap-icon-style (flomap->deep-flomap fm)) material))

(define (draw-rendered-icon-flomap draw-proc w h scale material)
  (let* ([fm  (draw-icon-flomap draw-proc w h scale)]
         [fm  (flomap-render-icon fm material)])
    fm))

(define (flomap-render-thin-icon fm material)
  (define scale (/ (flomap-height fm) 32))
  (define dfm
    (let* ([dfm  (flomap->deep-flomap fm)]
           [dfm  (deep-flomap-icon-style dfm 8)])
      dfm))
  (deep-flomap-render-icon dfm material))

(define (draw-short-rendered-icon-flomap draw-proc w h scale material)
  (flomap-render-thin-icon (draw-icon-flomap draw-proc w h scale) material))

;; ===================================================================================================
;; Syntax for writing icon functions

(define-for-syntax (arg-actual arg-stx)
  (syntax-parse arg-stx
    [[arg-kw:keyword arg-name:id arg-props:expr ...]
     (list #'arg-kw #'arg-name)]
    [[arg-name:id arg-props:expr ...]
     (list #'arg-name)]))

(define-syntax (define-icon-wrappers stx)
  (syntax-case stx ()
    [(_ (arg ...) [icon-fun flomap-fun] ...)
     (with-syntax ([(actual-args ...)  (apply append (map arg-actual (syntax->list #'(arg ...))))])
       (syntax/loc stx
         (begin
           (defproc (icon-fun arg ...) (is-a?/c bitmap%)
             (flomap->bitmap (flomap-fun actual-args ...)))
           ...)))]))
