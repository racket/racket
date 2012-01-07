#lang racket

(require racket/draw unstable/parameter-group
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/renderfx.rkt"
         "../private/transient-box.rkt")

(provide (all-defined-out))

(define icon-lighting
  (deep-flomap-lighting-value
   '(0.0 -1.0 1.0)
   '(1.0 1.0 1.0)
   '(1.0 1.0 1.0)
   '(1.0 1.0 1.0)))

(define plastic-icon-material
  (deep-flomap-material-value
   'cubic-zirconia 1.0 0.0 1.0
   1.0 0.3 1.0
   0.8 0.2 0.0
   0.0))

(define glass-icon-material
  (deep-flomap-material-value
   'cubic-zirconia 1.0 0.75 0.15
   0.5 0.2 1.0
   0.0 0.4 0.25
   0.08))

(define metal-icon-color "lightsteelblue")
(define dark-metal-icon-color "steelblue")
(define syntax-icon-color (make-object color% 38 38 128))
(define halt-icon-color (make-object color% 255 32 24))
(define run-icon-color "lawngreen")

(define default-icon-material (make-parameter plastic-icon-material))
(define default-icon-height (make-parameter 24))
(define toolbar-icon-height (make-parameter 16))

;(default-icon-material glass-icon-material)
;(default-icon-material matte-material)

(define (deep-flomap-render-icon dfm material)
  ;(printf "rendering~n")
  (parameterize/group ([deep-flomap-material  material]
                       [deep-flomap-lighting  icon-lighting])
    (deep-flomap-render dfm)))

(define (deep-flomap-icon-style dfm)
  (define s (/ (deep-flomap-height dfm) 32))
  (let* ([dfm  (deep-flomap-emboss dfm (* s 2) (* s 2))]
         [dfm  (deep-flomap-bulge-round dfm (* s 6))]
         [dfm  (deep-flomap-raise dfm (* s 18))])
    dfm))

(define (draw-icon-flomap w h draw-proc scale)
  (draw-flomap (inexact->exact (ceiling (* w scale)))
               (inexact->exact (ceiling (* h scale)))
               (λ (dc)
                 (send dc set-scale scale scale)
                 (send dc set-smoothing 'smoothed)
                 (send dc set-origin (* 0.5 scale) (* 0.5 scale))
                 (send dc set-pen (make-object pen% "black" 1 'solid 'projecting 'miter))
                 (draw-proc dc))))

(define (flomap-render-icon fm material)
  (deep-flomap-render-icon (deep-flomap-icon-style (flomap->deep-flomap fm)) material))

(define (draw-rendered-icon-flomap w h draw-proc scale material)
  (let* ([fm  (draw-icon-flomap w h draw-proc scale)]
         [fm  (flomap-render-icon fm material)])
    fm))

(define (clean-cache! h)
  (define ks (for*/list ([(k v)  (in-hash h)]
                         [vv  (in-value (transient-box-value v))]
                         #:when (not vv))
               k))
  (for ([k  (in-list ks)]) (hash-remove! h k)))

(define (transient-value-hash-ref! h k thnk)
  (thnk)
  #;(begin
  (define bx (hash-ref! h k (λ () (make-transient-box (thnk)))))
  (transient-box-touch! bx)
  (define val (transient-box-value bx))
  (cond [val  val]
        [else  (clean-cache! h)
               (let ([val  (thnk)])
                 (hash-set! h k (make-transient-box val))
                 val)])))

(define caches empty)

(define (add-cache! cache) (set! caches (cons cache caches)))

(define (clean-caches!)
  (for ([h  (in-list caches)])
    (clean-cache! h)))

(define (read-caches)
  (for*/list ([cache  (in-list caches)]
              [(k v)  (in-hash cache)])
    (cons k v)))

(define-syntax-rule (define-icon-flomap-proc name name* min-height args ...)
  (define name
    (let ([cache  (make-hash)])
      (add-cache! cache)
      (λ (args ...
          [height    (default-icon-height)]
          [material  (default-icon-material)])
        (cond [(height . < . min-height)
               (flomap-scale (transient-value-hash-ref! cache (list args ... min-height material)
                                                        (λ () (name* args ... min-height material)))
                             (/ height min-height))]
              [else
               (transient-value-hash-ref! cache (list args ... height material)
                                          (λ () (name* args ... height material)))])))))
