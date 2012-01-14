#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (activate-contract-out
          text-icon text-flomap
          recycle-icon recycle-flomap
          x-icon x-flomap
          check-icon check-flomap
          regular-polygon-icon regular-polygon-flomap
          octagon-icon octagon-flomap
          stop-sign-icon stop-sign-flomap
          stop-signs-icon stop-signs-flomap
          magnifying-glass-icon magnifying-glass-flomap
          left-magnifying-glass-icon  left-magnifying-glass-flomap
          bomb-icon bomb-flomap
          left-bomb-icon left-bomb-flomap)
         (only-doc-out (all-defined-out)))

;; ===================================================================================================
;; Unrendered flomaps

(define (flat-x-flomap color height)
  (define mn 7.5)
  (define mx 23.5)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-pen (make-object pen% "black" 12 'solid 'projecting 'miter))
           (send dc draw-line mn mn mx mx)
           (send dc draw-line mn mx mx mn)
           (send dc set-pen (make-object pen% color 10 'solid 'projecting  'miter))
           (send dc draw-line mn mn mx mx)
           (send dc draw-line mn mx mx mn))
   (/ height 32)))

(define (flat-check-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (draw-path-commands
            dc 0 0 '((m 0 19)
                     (c 0 0 7 4 14 12 5.5 -13.5 17 -23 17 -23)
                     (l -9 -8)
                     (c 0 0 -6.5 7.5 -9.5 16 -2.5 -4 -6 -6.5 -6 -6.5)
                     (l -6 9))))
   (/ height 32)))

(define (flat-regular-polygon-flomap sides start color size)
  (let ([start  (- start)])
    (draw-icon-flomap
     32 32 (λ (dc)
             (send dc set-brush color 'solid)
             (define dθ (/ (* 2 pi) sides))
             (define θs (sequence->list (in-range start (+ start (* 2 pi)) dθ)))
             (define max-frac (apply max (append (map (compose abs cos) θs)
                                                 (map (compose abs sin) θs))))
             (send dc draw-polygon (for/list ([θ  (in-list θs)])
                                     (cons (+ 15.5 (/ (* 15.5 (cos θ)) max-frac))
                                           (+ 15.5 (/ (* 15.5 (sin θ)) max-frac))))))
     (/ size 32))))

;; ===================================================================================================
;; Rendered flomaps

(defproc (text-flomap [str string?] [font (is-a?/c font%)]
                      [color (or/c string? (is-a?/c color%))]
                      [trim? boolean? #t]
                      [outline (or/c 'auto (and/c rational? (>=/c 0))) 'auto]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (define size (max 32 (send font get-point-size)))
  (define family (send font get-family))
  (define style (send font get-style))
  (define weight (send font get-weight))
  (define underline? (send font get-underlined))
  (define smoothing (send font get-smoothing))
  
  (make-cached-flomap
   [height str family style weight underline? smoothing color trim? outline material]
   (let ([font  (make-object font% size family style weight underline? smoothing #t)]
         [outline  (if (equal? outline 'auto) (/ height 32) outline)])
     (define-values (w h) (get-text-size str font))
     (define ceiling-amt (inexact->exact (ceiling outline)))
     (define fm
       (let* ([fm  (draw-flomap
                    w h (λ (dc)
                          (send dc set-font font)
                          (send dc set-text-foreground color)
                          (send dc draw-text str 0 0 #t)))]
              [fm  (if trim? (flomap-trim fm) fm)]
              [fm  (flomap-resize fm #f (- height (* 2 ceiling-amt)))]
              [fm  (flomap-inset fm ceiling-amt)]
              [fm  (if (outline . > . 0) (flomap-outlined fm outline) fm)])
         fm))
     (flomap-render-icon fm material))))

(defproc (recycle-flomap [color (or/c string? (is-a?/c color%))]
                         [height (and/c rational? (>=/c 0)) (default-icon-height)]
                         [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (define size (max 1 (min 1024 (inexact->exact (ceiling (* 2 height))))))
  (text-flomap "♻" (make-object font% size 'default) color #t (/ height 64) height material))

(defproc (x-flomap [color (or/c string? (is-a?/c color%))]
                   [height (and/c rational? (>=/c 0)) (default-icon-height)]
                   [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([fm   (flat-x-flomap color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-raise dfm (* -8 scale))])
     (deep-flomap-render-icon dfm material))))

(defproc (check-flomap [color (or/c string? (is-a?/c color%))]
                       [height (and/c rational? (>=/c 0)) (default-icon-height)]
                       [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([fm   (flat-check-flomap color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-raise dfm (* -12 scale))])
     (deep-flomap-render-icon dfm material))))

(defproc (regular-polygon-flomap [sides exact-positive-integer?]
                                 [start real?]
                                 [color (or/c string? (is-a?/c color%))]
                                 [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                 [material deep-flomap-material-value? (default-icon-material)]
                                 ) flomap?
  (make-cached-flomap
   [height sides start color material]
   (flomap-render-icon (flat-regular-polygon-flomap sides start color height) material)))

(defproc (octagon-flomap [color (or/c string? (is-a?/c color%))]
                         [height (and/c rational? (>=/c 0)) (default-icon-height)]
                         [material deep-flomap-material-value? (default-icon-material)]) flomap?
  #:document-body
  (regular-polygon-flomap 8 (/ (* 2 pi) 16) color height material))

(defproc (stop-sign-flomap [color (or/c string? (is-a?/c color%))]
                           [height (and/c rational? (>=/c 0)) (default-icon-height)]
                           [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([indent-fm  (fm* 0.5 (flat-x-flomap "black" (* 22 scale)))]
          [indent-dfm  (deep-flomap-raise (flomap->deep-flomap indent-fm) (* -1 scale))]
          [fm   (flat-regular-polygon-flomap 8 (/ (* 2 pi) 16) color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
          [fm  (deep-flomap-render-icon dfm material)])
     (flomap-cc-superimpose fm (x-flomap light-metal-icon-color (* 22 scale) metal-icon-material)))))

(defproc (stop-signs-flomap [color (or/c string? (is-a?/c color%))]
                            [height (and/c rational? (>=/c 0)) (default-icon-height)]
                            [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (define fm (stop-sign-flomap color (* height 2/3) material))
  (flomap-pin* 3/16 1/4 0 0
               fm (flomap-pin* 3/16 1/4 0 0 fm fm)))

;; ---------------------------------------------------------------------------------------------------
;; Magnifying glass

(define magnifying-glass-material
  (deep-flomap-material-value
   'glass 1.0 0.75 1.0
   0.25 0.15 1.0
   0.25 0.25 0.0
   0.0))

(defproc (magnifying-glass-flomap [frame-color (or/c string? (is-a?/c color%))]
                                  [handle-color (or/c string? (is-a?/c color%))]
                                  [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                  [material deep-flomap-material-value? (default-icon-material)]
                                  ) flomap?
  (make-cached-flomap
   [height frame-color handle-color material]
   (define scale (/ height 32))
   (define glass-fm
     (let* ([fm  (draw-icon-flomap
                  18 18 (λ (dc)
                          (send dc set-pen handle-color 1 'solid)
                          (send dc set-brush "azure" 'solid)
                          (draw-ellipse/smoothed dc 0 0 18 18)
                          (send dc set-alpha 0.5)
                          (send dc set-pen "black" 1 'solid)
                          (send dc set-brush "white" 'transparent)
                          (draw-ellipse/smoothed dc 0 0 18 18))
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-bulge-spheroid dfm (* 4 scale))]
            [dfm  (deep-flomap-raise dfm (* 4 scale))])
       (deep-flomap-render-icon dfm magnifying-glass-material)))
   
   (define circle-fm
     (let* ([fm  (draw-icon-flomap
                  28 28 (λ (dc)
                          (send dc set-pen "black" 3 'solid)
                          (send dc set-brush "black" 'solid)
                          (draw-ellipse/smoothed dc 1 1 26 26)
                          (send dc set-pen frame-color 1 'solid)
                          (send dc set-brush frame-color 'solid)
                          (draw-ellipse/smoothed dc 1 1 26 26))
                  scale)]
            [indent-fm  (draw-icon-flomap
                         28 28 (λ (dc)
                                 (send dc set-pen frame-color 1 'solid)
                                 (send dc set-brush frame-color 'solid)
                                 (draw-ellipse/smoothed dc 5 5 18 18))
                         scale)]
            [indent-dfm  (flomap->deep-flomap indent-fm)]
            [indent-dfm  (deep-flomap-raise indent-dfm (* -4 scale))]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-raise dfm (* 12 scale))]
            [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
            [dfm  (deep-flomap-smooth-z dfm (* 2/3 scale))])
       (deep-flomap-render-icon dfm metal-icon-material)))
   
   (define handle-fm
     (let* ([fm  (draw-icon-flomap
                  11 11 (λ (dc)
                          (send dc set-brush handle-color 'solid)
                          (define p (new dc-path%))
                          (send p move-to 4 0)
                          (send p line-to 10 5)
                          (send p curve-to 10 8 8 10 5 10)
                          (send p line-to 0 4)
                          (send p move-to 4 0)
                          (send dc draw-path p))
                  scale)])
       (flomap-render-icon fm material)))
   
   (flomap-pin* 0 0 21/28 21/28
                handle-fm
                (flomap-pin* 1/2 1/2 1/2 1/2 circle-fm glass-fm))))

(defproc (left-magnifying-glass-flomap [frame-color (or/c string? (is-a?/c color%))]
                                       [handle-color (or/c string? (is-a?/c color%))]
                                       [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                       [material deep-flomap-material-value? (default-icon-material)]
                                       ) flomap?
  (flomap-flip-horizontal (magnifying-glass-flomap frame-color handle-color height material)))

;; ---------------------------------------------------------------------------------------------------
;; Bomb

(defproc (left-bomb-flomap [cap-color (or/c string? (is-a?/c color%))]
                           [bomb-color (or/c string? (is-a?/c color%))]
                           [height (and/c rational? (>=/c 0)) (default-icon-height)]
                           [material deep-flomap-material-value? (default-icon-material)]
                           ) flomap?
  (make-cached-flomap
   [height cap-color bomb-color material]
   (define scale (/ height 32))
   (define fuse-fm
     (let* ([fm  (draw-icon-flomap
                  10 25 (λ (dc)
                          (send dc set-pen "darkred" 1 'solid)
                          (send dc set-brush "gold" 'solid)
                          (draw-path-commands
                           dc 0 0
                           '((m 3.5 0)
                             (c -5 0 -3.29080284 10.4205 -3 11.5
                                1.1137011 4.1343 2 6.5 0 8.5
                                -0.5711131 2.0524 1.5 4 3.5 3.5
                                2.5711131 -2.5524 3.1327042 -5.5355 2 -9.5
                                -2 -7 -2 -9 -1.5 -9
                                0 1 -0.5 2 1 3.5
                                2 0.5 4 -1.5 3.5 -3.5
                                -2 -2 -2 -5 -5.5 -5))))
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-icon-style dfm)]
            [dfm  (deep-flomap-scale-z dfm 1)])
       (deep-flomap-render-icon dfm matte-material)))
   
   (define (bomb-cap-flomap color)
     (draw-icon-flomap
      20 20 (λ (dc)
              (send dc set-pen "black" 1 'solid)
              (send dc set-brush color 'solid)
              (draw-path-commands dc 0 0 '((m 1.5 11.5)
                                           (l 10 -10 2.5 2.5)
                                           (c 4 5 -5 14 -10 10)
                                           (l -2.5 -2.5)))
              (draw-path-commands dc 0 0 '((m 1.5 11.5)
                                           (c -2 -5 5 -12 10 -10
                                              4 5 -5 14 -10 10))))
      scale))
   
   (define cap-fm
     (let* ([cap-fm  (bomb-cap-flomap cap-color)]
            [cap-dfm  (flomap->deep-flomap cap-fm)]
            [cap-dfm  (deep-flomap-icon-style cap-dfm)])
       (deep-flomap-render-icon cap-dfm material)))
   
   (define sphere-fm
     (let* ([sphere-fm  (draw-icon-flomap
                         30 30 (λ (dc)
                                 (send dc set-brush bomb-color 'solid)
                                 (draw-ellipse/smoothed dc 0 0 30 30))
                         scale)]
            [cap-fm  (bomb-cap-flomap cap-color)]
            [cap-dfm  (flomap->deep-flomap cap-fm)]
            [cap-dfm  (deep-flomap-raise cap-dfm (* -2 scale))]
            [cap-dfm  (deep-flomap-smooth-z cap-dfm (* 1 scale))]
            [sphere-dfm  (flomap->deep-flomap sphere-fm)]
            [sphere-dfm  (deep-flomap-bulge-spheroid sphere-dfm (* 15 scale))]
            [sphere-dfm  (deep-flomap-inset sphere-dfm 2 2 0 0)]
            [sphere-dfm  (deep-flomap-lt-superimpose 'add sphere-dfm cap-dfm)])
       (deep-flomap-render-icon sphere-dfm material)))
   (flomap-lt-superimpose sphere-fm cap-fm fuse-fm)))

(defproc (bomb-flomap [cap-color (or/c string? (is-a?/c color%))]
                      [bomb-color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (flomap-flip-horizontal (left-bomb-flomap cap-color bomb-color height material)))

;; ===================================================================================================
;; Bitmaps (icons)

(defproc (text-icon [str string?] [font (is-a?/c font%)]
                    [color (or/c string? (is-a?/c color%))]
                    [trim? boolean? #t]
                    [outline (or/c 'auto (and/c rational? (>=/c 0))) 'auto]
                    [height (and/c rational? (>=/c 0)) (default-icon-height)]
                    [material deep-flomap-material-value? (default-icon-material)]
                    ) (is-a?/c bitmap%)
  (flomap->bitmap (text-flomap str font color trim? outline height material)))

(defproc (regular-polygon-icon [sides exact-positive-integer?]
                               [start real?]
                               [color (or/c string? (is-a?/c color%))]
                               [height (and/c rational? (>=/c 0)) (default-icon-height)]
                               [material deep-flomap-material-value? (default-icon-material)]
                               ) (is-a?/c bitmap%)
  (flomap->bitmap (regular-polygon-flomap sides start color height material)))

(define-icon-wrappers
  ([color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [recycle-icon recycle-flomap]
  [x-icon x-flomap]
  [check-icon check-flomap]
  [octagon-icon octagon-flomap]
  [stop-sign-icon stop-sign-flomap]
  [stop-signs-icon stop-signs-flomap])

(define-icon-wrappers
  ([frame-color (or/c string? (is-a?/c color%))]
   [handle-color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [magnifying-glass-icon magnifying-glass-flomap]
  [left-magnifying-glass-icon  left-magnifying-glass-flomap])

(define-icon-wrappers
  ([cap-color (or/c string? (is-a?/c color%))]
   [bomb-color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [bomb-icon bomb-flomap]
  [left-bomb-icon left-bomb-flomap])
