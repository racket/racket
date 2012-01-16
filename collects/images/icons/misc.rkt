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
          foot-icon foot-flomap
          lambda-icon lambda-flomap
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
           (send dc set-pen (make-object pen% (icon-color->outline-color color) 
                              12 'solid 'projecting 'miter))
           (send dc draw-line mn mn mx mx)
           (send dc draw-line mn mx mx mn)
           (send dc set-pen (make-object pen% color 10 'solid 'projecting  'miter))
           (send dc draw-line mn mn mx mx)
           (send dc draw-line mn mx mx mn))
   (/ height 32)))

(define (flat-check-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
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
             (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
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
     (define outline-color (icon-color->outline-color color))
     (define r (/ (send outline-color red) 255.0))
     (define g (/ (send outline-color green) 255.0))
     (define b (/ (send outline-color blue) 255.0))
     (define-values (w h) (get-text-size str font))
     (define ceiling-amt (inexact->exact (ceiling outline)))
     (let* ([fm  (draw-flomap
                  w h (λ (dc)
                        (send dc set-font font)
                        (send dc set-text-foreground color)
                        (send dc draw-text str 0 0 #t)))]
            [fm  (if trim? (flomap-trim fm) fm)]
            [fm  (flomap-resize fm #f (- height (* 2 ceiling-amt)))]
            [fm  (flomap-inset fm ceiling-amt)]
            [fm  (if (outline . > . 0) (flomap-outlined fm outline (list r g b)) fm)])
       (flomap-render-icon fm material)))))

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

(define lambda-path-commands
  '((m 8.5 1.5)
    (c -1.6356765828908555 0.029546719528023596
       -3.191760877876106 0.5981878749262537
       -4.720477489085545 1.1242189706194692)
    (c 0.6669351268436579 0.7142825307374631
       0.5663221427728614 0.9399074888495575
       0.8574087929203539 0.8856493838348083)
    (c 1.1139361982300886 -0.26979469970501474
       2.7661170029498527 -0.8976661899705014
       3.5022074713864306 0.2920653404129794)
     (c 1.604836361061947 2.027318824778761
        2.2854387162241885 4.621830343362832
        2.528554440117994 7.151444427138643)
     (c 0.3116530407079646 1.536908007079646
        -2.857777387610619 7.039676186430679
        -3.8315742017699113 9.23609637758112)
     (c -1.5828472448377582 2.792818935693215
        -2.9889992117994097 5.691217406489675
        -4.772427818289086 8.366316818879056)
     (c 0.42649146902654866 0.5644402784660767
        1.0427237946902654 0.34355411445427725
        1.6228086182890855 0.25676724483775815)
     (c 0.49529097817109147 -0.07420284601769911
        0.9905831646017699 -0.14840448377581122
        1.4858741427728612 -0.22260672566371684)
     (c 1.5973270277286136 -3.787185161061947
        3.3219870961651914 -7.263537085545722
        4.820870569911505 -11.091467780530973)
     (c 0.6830176660766961 -1.5775599008849557
        1.0166688849557521 -2.445292667846608
        1.8281710631268435 -3.4783485734513273)
     (c 0.9620301781710914 0.5885710348082596
        1.2484493215339232 2.040281637758112
        1.77328405899705 3.0419137321533922)
     (c 1.5467160542772862 3.979993184660766
        3.0867486206489674 7.962568420058997
        4.546565437168141 11.975105472566373)
     (c 0.3820927622418879 0.13305596224188793
        0.7742605970501475 0.5306156554572271
        1.1366913510324481 0.14744150088495575)
     (c 0.9533687693215339 -0.5878412460176992
        2.0633098572271384 -0.9560281486725664
        2.857080825958702 -1.7685525144542773)
     (c -0.2264924884955752 -1.0982469474926253
        -0.9541940106194691 -2.1254820625368733
        -1.3975098902654866 -3.181664056637168)
     (c -2.8100934230088495 -5.615961562241888
        -5.519535197640117 -11.572843233038348
        -7.278479027728613 -17.620018746902655)
     (c -0.6478138147492625 -1.9033066855457228
        -1.4455158560471977 -4.19687149120944
        -3.5071903339233037 -4.948212008023599)
     (c -0.46965654277286134 -0.13943394171091444
        -0.9645608778761062 -0.1662308436578171
        -1.451858010619469 -0.16614886324483774)))

(defproc (lambda-flomap [color (or/c string? (is-a?/c color%))]
                        [height (and/c rational? (>=/c 0)) (default-icon-height)]
                        [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (set-icon-pen dc (icon-color->outline-color color) 4 'solid)
            (send dc set-brush (icon-color->outline-color color) 'solid)
            (draw-path-commands dc 4 0 lambda-path-commands)
            (set-icon-pen dc color 2 'solid)
            (send dc set-brush color 'solid)
            (draw-path-commands dc 4 0 lambda-path-commands))
    (/ height 32)
    material)))

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

(defproc (foot-flomap [color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
            (send dc set-brush color 'solid)
            (draw-ellipse/smoothed dc 4 8 24 24)
            (draw-ellipse/smoothed dc 0 10 5 4.5)
            (draw-ellipse/smoothed dc 3 4.5 5.5 5.5)
            (draw-ellipse/smoothed dc 8.75 1 6.25 6.25)
            (draw-ellipse/smoothed dc 16 0 7 7)
            (draw-ellipse/smoothed dc 23.5 1.5 8.5 10))
    (/ height 32)
    material)))

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
                          (set-icon-pen dc (icon-color->outline-color "azure") 1 'solid)
                          (send dc set-brush "azure" 'solid)
                          (draw-ellipse/smoothed dc 0 0 18 18))
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-bulge-spheroid dfm (* 4 scale))]
            [dfm  (deep-flomap-raise dfm (* 4 scale))])
       (deep-flomap-render-icon dfm magnifying-glass-material)))
   
   (define circle-fm
     (let* ([fm  (draw-icon-flomap
                  28 28 (λ (dc)
                          (define outline-color (icon-color->outline-color frame-color))
                          (send dc set-pen outline-color 3 'solid)
                          (send dc set-brush outline-color 'solid)
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
                          (set-icon-pen dc (icon-color->outline-color handle-color) 1 'solid)
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
              (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
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
                                 (set-icon-pen dc (icon-color->outline-color bomb-color) 1 'solid)
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
  [stop-signs-icon stop-signs-flomap]
  [foot-icon foot-flomap]
  [lambda-icon lambda-flomap])

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
