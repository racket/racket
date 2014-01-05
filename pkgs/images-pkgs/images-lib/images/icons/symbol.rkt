#lang racket/base

(require racket/draw racket/class racket/math racket/sequence racket/flonum
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide flat-x-flomap flat-check-flomap
         (activate-contract-out
          text-icon text-flomap
          recycle-icon recycle-flomap
          x-icon x-flomap
          check-icon check-flomap
          lambda-icon lambda-flomap
          hash-quote-icon hash-quote-flomap)
         (only-doc-out (all-defined-out)))

(define (flat-x-flomap color height #:thickness [thickness 10])
  (define mn 7.5)
  (define mx 23.5)
  (draw-icon-flomap
   (λ (dc)
     (send dc set-pen (make-object pen% (icon-color->outline-color color) 
                        (+ thickness 2) 'solid 'projecting 'miter))
     (send dc draw-line mn mn mx mx)
     (send dc draw-line mn mx mx mn)
     (send dc set-pen (make-object pen% color thickness 'solid 'projecting  'miter))
     (send dc draw-line mn mn mx mx)
     (send dc draw-line mn mx mx mn))
   32 32 (/ height 32)))

(define (flat-check-flomap color height)
  (draw-icon-flomap
   (λ (dc)
     (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
     (send dc set-brush color 'solid)
     (draw-path-commands dc '((m 0 19)
                              (c 0 0 7 4 14 12 5.5 -13.5 17 -23 17 -23)
                              (l -9 -8)
                              (c 0 0 -6.5 7.5 -9.5 16 -2.5 -4 -6 -6.5 -6 -6.5)
                              (l -6 9))
                         0 0))
   32 32 (/ height 32)))

(defproc (text-flomap [str string?]
                      [font (is-a?/c font%) (make-font)]
                      [#:trim? trim? boolean? #t]
                      [#:color color (or/c string? (is-a?/c color%)) "white"]
                      [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [#:material material deep-flomap-material-value? (default-icon-material)]
                      [#:outline outline (and/c rational? (>=/c 0)) (/ height 32)]
                      ) flomap?
  (define family (send font get-family))
  (define style (send font get-style))
  (define weight (send font get-weight))
  (define underline? (send font get-underlined))
  (define smoothing (send font get-smoothing))
  
  (make-cached-flomap
   [height str family style weight underline? smoothing trim? outline color material]
   (let ([font  (make-object font% (min height 1024) family style weight underline? smoothing #t)])
     (define outline-color (icon-color->outline-color color))
     (define r (real->double-flonum (/ (send outline-color red) 255)))
     (define g (real->double-flonum (/ (send outline-color green) 255)))
     (define b (real->double-flonum (/ (send outline-color blue) 255)))
     (define-values (w h) (get-text-size str font))
     (define ceiling-amt (inexact->exact (min (/ height 2) (ceiling outline))))
     (let* ([fm  (draw-flomap
                  (λ (dc)
                    (send dc scale 2 2)
                    (send dc set-font font)
                    (send dc set-text-foreground color)
                    (send dc draw-text str 0 0 #t))
                  (* w 2) (* h 2))]
            [fm  (if trim? (flomap-trim fm) fm)]
            [fm  (flomap-resize fm #f (- height (* 2 ceiling-amt)))]
            [fm  (flomap-inset fm ceiling-amt)]
            [fm  (cond [(outline . > . 0)
                        (flomap-cc-superimpose (flomap-outline fm outline (vector 1.0 r g b)) fm)]
                       [else  fm])])
       (flomap-render-icon fm material)))))

(define recycle-path-commands
  '((m 13.28125 0.65625)
    (c 0.463636 0.0842975 0.965857 0.50656 1.21875 0.84375)
    (l 4.09375 7.09375)
    (l -2.125 1.25)
    (l 7.0 0.0)
    (l 3.5 -6.0625)
    (l -2.15625 1.21875)
    (l -2.125 -3.78125)
    (c -0.210743 -0.37933886 -0.630114 -0.5625 -1.09375 -0.5625)
    (l -8.3125 0.0)
    (m -2.40625 0.4375)
    (c -1.0747934 0.0368802 -2.119938 0.438998 -2.5625 1.21875)
    (l -3.21875 5.59375)
    (l 6.15625 3.59375)
    (l 3.9375 -6.84375)
    (l -1.5625 -2.59375)
    (c -0.569008 -0.6743802 -1.675207 -1.0056302 -2.75 -0.96875)
    (m 16.65625 8.65625)
    (l -6.21875 3.5625)
    (l 3.9375 6.6875)
    (l 3.3125 0)
    (c 1.34876 -0.252893 3.398916 -2.442717 2.21875 -4.71875)
    (l -3.25 -5.53125)
    (m -27.4375 1.5)
    (l 2.21875 1.28125)
    (l -1.4375 2.40625)
    (c -1.2644628 2.360331 0.8605372 4.956767 2.125 5.09375)
    (l 3.28125 0.0)
    (l 2.21875 -3.875)
    (l 2.25 1.21875)
    (l -3.59375 -6.125)
    (l -7.0625 0.0)
    (m 20.09375 7.1875)
    (l -3.59375 6.15625)
    (l 3.59375 6.125)
    (l 0.0 -2.59375)
    (l 4.375 0.0)
    (c 0.505785 0.0 0.862655 -0.28781 1.03125 -0.625)
    (l 3.96875 -7.0)
    (c -0.210743 0.126446 -0.355424 0.3532 -1.15625 0.4375)
    (l -8.21875 0.0)
    (l 0.0 -2.5)
    (m -18.21875 2.15625)
    (c 0.168595 0.210744 0.1346592 0.174793 3.84375 6.75)
    (c 0.2528926 0.379339 0.5988637 0.8234 1.0625 0.78125)
    (l 7.875 0.0)
    (l 0.0 -7.1875)
    (l -11.71875 0.0)
    (c -0.6322314 0.0 -0.8622934 -0.175155 -1.0625 -0.34375)))

(defproc (recycle-flomap [#:color color (or/c string? (is-a?/c color%)) "forestgreen"]
                         [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                         [#:material material deep-flomap-material-value? (default-icon-material)]
                         ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-short-rendered-icon-flomap
    (λ (dc)
      (set-icon-pen dc (icon-color->outline-color color) 1/2 'solid)
      (send dc set-brush color 'solid)
      (draw-path-commands dc recycle-path-commands 0 0))
    32 32 (/ height 32) material)))

(defproc (x-flomap [#:color color (or/c string? (is-a?/c color%)) halt-icon-color]
                   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                   [#:material material deep-flomap-material-value? (default-icon-material)]
                   [#:thickness thickness (and/c rational? (>=/c 0)) 10]
                   ) flomap?
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([fm   (flat-x-flomap color height #:thickness thickness)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-raise dfm (* -8 scale))])
     (deep-flomap-render-icon dfm material))))

(defproc (check-flomap [#:color color (or/c string? (is-a?/c color%)) run-icon-color]
                       [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                       [#:material material deep-flomap-material-value? (default-icon-material)]
                       ) flomap?
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

(defproc (lambda-flomap [#:color color (or/c string? (is-a?/c color%)) "white"]
                        [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                        [#:material material deep-flomap-material-value? (default-icon-material)]
                        ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    (λ (dc)
      (set-icon-pen dc (icon-color->outline-color color) 4 'solid)
      (send dc set-brush (icon-color->outline-color color) 'solid)
      (draw-path-commands dc lambda-path-commands 4 0)
      (set-icon-pen dc color 2 'solid)
      (send dc set-brush color 'solid)
      (draw-path-commands dc lambda-path-commands 4 0))
    32 32 (/ height 32) material)))

(defproc (hash-quote-flomap
          [#:color color (or/c string? (is-a?/c color%)) "mediumseagreen"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (define (draw-hash-quote dc)
     ;; vertical lines
     (send dc draw-polygon '((6 . 0) (11 . 0) (9 . 30) (4 . 30)))
     (send dc draw-polygon '((17 . 0) (22 . 0) (20 . 30) (15 . 30)))
     ;; horizontal lines
     (send dc draw-polygon '((1 . 6.5) (26 . 6.5) (26 . 11.5) (1 . 11.5)))
     (send dc draw-polygon '((0 . 18.5) (25 . 18.5) (25 . 23.5) (0 . 23.5)))
     ;; quote
     (send dc draw-polygon '((30 . 0) (34 . 0) (33 . 9) (30 . 9))))
   
   (define outline-color (icon-color->outline-color color))
   
   (draw-rendered-icon-flomap
    (λ (dc)
      (send dc translate 0.5 0.5)
      (set-icon-pen dc outline-color 2 'solid)
      (send dc set-brush outline-color 'solid)
      (draw-hash-quote dc)
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush color 'solid)
      (draw-hash-quote dc))
    36 32 (/ height 32) material)))

;; ===================================================================================================
;; Bitmaps (icons)

(define-icon-wrappers
  ([str string?]
   [font (is-a?/c font%) (make-font)]
   [#:trim? trim? boolean? #t]
   [#:color color (or/c string? (is-a?/c color%)) "white"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)]
   [#:outline outline (and/c rational? (>=/c 0)) (/ height 32)])
  (height outline)
  [text-icon text-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) "forestgreen"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [recycle-icon recycle-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) halt-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)]
   [#:thickness thickness (and/c rational? (>=/c 0)) 10])
  (height)
  [x-icon x-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) run-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [check-icon check-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) light-metal-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [lambda-icon lambda-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) "mediumseagreen"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [hash-quote-icon hash-quote-flomap])
