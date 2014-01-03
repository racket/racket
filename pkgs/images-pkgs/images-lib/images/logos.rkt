#lang racket/base

(require racket/class racket/draw racket/math racket/match
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "private/flomap.rkt"
         "private/deep-flomap.rkt"
         "private/utils.rkt"
         "icons/symbol.rkt"
         "icons/misc.rkt"
         "icons/style.rkt")

(provide (activate-contract-out
          plt-logo plt-flomap
          planet-logo planet-flomap
          racket-logo racket-flomap
          stepper-logo stepper-flomap
          macro-stepper-logo macro-stepper-logo-flomap)
         (only-doc-out (all-defined-out)))

;; ===================================================================================================
;; PLT logo

(define glass-logo-material
  (deep-flomap-material-value
   'cubic-zirconia 0.7 0.6 0.4
   0.2 0.1 1.0
   0.2 0.1 0.1
   0.0))

(define lambda-path-commands
  '((m 97.5 10)
    (c -12.267574371681416 0.22160039646017698
       -23.938206584070794 4.486409061946903
       -35.40358116814159 8.431642279646018
       5.002013451327434 5.357118980530973
       4.2474160707964606 7.049306166371681
       6.430565946902655 6.642370378761062
       8.354521486725664 -2.0234602477876105
       20.745877522123894 -6.732496424778761
       26.26655603539823 2.1904900530973452
       12.036272707964603 15.204891185840708
       17.140790371681415 34.66372757522124
       18.964158300884954 53.635833203539825
       2.3373978053097346 11.526810053097345
       -21.433330407079644 52.79757139823009
       -28.736806513274335 69.27072283185841
       -11.871354336283186 20.946142017699113
       -22.417494088495573 42.68413054867256
       -35.79320863716814 62.74737614159292
       3.198686017699115 4.233302088495575
       7.820428460176991 2.5766558584070793
       12.171064637168142 1.925754336283186
       3.714682336283186 -0.5565213451327433
       7.429373734513274 -1.1130336283185842
       11.14405607079646 -1.6695504424778762
       11.979952707964602 -28.4038887079646
       24.914903221238937 -54.476528141592915
       36.156529274336286 -83.1860083539823
       5.122632495575221 -11.831699256637167
       7.625016637168141 -18.33969500884956
       13.711282973451327 -26.087614300884955
       7.215226336283186 4.414282761061947
       9.363369911504424 15.302112283185838
       13.299630442477875 22.814352991150443
       11.600370407079646 29.849948884955747
       23.150614654867255 59.71926315044247
       34.09924077876106 89.81329104424779
       2.8656957168141592 0.9979197168141594
       5.806954477876106 3.9796174159292033
       8.525185132743362 1.105811256637168
       7.150265769911504 -4.4088093451327435
       15.474823929203538 -7.170211115044248
       21.428106194690265 -13.26414385840708
       -1.6986936637168142 -8.23685210619469
       -7.156455079646018 -15.941115469026549
       -10.48132417699115 -23.86248042477876
       -21.07570067256637 -42.11971171681416
       -41.39651398230088 -86.79632424778761
       -54.5885927079646 -132.15014060176992
       -4.858603610619468 -14.274800141592921
       -10.841368920353982 -31.4765361840708
       -26.303927504424777 -37.111590060176994
       -3.5224240707964602 -1.0457545628318583
       -7.2342065840707965 -1.2467313274336282
       -10.888935079646018 -1.2461164743362831)))

(define (draw-lambda dc x y w h)
  (define-values (sx sy) (send dc get-scale))
  (draw-path-commands dc (scale-path-commands lambda-path-commands (/ w 240) (/ h 240)) x y)
  (send dc set-scale sx sy))

(define blue-θ-start (* -45 (/ pi 180)))
(define blue-θ-end (* 110 (/ pi 180)))

(define logo-red-color (make-object color% 255 36 32))
(define logo-blue-color (make-object color% 32 36 255))
(define lambda-outline-color (make-object color% 16 16 64))
(define (lambda-pen color width) (make-object pen% color width 'solid 'projecting 'miter))

(define (make-arc-path x y w h start end [ccw? #t])
  (define p (new dc-path%))
  (send p arc x y w h start end ccw?)
  (send p close)
  p)

(define (make-random-flomap c w h)
  (build-flomap c w h (λ (k x y i) (random))))

(define (flomap-rough fm z-amt)
  (match-define (flomap _ c w h) fm)
  (fm+ fm (fm* z-amt (make-random-flomap c w h))))

(defproc (plt-flomap [#:height height (and/c rational? (>=/c 0)) 256]) flomap?
  (make-cached-flomap
   [height]
   (define scale (/ height 256))
   (define bulge-fm
     (draw-icon-flomap
      (λ (dc)
        (send dc set-pen logo-red-color 2 'transparent)
        (send dc set-brush logo-red-color 'solid)
        (send dc draw-path (make-arc-path 8 8 239 239 blue-θ-end blue-θ-start))
        (send dc set-pen logo-blue-color 2 'transparent)
        (send dc set-brush logo-blue-color 'solid)
        (send dc draw-path (make-arc-path 8 8 239 239 blue-θ-start blue-θ-end))
        (send dc set-pen (lambda-pen lambda-outline-color 10))
        (send dc set-brush lambda-outline-color 'solid)
        (draw-lambda dc 8 8 240 240))
      256 256 scale))
   
   (define (lambda-flomap color pen-width)
     (draw-icon-flomap
      (λ (dc)
        (send dc set-scale scale scale)
        (send dc set-pen (lambda-pen color pen-width))
        (send dc set-brush color 'solid)
        (draw-lambda dc 8 8 240 240))
      256 256 scale))
   
   (let* ([bulge-dfm  (flomap->deep-flomap bulge-fm)]
          [bulge-dfm  (deep-flomap-bulge-spheroid bulge-dfm (* 112 scale))]
          [lambda-dfm  (flomap->deep-flomap (lambda-flomap "azure" 4))]
          [lambda-dfm  (deep-flomap-bulge-spheroid lambda-dfm (* 112 scale))]
          [lambda-dfm  (deep-flomap-smooth-z lambda-dfm (* 3 scale))]
          [lambda-fm  (deep-flomap-render-icon lambda-dfm metal-material)]
          [fm  (deep-flomap-render-icon bulge-dfm glass-logo-material)]
          [fm  (flomap-cc-superimpose
                fm
                (lambda-flomap lambda-outline-color 10)
                lambda-fm)]
          [fm  (flomap-cc-superimpose
                (draw-icon-flomap
                 (λ (dc)
                   (send dc set-pen lambda-outline-color 1/2 'solid)
                   (send dc set-brush "white" 'solid)
                   (send dc draw-ellipse -0.25 -0.25 31.5 31.5)
                   (send dc set-pen "lightblue" 1/2 'solid)
                   (send dc set-brush "white" 'transparent)
                   (send dc draw-ellipse 0.5 0.5 30 30))
                 32 32 (/ height 32))
                fm)])
     fm)))

;; ===================================================================================================
;; Planet logo

(define continents-path-commands
  '((m 11.526653 18.937779)
    (c 0.05278 0.724075 1.940414 1.202607 0.678885 2.296248
       0.249172 0.918181 1.040063 1.620575 1.448285 0.308034
       1.219485 -0.885607 3.250882 -0.938443 3.317014 -2.906655
       -1.599965 -1.033954 -4.029479 -0.431148 -5.444184 0.302373)
    (M 11.53125 18.125)
    (C 10.786965 18.380649 9.3917452 18.611001 9.1304904 19.245707
       10.289001 19.269837 11.178405 18.606302 11.53125 18.125)
    (M 8.1875 19.65625)
    (C 7.2652998 23.370888 8.6787734 19.63772 9.9124431 20.95891
       10.727811 21.80382 11.739516 20.92275 10.465247 20.422456
       9.7714766 19.980166 8.3964342 19.699414 8.1875 19.65625)
    (M 7.5625 21.125)
    (c -0.9196331 -1.962382 -3.205955 1.390782 -4.0978229 2.41995
       -1.707808 2.289408 -2.72190385 5.078558 -2.9334271 7.9238
       1.0237952 1.983695 5.5272247 2.76676 4.7145431 4.084262
       -0.7368064 1.151552 -0.8906555 2.601652 0.1135446 3.680893
       2.7495495 2.364498 1.2541019 5.824595 2.5609489 6.229519
       2.5755284 0.853846 2.7512924 -3.696022 4.1297234 -3.843434
       0.745066 -1.051147 0.04765 -2.428466 1.056101 -3.411232)
    (C 12.318556 36.222109 8.8169859 35.479018 8.6188979 33.8253
       7.7181807 34.141675 7.0679715 33.334232 6.30372 33.30415
       5.7220663 34.646967 3.9378253 34.122031 4.3012403 32.699798
       3.024533 33.043038 4.3605584 31.222879 3.40625 31.28125
       0.5 33 2.5 26.5 5.0295875 29.903027
       5.5 30.5 6.9002733 26.371666 8.8261905 25.876953
       9.8027554 25.533149 9.5159021 24.727855 8.5279357 25.0625
       7.6214946 24.941384 9.6975411 24.462771 10.075856 24.483273
       11.540792 24.233047 9.904685 23.334106 9.8601011 22.602389
       9.0900535 22.676405 9.4028275 22.737933 9.1185443 22.100147
       6.8948741 22.58513 7.6831847 24.739145 5.9002404 23.244912
       4.6247757 22.264239 7.321322 21.942832 7.5625 21.125)
    (m 15.15625 -0.9375)
    (c -1.37421 0.06218 -2.005432 1.159129 -2.784107 1.978327
       -0.114565 1.368674 0.952693 -0.07002 1.385771 0.968032
       0.953881 -0.129572 -0.01507 -1.993413 1.425543 -2.008859
       -0.269351 0.525838 -0.494795 1.470731 0.411144 1.15174
       -0.646943 0.90275 -1.874871 2.045333 -2.613442 0.960703
       0.08813 0.809648 -1.042388 0.509104 -1.186702 1.40851
       -0.738698 0.338761 -1.028513 0.375271 -0.383294 1.119927
       -1.340908 -0.226887 -1.979854 2.002883 -0.346874 1.903539
       3.128783 -3.578714 2.7333 -0.07275 3.379252 -0.61531
       -0.408321 -3.069544 0.823059 1.69915 1.30948 -0.328623
       0.476726 0.916648 1.583858 0.757279 2.129612 1.386838
       -2.140558 2.214946 -4.171988 -1.055384 -6.363065 -0.232922
       -2.486751 0.823935 -2.418258 3.347586 -3.103635 4.864439
       0.687061 3.597921 3.669743 1.43585 5.132502 2.724104
       -0.344691 1.08929 0.484513 1.884668 0.473244 3.022942
       -0.01352 2.068761 0.378264 6.65826 1.845318 5.542497
       1.472489 0.175399 1.430793 -1.740909 2.30904 -2.30502
       -1.36358 -1.181833 2.025569 -1.358588 0.887958 -2.838158
       -0.499809 -1.988948 1.367195 -3.177085 1.789594 -4.928946
       0.579613 -0.960476 -1.588234 -0.05789 -0.373062 -1.023304
       0.927113 -0.301781 2.379761 -2.07879 0.994298 -2.428506
       -0.676988 0.933612 -1.737597 -2.080985 -0.549773 -0.651497
       0.699549 -0.419557 1.900516 1.563553 1.759683 -0.08984
       -0.608903 -3.386912 -2.4601 -6.520148 -5.090986 -8.736865
       -0.200722 0.802307 -1.230158 0.889683 -1.228926 0.0694
       2.155263 -0.50116 -0.789058 -0.572123 -1.208573 -0.913148)
    (M 17.09375 21)
    (c -1.221276 0.05745 -0.44882 1.331427 0.232503 0.449916)
    (C 17.458514 21.23484 17.234278 21.104353 17.09375 21)
    (m -7.5 0.125)
    (c -1.2040413 0.60218 1.459244 1.052142 0.289004 0.112253)
    (m 8.96875 1.5)
    (c 0.38412 0.655402 -0.236077 2.74213 1.030518 1.55154
       0.0634 -0.524592 -0.59842 -1.401743 -1.030518 -1.55154)
    (m -0.21875 0.75)
    (c -1.155615 0.198578 0.509999 1.388302 0.06733 0.201634)
    (M 10.5 24.53125)
    (c -0.117519 1.313533 1.058399 0.642504 0 0)))

(define water-logo-material
  (deep-flomap-material-value
   'cubic-zirconia 1.0 0.7 1.0
   0.25 0.15 1.0
   0.15 0.1 0.2
   0.0))

(define logo-under-continents-color "black")
(define logo-continents-color "azure")
(define logo-water-color "lightskyblue")
(define logo-earth-outline-color logo-red-color)

(define (continents-flomap color height)
  (define scale (/ height 32))
  (draw-icon-flomap
   (λ (dc)
     (send dc set-pen lambda-outline-color 3/8 'solid)
     (send dc set-brush color 'solid)
     (draw-path-commands dc continents-path-commands 0 -17))
   32 32 scale))

(defproc (planet-flomap [#:height height (and/c rational? (>=/c 0)) 256]) flomap?
  (make-cached-flomap
   [height]
   (define scale (/ height 32))
   (define earth-fm
     (let* ([indent-fm  (continents-flomap logo-red-color height)]
            [indent-dfm  (flomap->deep-flomap indent-fm)]
            [indent-dfm  (deep-flomap-raise indent-dfm (* -1/8 scale))]
            [indent-dfm  (deep-flomap-smooth-z indent-dfm (* 1 scale))]
            [earth-fm  (draw-icon-flomap
                        (λ (dc)
                          (send dc set-pen logo-water-color 1/2 'solid)
                          (send dc set-brush logo-water-color 'solid)
                          (send dc draw-ellipse 0.75 0.75 29.5 29.5))
                        32 32 scale)]
            [earth-dfm  (flomap->deep-flomap earth-fm)]
            [earth-dfm  (deep-flomap-bulge-spheroid earth-dfm (* 16 scale))]
            [earth-dfm  (deep-flomap-cc-superimpose 'add earth-dfm indent-dfm)])
       (deep-flomap-render-icon earth-dfm water-logo-material)))
   
   (define land-fm
     (let* ([land-fm  (continents-flomap logo-continents-color height)]
            [land-dfm  (flomap->deep-flomap land-fm)]
            ;[land-dfm  (deep-flomap-emboss land-dfm (* 2 scale) (* 8 scale))]
            [land-dfm  (deep-flomap-bulge-spheroid land-dfm (* 16 scale))]
            [land-dfm  (deep-flomap-smooth-z land-dfm (* 1/2 scale))])
       (deep-flomap-render-icon land-dfm metal-material)))
   
   (flomap-cc-superimpose
    (draw-icon-flomap
     (λ (dc)
       (send dc set-pen lambda-outline-color 1/2 'solid)
       (send dc set-brush "white" 'solid)
       (send dc draw-ellipse -0.25 -0.25 31.5 31.5)
       (send dc set-pen "lightblue" 1/2 'solid)
       (send dc set-brush "white" 'transparent)
       (send dc draw-ellipse 0.5 0.5 30 30))
     32 32 scale)
    earth-fm
    land-fm)))

;; ===================================================================================================
;; Algebraic stepper logo

(defproc (stepper-flomap [#:height height (and/c rational? (>=/c 0)) 96]) flomap?
  (flomap-pin*
   1/2 20/32 1/2 1/2
   (foot-flomap #:color "forestgreen" #:height height #:material glass-icon-material)
   (lambda-flomap #:color light-metal-icon-color
                  #:height (* 5/8 height) #:material metal-icon-material)))

;; ===================================================================================================
;; Macro stepper logo

(defproc (macro-stepper-logo-flomap [#:height height (and/c rational? (>=/c 0)) 96]) flomap?
  (flomap-pin*
   1/2 20/32 15/36 1/2
   (foot-flomap #:color (make-object color% 34 42 160) #:height height #:material glass-icon-material)
   (hash-quote-flomap #:color light-metal-icon-color
                      #:height (* 1/2 height)
                      #:material metal-icon-material)))

;; ===================================================================================================
;; Racket logo

(define racket-r-commands
  (scale-path-commands
   '((m 4 76)
     (c 12 28 20 56 28 92
        5.560411 25.02185 4 44.00002 12 76.00002
        20 0 39.835333 -8 56 -24
        -20 -36.00004 -28 -72.00002 -28 -108.00002
        60 -40 96 -44 144 -40
        6 -12 16.19861 -35.94773 16 -48
        -60 -4 -112 4 -168 48
        -1 -12 -0.958295 -20 0 -28
        -28 4 -44 12 -60 32))
   1/8 1/8))

(define racket-r-outline-color (make-object color% 64 16 16))

(define (racket-r-flomap color height)
  (draw-icon-flomap
   (λ (dc)
     (set-icon-pen dc racket-r-outline-color 3/8 'solid)
     (send dc set-brush color 'solid)
     (draw-path-commands dc racket-r-commands 0 0))
   32 32 (/ height 32)))

(define racket-sphere-material
  (deep-flomap-material-value
   'cubic-zirconia 0.75 0.75 0.75
   3.5 0.25 0.25
   0.5 0.25 0.0
   0.01))

(defproc (racket-flomap [#:height height (and/c rational? (>=/c 0)) 256]) flomap?
  (make-cached-flomap
   [height]
   (define scale (/ height 32))
   (define sphere-fm
     (let* ([indent-fm  (racket-r-flomap racket-r-outline-color height)]
            [indent-dfm  (flomap->deep-flomap indent-fm)]
            [indent-dfm  (deep-flomap-raise indent-dfm (* -0.75 scale))]
            [indent-dfm  (deep-flomap-smooth-z indent-dfm (* 0.5 scale))]
            [sphere-fm  (draw-icon-flomap
                         (λ (dc)
                           (define top-rgn (make-object region% dc))
                           (send top-rgn set-polygon
                                 '((0 . 0) (31 . 0) (31 . 4) (5 . 13) (8 . 31) (0 . 31)))
                           
                           (send dc set-pen logo-blue-color 1/2 'solid)
                           (send dc set-brush logo-blue-color 'solid)
                           (send dc draw-ellipse 0.75 0.75 29.5 29.5)
                           
                           (send dc set-clipping-region top-rgn)
                           (send dc set-pen logo-red-color 1/2 'solid)
                           (send dc set-brush logo-red-color 'solid)
                           (send dc draw-ellipse 0.75 0.75 29.5 29.5))
                         32 32 scale)]
            [sphere-dfm  (flomap->deep-flomap sphere-fm)]
            [sphere-dfm  (deep-flomap-bulge-spheroid sphere-dfm (* 14 scale))]
            [sphere-dfm  (deep-flomap-cc-superimpose 'add sphere-dfm indent-dfm)])
       (deep-flomap-render-icon sphere-dfm glass-logo-material)))
   
   (define r-fm
     (let* ([r-fm  (racket-r-flomap light-metal-icon-color height)]
            [r-dfm  (flomap->deep-flomap r-fm)]
            [r-dfm  (deep-flomap-bulge-round r-dfm (* 48 scale))]
            [r-dfm  (deep-flomap-smooth-z r-dfm (* 1/2 scale))])
       (deep-flomap-render-icon r-dfm metal-material)))
   
   (flomap-cc-superimpose
    (draw-icon-flomap
     (λ (dc)
       (send dc set-pen racket-r-outline-color 1/2 'solid)
       (send dc set-brush "white" 'solid)
       (send dc draw-ellipse -0.25 -0.25 31.5 31.5)
       (send dc set-pen "lightblue" 1/2 'solid)
       (send dc set-brush "white" 'transparent)
       (send dc draw-ellipse 0.5 0.5 30 30))
     32 32 scale)
    sphere-fm
    r-fm)))

;; ===================================================================================================
;; Bitmaps

(define-icon-wrappers
  ([#:height height (and/c rational? (>=/c 0)) 256])
  (height)
  [plt-logo plt-flomap]
  [racket-logo racket-flomap])

(define-icon-wrappers
  ([#:height height (and/c rational? (>=/c 0)) 96])
  (height)
  [planet-logo planet-flomap]
  [stepper-logo stepper-flomap]
  [macro-stepper-logo macro-stepper-logo-flomap])
