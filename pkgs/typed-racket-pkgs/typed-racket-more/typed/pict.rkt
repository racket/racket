#lang s-exp typed-racket/base-env/extra-env-lang

;; Typed base-env wrapper for the pict library

(require pict
         (for-syntax (only-in typed-racket/rep/type-rep
                              make-Name
                              make-Union)))

(begin-for-syntax
 (define -pict (make-Name #'pict null #f #t))
 (define -pict-path
   (make-Union (list (-val #f) -pict (-lst -pict))))
 (define -child (make-Name #'child null #f #t))
 (define -linestyle
   (one-of/c 'transparent 'solid 'xor 'hilite
             'dot 'long-dash 'short-dash 'dot-dash
             'xor-dot 'xor-long-dash 'xor-short-dash
             'xor-dot-dash))
 (define -pin-arrow-line
   (->key -Real
          -pict
          -pict-path
          (-> -pict -pict-path (-values (list -Real -Real)))
          -pict-path
          (-> -pict -pict-path (-values (list -Real -Real)))
          #:start-angle (-opt -Real) #f
          #:end-angle (-opt -Real) #f
          #:start-pull -Real #f
          #:end-pull -Real #f
          #:line-width (-opt -Real) #f
          ;; FIXME: color%
          #:color (-opt (Un -String)) #f
          #:alpha -Real #f
          #:style -linestyle #f
          #:under? Univ #f
          #:solid? Univ #f
          #:hide-arrowhead? Univ #f
          -pict))
 (define -pict-finder
   (-> -pict -pict-path (-values (list -Real -Real))))
 (define -append-type
   (cl->*
     (->* (list -Real -pict) -pict -pict)
     (->* (list -pict) -pict -pict)))
 (define -superimpose-type
   (->* (list -pict) -pict -pict)))

(type-environment
 [#:struct pict ([draw : Univ]
                 [width : -Real]
                 [height : -Real]
                 [ascent : -Real]
                 [children : (-lst -child)]
                 [panbox : Univ]
                 [last : -pict-path])
           #:extra-constructor-name make-pict]
 [#:struct child ([pict : -pict]
                  [dx : -Real]
                  [dy : -Real]
                  [sx : -Real]
                  [sy : -Real]
                  [sxy : -Real]
                  [syx : -Real])
           #:extra-constructor-name make-child]

 ;; dc
 [blank (cl->* (-> -pict)
               (-> -Real -pict)
               (-> -Real -Real -pict)
               (-> -Real -Real -Real -pict)
               (-> -Real -Real -Real -Real -pict))]
 ;; text
 [hline (->key -Real -Real #:segment (-opt -Real) #f -pict)]
 [vline (->key -Real -Real #:segment (-opt -Real) #f -pict)]
 [frame (->key -pict
               #:segment (-opt -Real) #f
               ;; FIXME: add color% with class support
               #:color (-opt -String) #f
               #:width (-opt -Real) #f
               -pict)]
 [ellipse (-> -Real -Real -pict)]
 [circle (-> -Real -pict)]
 [filled-ellipse
  (->key -Real -Real #:draw-border? Univ #f -pict)]
 [disk
  (->key -Real #:draw-border? Univ #f -pict)]
 [rectangle (-> -Real -Real -pict)]
 [filled-rectangle
  (->key -Real -Real #:draw-border? Univ #f -pict)]
 [rounded-rectangle
  (->optkey -Real -Real [-Real] #:angle -Real #f -pict)]
 [filled-rounded-rectangle
  (->optkey -Real -Real [-Real]
            #:angle -Real #f
            #:draw-border? Univ #f
            -pict)]
 ;; FIXME: add bitmap% and image-snip%
 [bitmap (-> (Un -Pathlike) -pict)]
 [arrow (-> -Real -Real -pict)]
 [arrowhead (-> -Real -Real -pict)]
 [pip-line (-> -Real -Real -Real -pict)]
 [pip-arrow-line (-> -Real -Real -Real -pict)]
 [pip-arrows-line (-> -Real -Real -Real -pict)]
 [pin-line
  (->key -pict
         -pict-path
         (-> -pict -pict-path (-values (list -Real -Real)))
         -pict-path
         (-> -pict -pict-path (-values (list -Real -Real)))
         #:start-angle (-opt -Real) #f
         #:end-angle (-opt -Real) #f
         #:start-pull -Real #f
         #:end-pull -Real #f
         #:line-width (-opt -Real) #f
         ;; FIXME: color%
         #:color (-opt (Un -String)) #f
         #:alpha -Real #f
         #:style -linestyle #f
         #:under? Univ #f
         -pict)]
 [pin-arrow-line -pin-arrow-line]
 [pin-arrows-line -pin-arrow-line]
 [bitmap-draft-mode (-Param Univ Univ)]

 ;; 3 Pict Combiners
 [vl-append -append-type]
 [vc-append -append-type]
 [vr-append -append-type]
 [ht-append -append-type]
 [htl-append -append-type]
 [hc-append -append-type]
 [hbl-append -append-type]
 [hb-append -append-type]

 [lt-superimpose -superimpose-type]
 [ltl-superimpose -superimpose-type]
 [lc-superimpose -superimpose-type]
 [lbl-superimpose -superimpose-type]
 [lb-superimpose -superimpose-type]
 [ct-superimpose -superimpose-type]
 [ctl-superimpose -superimpose-type]
 [cc-superimpose -superimpose-type]
 [cbl-superimpose -superimpose-type]
 [cb-superimpose -superimpose-type]
 [rt-superimpose -superimpose-type]
 [rtl-superimpose -superimpose-type]
 [rc-superimpose -superimpose-type]
 [rbl-superimpose -superimpose-type]
 [rb-superimpose -superimpose-type]

 [pin-over
  (cl->*
    (-> -pict -Real -Real -pict)
    (-> -pict -pict-path
        (-> -pict -pict-path (-values (list -Real -Real)))
        -pict))]
 [pin-under
  (cl->*
    (-> -pict -Real -Real -pict)
    (-> -pict -pict
        (-> -pict -pict (-values (list -Real -Real)))
        -pict))]

 ;; FIXME: table has a weird type

 ;; 4 Pict Drawing Adjusters
 [scale
  (cl->* (-> -pict -Real -pict)
         (-> -pict -Real -Real -pict))]
 [scale-to-fit
  (cl->* (-> -pict -Real -pict)
               (-> -pict -Real -Real -pict))]
 [rotate (-> -pict -Real -pict)]
 [ghost (-> -pict -pict)]
 [linewidth (-> (-opt -Real) -pict -pict)]
 [linestyle (-> -linestyle -pict -pict)]
 [colorize (-> -pict (Un -String (-lst* -Integer -Integer -Integer)) -pict)]
 [cellophane (-> -pict -Real -pict)]
 [clip (-> -pict -pict)]
 [inset/clip
  (cl->* (-> -pict -Real -pict)
         (-> -pict -Real -Real -pict)
         (-> -pict -Real -Real -Real -Real -pict))]
 [black-and-white (-Param Univ Univ)]

 ;; 5 Bounding Box Adjusters
 [inset
  (cl->* (-> -pict -Real -pict)
         (-> -pict -Real -Real -pict)
         (-> -pict -Real -Real -Real -Real -pict))]
 [clip-descent (-> -pict -pict)]
 [lift-above-baseline (-> -pict -Real -pict)]
 [drop-below-ascent (-> -pict -Real -pict)]
 [baseless (-> -pict -pict)]
 [refocus (-> -pict -pict -pict)]
 [panorama (-> -pict -pict)]
 [use-last (-> -pict -pict-path -pict)]
 [use-last* (-> -pict -pict-path -pict)]

 ;; 6 Pict Finders
 [lt-find -pict-finder]
 [ltl-find -pict-finder]
 [lc-find -pict-finder]
 [lbl-find -pict-finder]
 [lb-find -pict-finder]
 [ct-find -pict-finder]
 [ctl-find -pict-finder]
 [cbl-find -pict-finder]
 [cb-find -pict-finder]
 [rt-find -pict-finder]
 [rtl-find -pict-finder]
 [rc-find -pict-finder]
 [rbl-find -pict-finder]
 [rb-find -pict-finder]
 [pict-path? (make-pred-ty -pict-path)]
 [launder (-> -pict -pict)]

 ;; 7.1 Dingbats
 ;; FIXME: color% for many of these
 [cloud (->opt -Real -Real [-String] -pict)]
 [file-icon (->opt -Real -Real -String [Univ] -pict)]
 [standard-fish
  (->key -Real -Real
         #:direction (one-of/c 'left 'right) #f
         #:color -String #f
         #:eye-color (-opt -String) #f
         #:open-mouth (Un -Boolean -Real) #f
         -pict)]
 [jack-o-lantern (->opt -Real [-String -String] -pict)]
 [angel-wing (-> -Real -Real Univ -pict)]
 [desktop-machine (->opt -Real [(-lst (one-of/c 'plt 'binary 'devil))] -pict)]
 ;; thermometer

 ;; 8 Animation Helpers
 [fade-pict (->key -Real -pict -pict
                   #:combine (-> -pict -pict -pict) #f
                   -pict)]
 [fade-around-pict (-> -Real -pict (-> -pict -pict) -pict)]
 [slide-pict (-> -pict -pict -pict -pict -Real -pict)]
 [sequence-animations (->* '() (-> -Real -pict) (-> -Real -pict))]
 [reverse-animations (->* '() (-> -Real -pict) (-> -Real -pict))]
 [fast-start (-> -Real -Real)]
 [fast-end (-> -Real -Real)]
 [fast-edges (-> -Real -Real)]
 [fast-middle (-> -Real -Real)]
 [split-phase (-> -Real (-values (list -Real -Real)))]

 ;; 10 Miscellaneous
 [hyperlinkize (-> -pict -pict)]
 ;; FIXME:
 ;; scale-color
 ;; color-series
 )
