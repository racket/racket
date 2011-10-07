#lang racket/base

(require racket/contract racket/draw racket/class
         "contract-doc.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Conveniences

(defcontract (real>=/c [r real?]) (and/c real? (>=/c r)))

(defcontract (integer>=/c [i integer?]) (and/c integer? (>=/c i)))

(defproc (treeof [contract (or/c contract? (any/c . -> . any/c))]) contract?
  (or/c contract (listof (recursive-contract (treeof contract)))))

;; ===================================================================================================
;; Plot-specific contracts

(defcontract anchor/c (one-of/c 'top-left 'top 'top-right
                                'left 'center 'right
                                'bottom-left 'bottom 'bottom-right))

(defcontract color/c (or/c (list/c real? real? real?)
                           string? symbol?
                           (is-a?/c color%)))

(defcontract plot-color/c (or/c exact-integer? color/c))

(defcontract pen-style/c (one-of/c 'transparent 'solid 'dot 'long-dash
                                   'short-dash 'dot-dash))

(defcontract plot-pen-style/c (or/c exact-integer? pen-style/c))

(defcontract brush-style/c (one-of/c 'transparent 'solid
                                     'bdiagonal-hatch 'fdiagonal-hatch 'crossdiag-hatch
                                     'horizontal-hatch 'vertical-hatch 'cross-hatch))

(defcontract plot-brush-style/c (or/c exact-integer? brush-style/c))

(defcontract plot-font-size/c (real>=/c 0))

(defcontract font-family/c (one-of/c 'default 'decorative 'roman 'script 'swiss
                                     'modern 'symbol 'system))

(define known-point-symbols
  '(dot point pixel
        plus times asterisk 5asterisk
        odot oplus otimes oasterisk o5asterisk
        circle square diamond triangle
        fullcircle fullsquare fulldiamond fulltriangle
        triangleup triangledown triangleleft triangleright
        fulltriangleup fulltriangledown fulltriangleleft fulltriangleright
        rightarrow leftarrow uparrow downarrow
        4star 5star 6star 7star 8star
        full4star full5star full6star full7star full8star
        circle1 circle2 circle3 circle4 circle5 circle6 circle7 circle8
        bullet fullcircle1 fullcircle2 fullcircle3 fullcircle4
        fullcircle5 fullcircle6 fullcircle7 fullcircle8))

(defcontract point-sym/c (or/c char? string? integer? (apply one-of/c known-point-symbols)))

(defcontract plot-colors/c (or/c (listof plot-color/c)
                                 ((listof real?) . -> . (listof plot-color/c))))

(defcontract pen-widths/c (or/c (listof (real>=/c 0))
                                ((listof real?) . -> . (listof (real>=/c 0)))))

(defcontract plot-pen-styles/c (or/c (listof plot-pen-style/c)
                                     ((listof real?) . -> . (listof plot-pen-style/c))))

(defcontract plot-brush-styles/c (or/c (listof plot-brush-style/c)
                                       ((listof real?) . -> . (listof plot-brush-style/c))))

(defcontract alphas/c (or/c (listof (real-in 0 1))
                            ((listof real?) . -> . (listof (real-in 0 1)))))
