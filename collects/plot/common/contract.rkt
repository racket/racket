#lang racket/base

(require racket/contract racket/draw racket/class unstable/contract unstable/latent-contract
         unstable/latent-contract/defthing)

(provide (except-out (all-defined-out)
                     maybe-function/c maybe-apply
                     plot-colors/c pen-widths/c plot-pen-styles/c plot-brush-styles/c alphas/c
                     labels/c)
         (activate-contract-out
          maybe-function/c maybe-apply
          plot-colors/c pen-widths/c plot-pen-styles/c plot-brush-styles/c alphas/c
          labels/c)
         (rename-out [natural-number/c nat/c])
         truth/c)

;; ===================================================================================================
;; Plot-specific contracts

(defcontract anchor/c (one-of/c 'top-left    'top    'top-right
                                'left        'center 'right
                                'bottom-left 'bottom 'bottom-right))

(defcontract color/c (or/c (list/c real? real? real?)
                           string? symbol?
                           (is-a?/c color%)))

(defcontract plot-color/c (or/c exact-integer? color/c))

(defcontract plot-pen-style/c (or/c exact-integer?
                                    (one-of/c 'transparent 'solid    'dot 'long-dash
                                              'short-dash  'dot-dash)))

(defcontract plot-brush-style/c (or/c exact-integer?
                                      (one-of/c 'transparent      'solid
                                                'bdiagonal-hatch  'fdiagonal-hatch 'crossdiag-hatch
                                                'horizontal-hatch 'vertical-hatch  'cross-hatch)))

(defcontract font-family/c (one-of/c 'default 'decorative 'roman  'script 'swiss
                                     'modern  'symbol     'system))

(defthing known-point-symbols (listof symbol?) #:document-value
  (list 'dot               'point            'pixel
        'plus              'times            'asterisk
        '5asterisk         'odot             'oplus
        'otimes            'oasterisk        'o5asterisk
        'circle            'square           'diamond
        'triangle          'fullcircle       'fullsquare
        'fulldiamond       'fulltriangle     'triangleup
        'triangledown      'triangleleft     'triangleright
        'fulltriangleup    'fulltriangledown 'fulltriangleleft
        'fulltriangleright 'rightarrow       'leftarrow
        'uparrow           'downarrow        '4star
        '5star             '6star            '7star
        '8star             'full4star        'full5star
        'full6star         'full7star        'full8star
        'circle1           'circle2          'circle3
        'circle4           'circle5          'circle6
        'circle7           'circle8          'bullet
        'fullcircle1       'fullcircle2      'fullcircle3
        'fullcircle4       'fullcircle5      'fullcircle6
        'fullcircle7       'fullcircle8))

(defcontract point-sym/c (or/c char? string? integer? (apply one-of/c known-point-symbols)))

(defcontract (maybe-function/c [in-contract contract?] [out-contract contract?])
  (or/c out-contract (in-contract . -> . out-contract)))

(defproc (maybe-apply [f (maybe-function/c any/c any/c)]
                      [arg any/c]) any/c
  (cond [(procedure? f)  (f arg)]
        [else            f]))

(defcontract (plot-colors/c [in-contract contract?])
  (maybe-function/c in-contract (listof plot-color/c)))

(defcontract (pen-widths/c [in-contract contract?])
  (maybe-function/c in-contract (listof (>=/c 0))))

(defcontract (plot-pen-styles/c [in-contract contract?])
  (maybe-function/c in-contract (listof plot-pen-style/c)))

(defcontract (plot-brush-styles/c [in-contract contract?])
  (maybe-function/c in-contract (listof plot-brush-style/c)))

(defcontract (alphas/c [in-contract contract?])
  (maybe-function/c in-contract (listof (real-in 0 1))))

(defcontract (labels/c [in-contract contract?])
  (maybe-function/c in-contract (listof (or/c string? #f))))
