#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/private/contracted/draw
 [->color (Color -> (List Real Real Real))]
 [->pen-color (Plot-Color -> (List Real Real Real))]
 [->brush-color (Plot-Color -> (List Real Real Real))]
 [->pen-style (Plot-Pen-Style -> Symbol)]
 [->brush-style (Plot-Brush-Style -> Symbol)]
 
 [color-seq (Color Color Integer [#:start? Boolean] [#:end? Boolean] 
                   -> (Listof (List Real Real Real)))]
 
 [color-seq* ((Listof Color) Integer [#:start? Boolean] [#:end? Boolean]
                             -> (Listof (List Real Real Real)))]
 
 [alpha-expt (Real Real -> Real)]
 )
