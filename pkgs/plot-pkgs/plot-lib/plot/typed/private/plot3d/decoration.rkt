#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide point-label3d)

(require/typed*
 plot/no-gui
 
 [point-label3d (((Sequenceof Real))
                 ((Option String)
                  [#:color Plot-Color]
                  [#:size Real]
                  [#:family Font-Family]
                  [#:anchor Anchor]
                  [#:angle Real]
                  [#:point-color Plot-Color]
                  [#:point-fill-color (U Plot-Color 'auto)]
                  [#:point-size Real]
                  [#:point-line-width Real]
                  [#:point-sym Point-Sym]
                  [#:alpha Real])
                 ->* renderer3d)]
 )
