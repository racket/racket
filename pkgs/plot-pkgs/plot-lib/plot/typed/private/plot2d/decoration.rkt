#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide x-axis y-axis axes polar-axes
         x-tick-lines y-tick-lines tick-grid
         point-label parametric-label polar-label function-label inverse-label)

(require/typed*
 plot/no-gui
 
 [x-axis (()
          (Real
           [#:ticks? Boolean]
           [#:labels? Boolean]
           [#:far? Boolean]
           [#:alpha Real])
          ->* renderer2d)]
 
 [y-axis (()
          (Real
           [#:ticks? Boolean]
           [#:labels? Boolean]
           [#:far? Boolean]
           [#:alpha Real])
          ->* renderer2d)]
 
 [axes (()
        (Real
         Real
         [#:x-ticks? Boolean]
         [#:y-ticks? Boolean]
         [#:x-labels? Boolean]
         [#:y-labels? Boolean]
         [#:x-alpha Real]
         [#:y-alpha Real])
        ->* (Listof renderer2d))]
 
 [polar-axes ([#:number Integer]
              [#:ticks? Boolean]
              [#:labels? Boolean]
              [#:alpha Real]
              -> renderer2d)]
 
 [x-tick-lines (-> renderer2d)]
 [y-tick-lines (-> renderer2d)]
 [tick-grid (-> (Listof renderer2d))]
 
 [point-label (((Sequenceof Real))
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
               ->* renderer2d)]
 
 [parametric-label (((Real -> (Sequenceof Real)) Real)
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
                    ->* renderer2d)]
 
 [polar-label (((Real -> Real) Real)
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
               ->* renderer2d)]
 
 [function-label (((Real -> Real) Real)
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
                  ->* renderer2d)]
 
 [inverse-label (((Real -> Real) Real)
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
                 ->* renderer2d)]
 )
