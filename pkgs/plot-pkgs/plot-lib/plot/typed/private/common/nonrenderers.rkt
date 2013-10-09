#lang typed/racket/base

(require "types.rkt")

(require/typed/provide
 plot/no-gui
 [x-ticks ((Listof tick) [#:far? Boolean] -> nonrenderer)]
 [y-ticks ((Listof tick) [#:far? Boolean] -> nonrenderer)]
 [z-ticks ((Listof tick) [#:far? Boolean] -> nonrenderer)]
 
 [invisible-rect ((Option Real)
                  (Option Real)
                  (Option Real)
                  (Option Real)
                  -> nonrenderer)]
 
 [invisible-rect3d ((Option Real)
                    (Option Real)
                    (Option Real)
                    (Option Real)
                    (Option Real)
                    (Option Real)
                    -> nonrenderer)]
 )
