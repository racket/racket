#lang typed/racket/base

(require (only-in typed/mred/mred Bitmap%)
         "../common/types.rkt"
         "../syntax.rkt")

(provide plot/dc
         plot-bitmap
         plot-pict
         plot-file)

(require/typed*
 plot/no-gui
 
 [plot/dc ((Treeof (U renderer2d nonrenderer))
           Any Real Real Real Real
           [#:x-min (Option Real)]
           [#:x-max (Option Real)]
           [#:y-min (Option Real)]
           [#:y-max (Option Real)]
           [#:title (Option String)]
           [#:x-label (Option String)]
           [#:y-label (Option String)]
           [#:legend-anchor Anchor]
           -> Void)]
 
 [plot-bitmap ((Treeof (U renderer2d nonrenderer))
               [#:x-min (Option Real)]
               [#:x-max (Option Real)]
               [#:y-min (Option Real)]
               [#:y-max (Option Real)]
               [#:width  Integer]
               [#:height Integer]
               [#:title (Option String)]
               [#:x-label (Option String)]
               [#:y-label (Option String)]
               [#:legend-anchor Anchor]
               -> (Instance Bitmap%))]
 
 [plot-pict ((Treeof (U renderer2d nonrenderer))
             [#:x-min (Option Real)]
             [#:x-max (Option Real)]
             [#:y-min (Option Real)]
             [#:y-max (Option Real)]
             [#:width  Integer]
             [#:height Integer]
             [#:title (Option String)]
             [#:x-label (Option String)]
             [#:y-label (Option String)]
             [#:legend-anchor Anchor]
             -> Pict)]
 
 [plot-file (((Treeof (U renderer2d nonrenderer))
              (U Path-String Output-Port))
             ((U 'auto Image-File-Format)
              [#:x-min (Option Real)]
              [#:x-max (Option Real)]
              [#:y-min (Option Real)]
              [#:y-max (Option Real)]
              [#:width  Integer]
              [#:height Integer]
              [#:title (Option String)]
              [#:x-label (Option String)]
              [#:y-label (Option String)]
              [#:legend-anchor Anchor])
             ->* Void)]
 )
