#lang typed/racket/base

(require (only-in typed/mred/mred Bitmap%)
         "../common/types.rkt"
         "../syntax.rkt")

(provide plot3d/dc
         plot3d-bitmap
         plot3d-pict
         plot3d-file)

(require/typed*
 plot/no-gui
 
 [plot3d/dc ((Treeof (U renderer3d nonrenderer))
             Any Real Real Real Real
             [#:x-min (Option Real)]
             [#:x-max (Option Real)]
             [#:y-min (Option Real)]
             [#:y-max (Option Real)]
             [#:z-min (Option Real)]
             [#:z-max (Option Real)]
             [#:angle Real]
             [#:altitude Real]
             [#:title (Option String)]
             [#:x-label (Option String)]
             [#:y-label (Option String)]
             [#:z-label (Option String)]
             [#:legend-anchor Anchor]
             -> Void)]
 
 [plot3d-bitmap ((Treeof (U renderer3d nonrenderer))
                 [#:x-min (Option Real)]
                 [#:x-max (Option Real)]
                 [#:y-min (Option Real)]
                 [#:y-max (Option Real)]
                 [#:z-min (Option Real)]
                 [#:z-max (Option Real)]
                 [#:width  Integer]
                 [#:height Integer]
                 [#:angle Real]
                 [#:altitude Real]
                 [#:title (Option String)]
                 [#:x-label (Option String)]
                 [#:y-label (Option String)]
                 [#:z-label (Option String)]
                 [#:legend-anchor Anchor]
                 -> (Instance Bitmap%))]
 
 [plot3d-pict ((Treeof (U renderer3d nonrenderer))
               [#:x-min (Option Real)]
               [#:x-max (Option Real)]
               [#:y-min (Option Real)]
               [#:y-max (Option Real)]
               [#:z-min (Option Real)]
               [#:z-max (Option Real)]
               [#:width  Integer]
               [#:height Integer]
               [#:angle Real]
               [#:altitude Real]
               [#:title (Option String)]
               [#:x-label (Option String)]
               [#:y-label (Option String)]
               [#:z-label (Option String)]
               [#:legend-anchor Anchor]
               -> Pict)]
 
 [plot3d-file (((Treeof (U renderer3d nonrenderer))
                (U Path-String Output-Port))
               ((U 'auto Image-File-Format)
                [#:x-min (Option Real)]
                [#:x-max (Option Real)]
                [#:y-min (Option Real)]
                [#:y-max (Option Real)]
                [#:z-min (Option Real)]
                [#:z-max (Option Real)]
                [#:width  Integer]
                [#:height Integer]
                [#:angle Real]
                [#:altitude Real]
                [#:title (Option String)]
                [#:x-label (Option String)]
                [#:y-label (Option String)]
                [#:z-label (Option String)]
                [#:legend-anchor Anchor])
               ->* Void)]
 )
