#lang typed/racket/base

(require (only-in typed/mred/mred Snip% Bitmap% Frame%)
         "../common/types.rkt"
         "../syntax.rkt")

(provide plot3d
         plot3d-file
         ;plot3d-pict  ; can't be typed yet
         plot3d-bitmap
         plot3d-snip
         plot3d-frame
         plot3d/dc)

(require/typed*
 plot
 
 [plot3d ((Treeof (U renderer3d nonrenderer))
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
          [#:out-file (Option (U Path-String Output-Port))]
          [#:out-kind (U 'auto Image-File-Format)]
          -> (U (Instance Snip%) Void))]
 
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
 
 #;; Picts are from slideshow/pict, which isn't typed yet
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
 
 [plot3d-snip ((Treeof (U renderer3d nonrenderer))
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
               -> (Instance Snip%))]
 
 [plot3d-frame ((Treeof (U renderer3d nonrenderer))
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
                -> (Instance Frame%))]
 
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
 )
