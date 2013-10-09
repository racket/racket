#lang typed/racket/base

(require (only-in typed/mred/mred Bitmap%)
         "../common/types.rkt"
         "../syntax.rkt")

(provide plot plot3d)

(require/typed*
 plot/bitmap
 
 [plot ((Treeof (U renderer2d nonrenderer))
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
        [#:out-file (Option (U Path-String Output-Port))]
        [#:out-kind (U 'auto Image-File-Format)]
        -> (Instance Bitmap%))]
 
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
          -> (Instance Bitmap%))]
 )
