#lang typed/racket/base

(require (only-in typed/mred/mred Snip% Bitmap% Frame%)
         "../common/types.rkt"
         "../syntax.rkt")

(provide plot
         plot-file
         ;plot-pict  ; can't be typed yet
         plot-bitmap
         plot-snip
         plot-frame
         plot/dc)

(require/typed*
 plot
 
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
        -> (U (Instance Snip%) Void))]
 
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
 
 #;; Picts are from slideshow/pict, which isn't typed yet
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
 
 [plot-snip ((Treeof (U renderer2d nonrenderer))
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
             -> (Instance Snip%))]
 
 [plot-frame ((Treeof (U renderer2d nonrenderer))
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
              -> (Instance Frame%))]
 
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
 )
