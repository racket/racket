#lang typed/racket/base

(require (only-in typed/mred/mred Snip% Frame%)
         plot/typed/private/common/types
         plot/typed/private/syntax)

(provide plot-snip
         plot-frame
         plot)

(require/typed*
 plot
 
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
 )
