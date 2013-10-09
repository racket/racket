#lang typed/racket/base

(require (only-in typed/mred/mred Snip% Frame%)
         plot/typed/private/common/types
         plot/typed/private/syntax)

(provide plot3d-snip
         plot3d-frame
         plot3d)

(require/typed*
 plot
 
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
 )
