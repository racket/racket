#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[vertical-pane% pane% ()]{

A vertical pane arranges its subwindows in a single column. See also @scheme[pane%].




@defconstructor[[parent (or/c (is-a/c frame%) (is-a/c dialog%) (is-a/c panel%) (is-a/c pane%))]
                [vert-margin (integer-in 0 1000) 0]
                [horiz-margin (integer-in 0 1000) 0]
                [border (integer-in 0 1000) 0]
                [spacing (integer-in 0 1000) 0]
                [alignment two-element list: @scheme['left], @scheme['center], or @scheme['right] and @scheme['top], @scheme['center], or @scheme['bottom] '(center top)]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c #t]
                [stretchable-height any/c #t]]{

@SubareaKWs[] @AreaContKWs[] @AreaKWs[]



}}

