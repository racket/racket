#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[vertical-panel% panel% ()]{

A vertical panel arranges its subwindows in a single column. See
 also @scheme[panel%].




@defconstructor[[parent (or/c (is-a/c frame%) (is-a/c dialog%) (is-a/c panel%) (is-a/c pane%))]
                [style (symbols/c deleted border) null]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 0]
                [horiz-margin (integer-in 0 1000) 0]
                [border (integer-in 0 1000) 0]
                [spacing (integer-in 0 1000) 0]
                [alignment two-element list: @scheme['left], @scheme['center], or @scheme['right] and @scheme['top], @scheme['center], or @scheme['bottom] '(left center)]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c #t]
                [stretchable-height any/c #t]]{

If the @scheme['border] style is specified, the window is created with
 a thin border (only in this case, the client size of the panel may be
 less than its total size). \DeletedStyleNote{panel}

@WindowKWs[] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]



}}

