#reader(lib "defreader.ss" "scribble")
@require["common.ss"]
@require["panel-class.scrbl"]

@defclass[horizontal-panel% panel% ()]{

A horizontal panel arranges its subwindows in a single row. See also
 @scheme[panel%].

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'border 'deleted)) null]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(left center)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

If the @scheme['border] style is specified, the window is created with
 a thin border (only in this case, the client size of the panel may be
 less than its total size). @DeletedStyleNote{panel}

@WindowKWs[] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]



}}

