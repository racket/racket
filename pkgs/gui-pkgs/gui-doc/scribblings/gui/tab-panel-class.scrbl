#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/tab-panel}}

@defclass/title[tab-panel% vertical-panel% ()]{

A tab panel arranges its subwindows in a single column, but also
 includes a horizontal row of tabs at the top of the panel. See
 also @racket[panel%].

The @racket[tab-panel%] class does not implement the virtual
 swapping of the panel content when a new tab is selected. Instead, it
 merely invokes a callback procedure to indicate that a user changed
 the tab selection.




@defconstructor[([choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c tab-panel%) (is-a?/c control-event%)
                            . -> . any) 
                           (lambda (b e) (void))]
                 [style (listof (or/c 'no-border 'deleted)) null]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 0]
                 [horiz-margin spacing-integer? 0]
                 [border spacing-integer? 0]
                 [spacing spacing-integer? 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

Creates a tab pane, where the @racket[choices] list specifies the tab
 labels.

Each string in @racket[choices] can contain an ampersand, which (in the
 future) may create a mnemonic for clicking the corresponding tab. A
 double ampersand is converted to a single ampersand.

The @racket[callback] procedure is called (with the event type
 @indexed-racket['tab-panel]) when the user changes the tab selection.

If the @racket[style] list includes @racket['no-border], no border is
 drawn around the panel content. @DeletedStyleNote[@racket[style] @racket[parent]]{tab panel}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(append [choice label-string?])
           void?]{

Adds a tab to the right end of panel's top row of tabs.

The label string @racket[choice] can contain @litchar{&}, which (in
 the future) may create a mnemonic for clicking the new tab. A
 @litchar{&&} is converted to @litchar{&}.

}

@defmethod[(delete [n exact-nonnegative-integer?])
           void?]{

Deletes an existing tab. If @racket[n] is equal to or larger than the
 number of tabs on the panel, @|MismatchExn|.

}

@defmethod[(get-item-label [n exact-nonnegative-integer?])
           string?]{

Gets the label of a tab by position. Tabs are numbered from @racket[0].
If @racket[n] is equal to or larger than the number of tabs in the panel,
 @|MismatchExn|.

}

@defmethod[(get-number)
           exact-nonnegative-integer?]{

Returns the number of tabs on the panel.

}

@defmethod[(get-selection)
           (or/c exact-nonnegative-integer? #f)]{

Returns the index (counting from 0) of the currently selected tab.  If
 the panel has no tabs, the result is @racket[#f].

}

@defmethod[(set [choices (listof label-string?)])
           void?]{

Removes all tabs from the panel and installs tabs with the given
 labels.

}

@defmethod[(set-item-label [n exact-nonnegative-integer?]
                           [label label-string?])
           void?]{

Set the label for tab @racket[n] to @racket[label]. If @racket[n] is equal to
 or larger than the number of tabs in the panel, @|MismatchExn|.

}

@defmethod[(set-selection [n exact-nonnegative-integer?])
           void?]{

Sets the currently selected tab by index (counting from 0).
If @racket[n] is equal to or larger than the number of tabs in the panel,
 @|MismatchExn|.

}}

