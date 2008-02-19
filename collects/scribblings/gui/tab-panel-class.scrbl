#lang scribble/doc
@(require "common.ss")

@defclass/title[tab-panel% vertical-panel% ()]{

A tab panel arranges its subwindows in a single column, but also
 includes a horizontal row of tabs at the top of the panel. See
 also @scheme[panel%].

The @scheme[tab-panel%] class does not implement the virtual
 swapping of the panel content when a new tab is selected. Instead, it
 merely invokes a callback procedure to indicate that a user changed
 the tab selection.




@defconstructor[([choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c tab-panel%) (is-a?/c control-event%)
                            . -> . any) 
                           (lambda (b e) (void))]
                 [style (listof (one-of/c 'no-border 'deleted)) null]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

Creates a tab pane, where the @scheme[choices] list specifies the tab
 labels.

Each string in @scheme[choices] can contain an ampersand, which (in the
 future) may create a mnemonic for clicking the corresponding tab. A
 double ampersand is converted to a single ampersand.

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['tab-panel]) when the user changes the tab selection.

If the @scheme[style] list includes @scheme['no-border], no border is
 drawn around the panel content. @DeletedStyleNote{tab panel}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(append [choice label-string?])
           void?]{

Adds a tab to the right end of panel's top row of tabs.

The label string @scheme[choice] can contain @litchar{&}, which (in
 the future) may create a mnemonic for clicking the new tab. A
 @litchar{&&} is converted to @litchar{&}.

}

@defmethod[(delete [n nonnegative-exact-integer?])
           void?]{

Deletes an existing tab. If @scheme[n] is equal to or larger than the
 number of tabs on the panel, @|MismatchExn|.

}

@defmethod[(get-item-label [n nonnegative-exact-integer?])
           string?]{

Gets the label of a tab by position. Tabs are numbered from @scheme[0].
If @scheme[n] is equal to or larger than the number of tabs in the panel,
 @|MismatchExn|.

}

@defmethod[(get-number)
           nonnegative-exact-integer?]{

Returns the number of tabs on the panel.

}

@defmethod[(get-selection)
           (or/c nonnegative-exact-integer? false/c)]{

Returns the index (counting from 0) of the currently selected tab.  If
 the panel has no tabs, the result is @scheme[#f].

}

@defmethod[(set [choices (listof label-string?)])
           void?]{

Removes all tabs from the panel and installs tabs with the given
 labels.

}

@defmethod[(set-item-label [n nonnegative-exact-integer?]
                           [label label-string?])
           string?]{

Set the label for tab @scheme[n] to @scheme[label]. If @scheme[n] is equal to
 or larger than the number of tabs in the panel, @|MismatchExn|.

}

@defmethod[(set-selection [n nonnegative-exact-integer?])
           void?]{

Sets the currently selected tab by index (counting from 0).
If @scheme[n] is equal to or larger than the number of tabs in the panel,
 @|MismatchExn|.

}}

