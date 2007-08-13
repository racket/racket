#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[tab-panel% vertical-panel% ()]{

A tab panel arranges its subwindows in a single column, but also
 includes a horizontal row of tabs at the top of the panel. See
 also @scheme[panel%].

The @scheme[tab-panel%] class does not implement the virtual
 swapping of the panel content when a new tab is selected. Instead, it
 merely invokes a callback procedure to indicate that a user changed
 the tab selection.




@defconstructor[[choices list of {\labelstrings}]
                [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) (is-a?/c panel%) (is-a?/c pane%))]
                [callback procedure of two arguments: a @scheme[tab-panel%] object and a @scheme[control-event%] object @scheme[(\scmk{lambda] (@scheme[tp] @scheme[e]) (void))}]
                [style (symbols/c deleted no-border) null]
                [font (is-a?/c font%) @scheme[normal-control-font]]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 0]
                [horiz-margin (integer-in 0 1000) 0]
                [border (integer-in 0 1000) 0]
                [spacing (integer-in 0 1000) 0]
                [alignment two-element list: @scheme['left], @scheme['center], or @scheme['right] and @scheme['top], @scheme['center], or @scheme['bottom] '(center top)]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c #t]
                [stretchable-height any/c #t]]{

Creates a tab pane, where the
 @scheme[choices] list specifies the tab labels.

Each string in @scheme[choices] can contain an ampersand, which (in the
 future) may create a mnemonic for clicking the corresponding tab. A
 double ampersand is converted to a single ampersand.

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['tab-panel]) when the user changes the tab selection.

If the @scheme[style] list includes @scheme['no-border], no border is
 drawn around the panel content. \DeletedStyleNote{tab panel}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(append [choice label-string?])
           void?]{
@spec{

Adds a tab to the right end of panel's top row of tabs.

}
@impl{

The label string @scheme[choice] can contain an ampersand, which (in the
 future) may create a mnemonic for clicking the new tab. A double
 ampersand is converted to a single ampersand.



}}

@defmethod[(delete [n nonnegative-exact-integer?])
           void?]{
@spec{

Deletes an existing tab.

}
@impl{

If @scheme[n] is equal to or larger than the number of tabs on the panel,
 @|MismatchExn|}.



}}

@defmethod[(get-selection)
           (or/c nonnegative-exact-integer? false/c)]{
@spec{

Returns the index (counting from 0) of the currently selected tab.
 If the panel has no tabs, the result is @scheme[#f].

}}

@defmethod[(set-selection [n nonnegative-exact-integer?])
           void?]{
@spec{

Sets the currently selected tab by index (counting from 0).

}
@impl{

If @scheme[n] is equal to or larger than the number of tabs in the panel,
 @|MismatchExn|}.



}}

@defmethod[(get-number)
           nonnegative-exact-integer?]{
@spec{

Returns the number of tabs on the panel.

}}

@defmethod[(get-item-label [n nonnegative-exact-integer?])
           string]{
@spec{

Gets the label of a tab by position. Tabs are numbered from @scheme[0].

}
@impl{

If @scheme[n] is equal to or larger than the number of tabs in the panel,
 @|MismatchExn|}.



}}

@defmethod[(set-item-label [n nonnegative-exact-integer?]
                           [label label-string?])
           string]{
@spec{

Sets the label of a tab by position. Tabs are numbered from @scheme[0].

}
@impl{

Set the label for tab @scheme[n] to @scheme[label]. If @scheme[n] is equal to
 or larger than the number of tabs in the panel, @|MismatchExn|}.



}}

@defmethod[(set [choices list of {\labelstrings}])
           void?]{
@spec{

Removes all tabs from the panel and installs tabs with the given
 labels.

}
@impl{




}}}

