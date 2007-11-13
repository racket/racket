#lang scribble/doc
@require["common.ss"]

@defclass/title[editor-canvas% object% (canvas<%>)]{

An @scheme[editor-canvas%] object manages and displays a
 @scheme[text%] or @scheme[pasteboard%] object.


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [editor (or/c (or/c @scheme[text%] (is-a?/c pasteboard%)) false/c) #f]
                 [style (listof (one-of/c 'no-border 'control-border 'combo 
                                          'no-hscroll 'no-vscroll 
                                          'hide-hscroll 'hide-vscroll 
                                          'auto-vscroll 'auto-hscroll 
                                          'resize-corner 'deleted 'transparent)) null]
                 [scrolls-per-page (integer-in 1 10000) 100]
                 [label (or/c label-string? false/c) #f]
                 [wheel-step (or/c (integer-in 1 10000) false/c) 3]
                 [line-count (or/c (integer-in 1 1000) false/c) #f]
                 [horizontal-inset (integer-in 0 1000) 5]
                 [vertical-inset (integer-in 0 1000) 5]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

If a canvas is initialized with @scheme[#f] for @scheme[editor],
 install an editor later with @method[editor-canvas% set-editor].

The @scheme[style] list can contain the following flags:

@itemize{

 @item{@scheme['no-border] --- omits a border around the canvas}

 @item{@scheme['control-border] --- gives the canvas a border that is
                              like a @scheme[text-field%] control}

 @item{@scheme['combo] --- gives the canvas a combo button that is like
                          a @scheme[combo-field%] control; this
                          style is intended for use with
                          @scheme['control-border],
                          @scheme['hide-hscroll], and
                          @scheme['hide-vscroll]}

 @item{@scheme['no-hscroll] --- disallows horizontal scrolling and hides the horizontal scrollbar}

 @item{@scheme['no-vscroll] --- disallows vertical scrolling and hides the vertical scrollbar}

 @item{@scheme['hide-hscroll] --- allows horizontal scrolling, but hides the horizontal scrollbar}

 @item{@scheme['hide-vscroll] --- allows vertical scrolling, but hides the vertical scrollbar}

 @item{@scheme['auto-hscroll] --- automatically hides the horizontal scrollbar when unneeded
                                 (unless @scheme['no-hscroll] or @scheme['hide-hscroll] is specified)}

 @item{@scheme['auto-vscroll] --- automatically hides the vertical scrollbar when unneeded
                                 (unless @scheme['no-vscroll] or @scheme['hide-vscroll] is specified)}

 @item{@scheme['resize-corner] --- leaves room for a resize control at the canvas's
                                  bottom right when only one scrollbar is visible}

 @item{@scheme['deleted] --- creates the canvas as initially hidden and without affecting
                             @scheme[parent]'s geometry; the canvas can be made active
                             later by calling @scheme[parent]'s @method[area-container<%> add-child]
                             method}

 @item{@scheme['transparent] --- the canvas is ``erased'' before an
                             update using it's parent window's background}

}

While vertical scrolling of text editors is based on lines,
 horizontal scrolling and pasteboard vertical scrolling is based on a
 fixed number of steps per horizontal page. The @scheme[scrolls-per-page]
 argument sets this value.

@index["wheel on mouse"]{If} provided, the @scheme[wheel-step]
 argument is passed on to the @method[editor-canvas% wheel-step]
 method. The default wheel step can be overridden globally though the
 @ResourceFirst{wheelStep}; see @|mrprefsdiscuss|.

If @scheme[line-count] is not @scheme[#f], it is passed on to the
 @method[editor-canvas% set-line-count] method.

If @scheme[horizontal-inset] is not @scheme[5], it is passed on to the
 @method[editor-canvas% horizontal-inset] method. Similarly, if
 @scheme[vertical-inset] is not @scheme[5], it is passed on to the
 @method[editor-canvas% vertical-inset] method.

@WindowKWs[] @SubareaKWs[] @AreaKWs[]

}


@defmethod*[([(allow-scroll-to-last)
              boolean?]
             [(allow-scroll-to-last [on? any/c])
              void?])]{

Enables or disables last-line scrolling, or gets the current enable
 state.  If last-line scrolling is enabled, then an editor displayed
 in this canvas can be scrolled so that the last line of text is at
 the top of the canvas (or bottom of the canvas when bottom-based
 scrolling is enabled; see @method[editor-canvas%
 scroll-with-bottom-base]). By default, an editor can only be scrolled
 until the last line is at the bottom (or top) of the canvas.

}

@defmethod*[([(allow-tab-exit)
              boolean?]
             [(allow-tab-exit [on? any/c])
              void?])]{

@index['("keyboard focus" "navigation")]{Gets} or sets whether
 tab-exit is enabled for the editor canvas. When tab-exit is enabled,
 the user can move the keyboard focus out of the editor using the Tab
 and arrow keys, or invoke the default button using the Enter/Return
 key. By default, tab-exit is disabled.

When tab-exit is enabled for an editor canvas, Tab, arrow, and Enter
 keyboard events are consumed by a frame's default
 @method[top-level-window<%> on-traverse-char] method. (In addition, a
 dialog's default method consumes Escape key events.) Otherwise,
 @method[top-level-window<%> on-traverse-char] allows the keyboard
 events to be propagated to the canvas.

}

@defmethod[(call-as-primary-owner [f (-> any)])
           any]{

Calls a thunk and returns the value. While the thunk is being called,
 if the canvas has an editor, the editor's @method[editor<%>
 get-admin] method returns the administrator for this canvas. This
 method is only useful when an editor is displayed in multiple
 canvases.

}

@defmethod*[([(force-display-focus)
              boolean?]
             [(force-display-focus [on? any/c])
              void?])]{

Enables or disables force-focus mode.  In force-focus mode, the caret
 of the editor displayed in this canvas will always be visible, even
 when the canvas does not actually have the keyboard focus.

}


@defmethod[(get-editor)
           (or/c (or/c @scheme[text%] (is-a?/c pasteboard%)) false/c)]{

Returns the editor currently displayed by this canvas, or @scheme[#f]
 if the canvas does not have an editor.

}


@defmethod[(get-line-count)
           (or/c (integer-in 1 1000) false/c)]{

Returns a line count installed with @method[editor-canvas%
 set-line-count], or @scheme[#f] if no minimum line count is set.

}

@defmethod*[([(horizontal-inset)
              (integer-in 1 10000)]
             [(horizontal-inset [step (integer-in 1 10000)])
              void?])]{

Gets or sets the number of pixels within the canvas reserved to
 the left and right of editor content. The default is @scheme[5].

}


@defmethod*[([(lazy-refresh)
              boolean?]
             [(lazy-refresh [on? any/c])
              void?])]{

Enables or disables lazy-refresh mode, or gets the current enable
 state. In lazy-refresh mode, the canvas's @method[window<%> refresh]
 method is called when the window needs to be updated, rather than
 @method[editor-canvas% on-paint]. By default, an
 @scheme[editor-canvas%] object is @italic{not} in lazy-refresh mode.

}


@defmethod[#:mode override 
           (on-char [event (is-a?/c key-event%)])
           void?]{

Handles @scheme['wheel-up] and @scheme['wheel-down] events by
 scrolling vertically. Otherwise, passes the event to the canvas's
 editor, if any, by calling its @method[editor<%> on-char] method.

See also @method[editor-canvas% get-editor].

}


@defmethod[#:mode override 
           (on-event [event (is-a?/c mouse-event%)])
           void?]{

Passes the event to the canvas's editor, if any, by calling its
 @method[editor<%> on-event] method.

See also @method[editor-canvas% get-editor].

}

@defmethod[#:mode override 
           (on-focus [on? any/c])
           void?]{

Enables or disables the caret in the @techlink{display}'s editor, if
 there is one.

}

@defmethod[#:mode override 
           (on-paint)
           void?]{

Repaints the editor.

}

@defmethod[#:mode override 
           (on-size [width (integer-in 0 10000)]
                    [height (integer-in 0 10000)])
           void?]{

If the canvas is displaying an editor, its @method[editor<%>
on-display-size] method is called.

}

@defmethod[(scroll-to [localx real?]
                      [localy real?]
                      [w (and/c real? (not/c negative?))]
                      [h (and/c real? (not/c negative?))]
                      [refresh? any/c]
                      [bias (one-of/c 'start 'end 'none) 'none])
           boolean?]{

Requests scrolling so that the given region in the currently displayed
 editor is made visible.

The @scheme[localx], @scheme[localy], @scheme[w], and @scheme[h] arguments specify
 a region of the editor to be made visible by the scroll (in editor
 coordinates).

If @scheme[refresh?] is not @scheme[#f], then the editor is updated
 immediately after a successful scroll.

The @scheme[bias] argument is one of:
@itemize{

 @item{@scheme['start] --- if the range doesn't fit in the visible
 area, show the top-left region}

 @item{@scheme['none] --- no special scrolling instructions}

 @item{@scheme['end] --- if the range doesn't fit in the visible area,
 show the bottom-right region}

}

The return value is @scheme[#t] if the @techlink{display} is scrolled, @scheme[#f]
 if not (either because the requested region is already visible,
 because the @techlink{display} has zero size, or because the editor is currently
 printing).


}


@defmethod*[([(scroll-with-bottom-base)
              boolean?]
             [(scroll-with-bottom-base [on? any/c])
              void?])]{

Enables or disables bottom-base scrolling, or gets the current enable
 state. If bottom-base scrolling is on, then scroll positions are
 determined by line boundaries aligned with the bottom of the viewable
 area (rather than with the top of the viewable area). If last-line
 scrolling is also enabled (see @method[editor-canvas%
 allow-scroll-to-last]), then the editor is bottom-aligned in the
 @techlink{display} area even when the editor does not fill the
 viewable area.

}



@defmethod[(set-editor [edit (or/c (or/c @scheme[text%] (is-a?/c pasteboard%)) false/c)]
                       [redraw? any/c #t])
           void?]{

Sets the editor that is displayed by the canvas, releasing the current
 editor (if any). If the new editor already has an administrator that
 is not associated with an @scheme[editor-canvas%], then the new
 editor is @italic{not} installed into the canvas.

If @scheme[redraw?] is @scheme[#f], then the editor is not immediately
 drawn; in this case, something must force a redraw later (e.g., a
 call to the @method[editor-canvas% on-paint] method).

If the canvas has a line count installed with @method[editor-canvas%
 set-line-count], the canvas's minimum height is adjusted.

}


@defmethod[(set-line-count [count (or/c (integer-in 1 1000) false/c)])
           void?]{

Sets the canvas's graphical minimum height to display a particular
 number of lines of text. The line height is determined by measuring
 the difference between the top and bottom of a displayed editor's
 first line. The minimum height is not changed until the canvas gets
 an editor. When the canvas's editor is changed, the minimum height is
 recalculated.

If the line count is set to @scheme[#f], then the canvas's graphical
 minimum height is restored to its original value.

}


@defmethod*[([(vertical-inset)
              (integer-in 1 10000)]
             [(vertical-inset [step (integer-in 1 10000)])
              void?])]{

Gets or sets the number of pixels within the canvas reserved above
 and below editor content. The default is @scheme[5].

}


@defmethod*[([(wheel-step)
              (or/c (integer-in 1 10000) false/c)]
             [(wheel-step [step (or/c (integer-in 1 10000) false/c)])
              void?])]{

Gets or sets the number of vertical scroll steps taken for one click
 of the mouse wheel via a @scheme['wheel-up] or @scheme['wheel-down]
 @scheme[key-event%]. A @scheme[#f] value disables special handling
 for wheel events (i.e., wheel events are passed on to the canvas's
 editor).

}}
