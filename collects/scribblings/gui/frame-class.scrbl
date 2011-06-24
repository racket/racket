#lang scribble/doc
@(require "common.rkt")

@defclass/title[frame% object% (top-level-window<%>)]{

A frame is a top-level container window. It has a title bar (which
 displays the frame's label), an optional menu bar, and an optional
 status line.

On Windows, both Multiple Document Interface (MDI) and Single
 Document Interface (SDI) frames are supported.

@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c frame%) false/c) #f]
                 [width (or/c (integer-in 0 10000) false/c) #f]
                 [height (or/c (integer-in 0 10000) false/c) #f]
                 [x (or/c (integer-in -10000 10000) false/c) #f]
                 [y (or/c (integer-in -10000 10000) false/c) #f]
                 [style (listof (one-of/c 'no-resize-border 'no-caption 
                                          'no-system-menu 'hide-menu-bar 
                                          'mdi-parent 'mdi-child
                                          'toolbar-button 'float 'metal)) null]
                 [enabled any/c #t]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

The @racket[label] string is displayed in the frame's title
bar. If the frame's label is changed (see @method[window<%>
set-label]), the title bar is updated.

The @racket[parent] argument can be @racket[#f] or an existing
frame. On Windows, if @racket[parent] is an existing frame,
the new frame is always on top of its parent. Also, the
@racket[parent] frame may be an MDI parent frame from a new MDI
child frame. On Windows and Unix (for many window managers), a
frame is iconized when its parent is iconized.

If @racket[parent] is @racket[#f], then the eventspace for the
new frame is the current eventspace, as determined by
@racket[current-eventspace]. Otherwise, @racket[parent]'s
eventspace is the new frame's eventspace.

If the @racket[width] or @racket[height] argument is not
@racket[#f], it specifies an initial size for the frame (in
pixels) assuming that it is larger than the minimum size,
otherwise the minimum size is used.

If the @racket[x] or @racket[y] argument is not @racket[#f], it
specifies an initial location for the frame. Otherwise, a
location is selected automatically (tiling frames and dialogs as
they are created).

The @racket[style] flags adjust the appearance of the frame on
some platforms:

@itemize[

 @item{@racket['no-resize-border] --- omits the resizeable border
  around the window (Windows, Unix) or grow box in the bottom right
  corner (Mac OS X)}

 @item{@racket['no-caption] --- omits the title bar for the frame
 (Windows, Mac OS X, Unix)}

 @item{@racket['no-system-menu] --- omits the system menu
 (Windows)}

 @item{@racket['mdi-child] --- creates the frame as a MDI
 (multiple document interface) child frame, mutually exclusive with
 @racket['mdi-parent] (Windows)}

 @item{@racket['mdi-parent] --- creates the frame as a MDI
 (multiple document interface) parent frame, mutually exclusive with
 @racket['mdi-child] (Windows)}

 @item{@racket['toolbar-button] --- includes a toolbar button on the
 frame's title bar (Mac OS X); a click on the toolbar button triggers
 a call to @method[frame% on-toolbar-button-click]}
 @item{@racket['hide-menu-bar] --- hides the menu bar and dock when
 the frame is active (Mac OS X) or asks the window manager to make
 the frame fullscreen (Unix)}

 @item{@racket['float] --- causes the frame to stay in front of all
 other non-floating windows (Windows, Mac OS X, Unix); on Mac OS X, a floating frame
 shares the focus with an active non-floating frame; when this style
 is combined with @racket['no-caption], then showing the frame does
 not cause the keyboard focus to shift to the window, and on Unix,
 clicking the frame does not move the focus; on Windows, a floating
 frame has no taskbar button}

 @item{@racket['metal] --- ignored (formerly supported for Mac OS X)}

]

If the @racket['mdi-child] style is specified, the @racket[parent] must be
 a frame with the @racket['mdi-parent] style, otherwise @|MismatchExn|.

Even if the frame is not shown, a few notification events may be
 queued for the frame on creation. Consequently, the new frame's
 resources (e.g., memory) cannot be reclaimed until some events are
 handled, or the frame's eventspace is shut down.

@WindowKWs[@racket[enabled]] @AreaContKWs[] @AreaKWs[]

}


@defmethod[(create-status-line)
           void?]{

Creates a status line at the bottom of the frame. The width of the
 status line is the whole width of the frame (adjusted automatically
 when resizing), and the height and text size are platform-specific.

See also @method[frame% set-status-text].

}

@defmethod[(get-menu-bar)
           (or/c (is-a?/c menu-bar%) false/c)]{

Returns the frame's menu bar, or @racket[#f] if none has been created
 for the frame.

}

@defmethod[(has-status-line?)
           boolean?]{

Returns @racket[#t] if the frame's status line has been created,
 @racket[#f] otherwise. See also @method[frame% create-status-line].

}

@defmethod[(iconize [iconize? any/c])
           void?]{

Iconizes (@as-index{minimizes}) or deiconizes (restores) the
 frame. Deiconizing brings the frame to the front.

@Unmonitored[@elem{A frame's iconization} @elem{the user} @elem{a
frame has been iconized} @elem{@method[frame% is-iconized?]}]

}

@defmethod[(is-iconized?)
           boolean?]{

Returns @racket[#t] if the frame is iconized (minimized), @racket[#f]
otherwise.

}

@defmethod[(is-maximized?)
           boolean?]{

On Windows and Mac OS X, returns @racket[#t] if the frame is
maximized, @racket[#f] otherwise. On Unix, the result is always
@racket[#f].

}

@defmethod[(maximize [maximize? any/c])
           void?]{
@methspec{

Maximizes or restores the frame on Windows and Mac OS X; the
 frame's show state is not affected. On Windows, an iconized frame
 cannot be maximized or restored.

@MonitorMethod[@elem{A window's maximization} @elem{the user} @elem{@method[window<%> on-size]} @elem{size}]

}
@methimpl{

If @racket[maximize?] is @racket[#f], the window is restored, otherwise
 it is maximized.



}}

@defmethod*[([(modified)
              boolean?]
             [(modified [modified? any/c])
              void?])]{

Gets or sets the frame's modification state as reflected to the user.
 On Mac OS X, the modification state is reflected as a dot in the
 frame's close button. On Windows and Unix, the modification state is
 reflected by an asterisk at the end of the frame's displayed title.

}

@defmethod[(on-mdi-activate [active? any/c])
           void?]{

Called on Windows when a MDI-child frame becomes the active frame
 within its parent (in which case the argument is @racket[#t]), or when
 the child frame ceases to be the active frame (in which case the
 argument is @racket[#f]).


MDI activation is different from keyboard-focus activation. If the
 parent frame is the frontmost top-level frame, so that the MDI child
 gets or loses the keyboard focus, then a separate
@method[top-level-window<%> on-activate] notification is sent to the MDI-child frame.

}

@defmethod[(on-menu-char [event (is-a?/c key-event%)])
           boolean?]{

If the frame has a menu bar with keyboard shortcuts, and if the key
event includes a Control, Alt, Option, Meta, Command, Shift, or
Function key, then @method[frame% on-menu-char] attempts to match the
given event to a menu item. If a match is found, @racket[#t] is
returned, otherwise @racket[#f] is returned.

When the match corresponds to a complete shortcut combination, the
 menu item's callback is called (before
@method[frame% on-menu-char] returns).

If the event does not correspond to a complete shortcut combination,
 the event may be handled anyway if it corresponds to a mnemonic in the
 menu bar (i.e., an underlined letter in a menu's title, which is
 installed by including an ampersand in the menu's label). If a
 mnemonic match is found, the keyboard focus is moved to the menu bar
 (selecting the menu with the mnemonic), and @racket[#t] is returned.

}

@defmethod[#:mode override 
           (on-subwindow-char [receiver (is-a?/c window<%>)]
                              [event (is-a?/c key-event%)])
           boolean?]{

Returns the result of

@racketblock[
(or (send this @#,method[frame% on-menu-char] event)
    (send this @#,method[top-level-window<%> on-system-menu-char] event)
    (send this @#,method[top-level-window<%> on-traverse-char] event))
]
}

@defmethod[(on-toolbar-button-click)
           void?]{

On Mac OS X, called when the user clicks the toolbar button on a
 frame created with the @indexed-racket['toolbar-button] style.

}

@defmethod[(set-icon [icon (is-a?/c bitmap%)]
                     [mask (is-a?/c bitmap%) #f]
                     [which (one-of/c 'small 'large 'both) 'both])
           void?]{

Sets the large or small icon bitmap for this frame.  Future changes to
 the bitmap do not affect the frame's icon.

The icon is used in a platform-specific way:

@itemize[

 @item{Windows --- the small icon is used for the frame's icon (in the
       top-left) and in the task bar, and the large icon is used for
       the Alt-Tab task switcher.}

 @item{Mac OS X --- both icons are ignored.}

 @item{Unix --- many window managers use the small icon in the same way
       as Windows, and others use the small icon when iconifying the
       frame; the large icon is ignored.}

]

The bitmap for either icon can be any size, but most platforms scale
 the small bitmap to 16 by 16 pixels and the large bitmap to 32 by 32
 pixels.

If a mask bitmap is not provided, then the entire (rectangular) bitmap
 is used as an icon.

If a mask bitmap is provided, the mask must be monochrome. In the mask
 bitmap, use black pixels to indicate the icon's region and use white
 pixels outside the icon's region. In the icon bitmap, use black
 pixels for the region outside the icon.

}

@defmethod[(set-status-text [text string?])
           void?]{

Sets the frame's status line text and redraws the status line. See
 also @method[frame% create-status-line].

}}

