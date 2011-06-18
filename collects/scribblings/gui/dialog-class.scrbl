#lang scribble/doc
@(require "common.ss")

@defclass/title[dialog% object% (top-level-window<%>)]{

A dialog is a top-level window that is @defterm{modal}: while the
 dialog is shown, key and mouse press/release events are disabled for
 all other top-level windows in the dialog's eventspace.

@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                 [width (or/c (integer-in 0 10000) false/c) #f]
                 [height (or/c (integer-in 0 10000) false/c) #f]
                 [x (or/c (integer-in 0 10000) false/c) #f]
                 [y (or/c (integer-in 0 10000) false/c) #f]
                 [style (listof (one-of/c 'no-caption 'resize-border 
                                          'no-sheet 'close-button)) 
                        null]
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

The @scheme[label] string is used as the dialog's title in its
title bar.  If the dialog's label is changed (see
@method[window<%> set-label]), the title bar is updated.

The @scheme[parent] argument can be @scheme[#f] or an existing
 frame. On Windows, if @scheme[parent] is an existing frame, the
 new dialog is always on top of its parent. On Windows and Unix, a
 dialog is iconized when its parent is iconized.

If @scheme[parent] is @scheme[#f], then the eventspace for the new
 dialog is the current eventspace, as determined by
 @scheme[current-eventspace]. Otherwise, @scheme[parent]'s eventspace
 is the new dialog's eventspace.

If the @scheme[width] or @scheme[height] argument is not @scheme[#f],
 it specifies an initial size for the dialog (in pixels) assuming that
 it is larger than the minimum size, otherwise the minimum size is
 used. On Windows and Mac OS X (and with some Unix window managers)
 dialogs are not resizeable.

If the @scheme[x] or @scheme[y] argument is not @scheme[#f], it
 specifies an initial location for the dialog. Otherwise, if no
 location is set before the dialog is shown, it is centered (with
 respect @scheme[parent] if not @scheme[#f], the screen otherwise).

The @scheme[style] flags adjust the appearance of the dialog on some
 platforms:

@itemize[

 @item{@scheme['no-caption] --- omits the title bar for the dialog
 (Windows)}

 @item{@scheme['resize-border] --- adds a resizeable border
  around the window (Windows) or grow box in the bottom right corner
  (Mac OS X)}

 @item{@scheme['no-sheet] --- uses a movable window for the dialog,
 even if a parent window is provided (Mac OS X)}

 @item{@scheme['close-button] --- include a close button in the 
 dialog's title bar, which would not normally be included (Mac OS X)}

]

Even if the dialog is not shown, a few notification events may be
 queued for the dialog on creation. Consequently, the new dialog's
 resources (e.g., memory) cannot be reclaimed until some events are
 handled, or the dialog's eventspace is shut down.

@WindowKWs[@scheme[enabled]] @AreaContKWs[] @AreaKWs[]
}

@defmethod[#:mode override 
           (on-subwindow-char [receiver (is-a?/c window<%>)]
                              [event (is-a?/c key-event%)])
           boolean?]{

Returns the result of

@schemeblock[
(or (send this #,(:: top-level-window<%> on-system-menu-char) event)
    (send this #,(:: top-level-window<%> on-traverse-char) event))
]

}

@defmethod[#:mode override
           (show [show? any/c])
           void?]{

If @scheme[show?] is true, the dialog is shown and all frames (and other
 dialogs) in the eventspace become disabled until the dialog is
 closed.  If @scheme[show?] is false, the dialog is hidden and other
 frames and dialogs are re-enabled (unless a different, pre-existing
 dialog is still shown).

If @scheme[show?] is true, the method does not immediately return. Instead,
 it loops with @scheme[yield] until the dialog is found to be hidden
 between calls to @scheme[yield]. An internal semaphore is used with
 @scheme[yield] to avoid a busy-wait, and to ensure that the @scheme[show]
  method returns as soon as possible after the dialog is hidden.

}

@defmethod[(show-without-yield)
           void?]{

Like @racket[(send @#,this-obj[] @#,method[dialog% show] #t)], but returns
immediately instead of @racket[yield]ing.}

}
