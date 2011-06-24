#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Test}

@defmodule*/no-declare[(framework/test)]
@declare-exporting[framework/test framework]

The framework provides several new primitive functions that simulate user
actions, which may be used to test applications.  You use these primitives and
combine them just as regular Racket functions.  For example,
@racketblock[
  (test:keystroke #\A)
  (test:menu-select "File" "Save")
]
sends a keystroke event to the window with the keyboard focus and invokes the
callback function for the ``Save'' menu item from the ``File'' menu.  This has
the same effect as if the user typed the key ``A'', pulled down the ``File''
menu and selected ``Save''.

It is possible to load this portion of the framework without loading the rest
of the framework. Use @racket[(require framework/test)].

Currently, the test engine has primitives for pushing buttons, setting
check-boxes and choices, sending keystrokes, selecting menu items and clicking
the mouse.  Many functions that are also useful in application testing, such as
traversing a tree of panels, getting the text from a canvas, determining if a
window is shown, and so on, exist in GRacket.

@section[#:tag "test:actions-completeness"]{Actions and completeness}

The actions associated with a testing primitive may not have finished when the
primitive returns to its caller.  Some actions may yield control before they
can complete.  For example, selecting ``Save As...'' from the ``File'' menu
opens a dialog box and will not complete until the ``OK'' or ``Cancel'' button
is pushed.

However, all testing functions wait at least a minimum interval before
returning to give the action a chance to finish.  This interval controls the
speed at which the test suite runs, and gives some slack time for events to
complete.  The default interval is 100 milliseconds.  The interval can be
queried or set with @racket[test:run-interval].

A primitive action will not return until the run-interval has expired and the
action has finished, raised an error, or yielded.  The number of incomplete
actions is given by @racket[test:number-pending-actions].

@italic{Note:}
Once a primitive action is started, it is not possible to undo it or kill its
remaining effect.  Thus, it is not possible to write a utility that flushes the
incomplete actions and resets number-pending-actions to zero.

However, actions which do not complete right away often provide a way to cancel
themselves.  For example, many dialog boxes have a ``Cancel'' button which will
terminate the action with no further effect.  But this is accomplished by
sending an additional action (the button push), not by undoing the original
action.

@section[#:tag "test:errors"]{Errors}

Errors in the primitive actions (which necessarily run in the handler thread)
are caught and reraised in the calling thread.

However, the primitive actions can only guarantee that the action has started,
and they may return before the action has completed.  As a consequence, an
action may raise an error long after the function that started it has returned.
In this case, the error is saved and reraised at the first opportunity (the
next primitive action).

The test engine keeps a buffer for one error, saving only the first error.  Any
subsequent errors are discarded.  Reraising an error empties the buffer,
allowing the next error to be saved.

The function @racket[test:reraise-error] reraises any pending errors.

@section{Technical Issues}

@subsection{Active Frame}

The Self Test primitive actions all implicitly apply to the top-most (active)
frame.

@subsection{Thread Issues}

The code started by the primitive actions must run in the handler thread of the
eventspace where the event takes place.  As a result, the test suite that
invokes the primitive actions must @italic{not} run in that handler thread (or
else some actions will deadlock).  See @racket[make-eventspace] for more info.

@subsection{Window Manager (Unix only)}

In order for the Self Tester to work correctly, the window manager must set the
keyboard focus to follow the active frame.  This is the default behavior in
Microsoft Windows and MacOS, but not in X windows.

In X windows, you must explicitly tell your window manager to set the keyboard
focus to the top-most frame, regardless of the position of the actual mouse.

@section{Test Functions}

@(include-extracted (lib "test.rkt" "framework"))
