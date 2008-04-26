#lang scribble/doc

@(require scribble/manual)
@(defmodule framework/framework)

@title{@bold{Framework}: PLT GUI Application Framework}

The framework provides these libraries:
@itemize{

@item{@bold{Entire Framework}

@itemize{

@item{@scheme[(require framework/framework)]

  This library provides all of the definitions and syntax
  described in this manual.
}
@item{@scheme[(require (lib "framework-sig.ss" "framework"))]
  
  This library provides the signature definitions:
  @scheme[framework^], and
  @scheme[framework-class^].
  The @scheme[framework^] signature contains all of the 
  names of the procedures described in this manual, except
  those that begin with @scheme[test:] and
  @scheme[gui-utils:]. The @scheme[framework-class^]
  signature contains all of the classes defined in this
  manual.
}
@item{@scheme[(require (lib "framework-unit.ss" "framework"))]

  This library provides one
  @scheme[unit/sig]: @scheme[framework@]. It exports the signature
  @scheme[framework^]. It imports the @scheme[mred^] signature.

}
}}
@item{
 @bold{Test Suite Engine}

@scheme[(require (lib "test.ss" "framework"))]

This library provides all of the definitions beginning with
@scheme[test:] described in this manual.
}
@item{ @bold{GUI Utilities}
@scheme[(require (lib "gui-utils.ss" "framework"))]
    
    This libraries provides all of the definitions beginning
    with \scheme{gui-utils:} described in this manual.
}
@item{ @bold{Preferences}
@scheme[(require (lib "preferences.ss" "framework"))]
    
  This library provides a subset of the names of the
  \scheme|framework.ss| library, namely those for
  manipulating preference settings and is designed to be
  used from mzscheme.

The precise set of exported names is:
@scheme[preferences:snapshot?],
@scheme[preferences:restore-prefs-snapshot],
@scheme[preferences:get-prefs-snapshot],
@scheme[exn:make-unknown-preference],
@scheme[exn:unknown-preference?],
@scheme[preferences:low-level-put-preferences],
@scheme[preferences:get],
@scheme[preferences:set],
@scheme[preferences:add-callback],
@scheme[preferences:set-default],
@scheme[preferences:set-un/marshall], and
@scheme[preferences:restore-defaults].
}}

@section{GUI Test Suite Utilities}

The framework provides several new primitive functions that simulate
user actions, which may be used to test applications.  You use these
primitives and combine them just as regular MzScheme functions.  For
example, 
@schemeblock[
(test:keystroke #\A)
(test:menu-select "File" "Save")
]
sends a keystroke event to the window with the keyboard focus and invokes
the callback function for the ``Save'' menu item from the ``File'' menu.
This has the same effect as if the user typed the key ``A'', pulled
down the ``File'' menu and selected ``Save''.

It is possible to load this portion of the framework without loading
the rest of the framework. Use
@scheme[(require framework/test)].

Currently, the test engine has primitives for pushing
buttons, setting check-boxes and choices, sending keystrokes,
selecting menu items and clicking the mouse.  Many functions 
that are also useful in application testing, such as 
traversing a tree of panels, getting the text from a canvas,
determining if a window is shown, and so on, exist in MrEd.

@subsection{Actions and completeness}

The actions associated with a testing primitive may not have finished 
when the primitive returns to its caller.  
Some actions may yield control before they can complete.
For example, selecting ``Save As...'' from the ``File'' menu
opens a dialog box and will not complete until the ``OK'' 
or ``Cancel'' button is pushed.

However, all testing functions wait at least a minimum interval
before returning to give the action a chance to finish.
This interval controls the speed at which the test suite runs,
and gives some slack time for events to complete.
The default interval is 100 milliseconds.  The interval can be queried 
or set with \iscmprocedure{test:run-interval}.

A primitive action will not return until the run-interval has
expired and the action has finished, raised an error, or yielded.
The number of incomplete actions is given by
\iscmprocedure{test:number-pending-actions}.

{\it Note:}
Once a primitive action is started, it is not possible to undo it
or kill its remaining effect.
Thus, it is not possible to write a utility that flushes the 
incomplete actions and resets number-pending-actions to zero.

However, actions which do not complete right away often provide a 
way to cancel themselves.
For example, many dialog boxes have a ``Cancel'' button which will
terminate the action with no further effect.
But this is accomplished by sending an additional action
(the button push), not by undoing the original action.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Errors}
\label{fw:test:errors}

Errors in the primitive actions (which necessarily run in the 
handler thread) are caught and reraised in the calling thread.

However, the primitive actions can only guarantee that the action 
has started, and they may return before the action has completed.
As a consequence, an action may raise an error long after the
function that started it has returned.
In this case, the error is saved and reraised at the first opportunity
(the next primitive action).

The test engine keeps a buffer for one error, saving only the 
first error.  Any subsequent errors are discarded.  
Reraising an error empties the buffer, allowing the next error
to be saved.

The function
\iscmprocedure{test:reraise-error}
reraises any pending errors.

\subsection{Technical Issues}

{\bf Active Frame}

The Self Test primitive actions all implicitly apply to the 
top-most (active) frame.

{\bf Thread Issues}

The code started by the primitive actions must run in the handler
thread of the eventspace where the event takes place.  As a result,
the test suite that invokes the primitive actions must {\it not} run
in that handler thread (or else some actions will deadlock).  See
\Mrhyperref{the eventspace section}{see section~}{}{eventspaceinfo} 
for more info.

{\bf Window Manager (Unix only)}

In order for the Self Tester to work correctly, the window manager
must set the keyboard focus to follow the active frame.
This is the default behavior in Microsoft Windows and MacOS,
but not in X windows.

In X windows, you must explicitly tell your window manager to set the 
keyboard focus to the top-most frame, regardless of the position of the 
actual mouse.  Some window managers may not implement such functionality. 
You can obtain such an effect in Fvwm and Fvwm95 by using the option:
\begin{verbatim}
  Style  "*"  ClickToFocus
\end{verbatim}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@section{Thanks}

Thanks to Shriram Krishnamurthi, Cormac Flanagan, Matthias
Felleisen, Ian Barland, Gann Bierner, Richard Cobbe, Dan
Grossman, Stephanie Weirich, Paul Steckler, Sebastian Good,
Johnathan Franklin, Mark Krentel, Corky Cartwright, Michael
Ernst, Kennis Koldewyn, Bruce Duba, and many others for
their feedback and help.


@include-section["framework-application.scrbl"]
@include-section["framework-autosave.scrbl"]
@include-section["framework-canvas.scrbl"]
@include-section["framework-color-model.scrbl"]
@include-section["framework-color-prefs.scrbl"]
@include-section["framework-color.scrbl"]
@include-section["framework-comment-box.scrbl"]
@include-section["framework-editor.scrbl"]
@include-section["framework-exit.scrbl"]
@include-section["framework-finder.scrbl"]
@include-section["framework-frame.scrbl"]
@include-section["framework-group.scrbl"]
@include-section["framework-handler.scrbl"]
@include-section["framework-icon.scrbl"]
@include-section["framework-keymap.scrbl"]
@;include-section["framework-main.scrbl"]
@include-section["framework-menu.scrbl"]
@;include-section["framework-mode.scrbl"]
@include-section["framework-number-snip.scrbl"]
@include-section["framework-panel.scrbl"]
@include-section["framework-pasteboard.scrbl"]
@include-section["framework-path-utils.scrbl"]
@include-section["framework-preferences.scrbl"]
@include-section["framework-scheme.scrbl"]
@include-section["framework-text.scrbl"]
@include-section["framework-test.scrbl"]
@include-section["framework-version.scrbl"]

@index-section[]
