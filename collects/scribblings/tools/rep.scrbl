#lang scribble/doc
@(require "common.rkt")
@(tools-title "rep")


@definterface[drracket:rep:text<%> ()]{
}


@defclass[drracket:rep:text% racket:text% (drracket:rep:text<%>)]{

This class implements a read-eval-print loop for DrRacket.  User
submitted evaluations in DrRacket are evaluated asynchronously, in an
eventspace created for the user. No evaluations carried out by this
class affect the implementation that uses it.



@defconstructor/make[([context (implements drracket:rep:context<%>)])]{
}

@defmethod[#:mode override 
           (after-delete)
           void?]{

Resets any error highlighting in this editor.


}

@defmethod[#:mode override 
           (after-insert)
           void?]{

Resets any error highlighting in this editor.


}

@defmethod[(display-results [results (list-of TST)])
           void?]{

This displays each of the elements of @racket[results] in the interactions
window, expect those elements of @racket[results] that are void. Those
are just ignored.


}

@defmethod[(evaluate-from-port [port input-port?]
                               [complete-program? boolean?]
                               [cleanup (-> void)])
           any]{
  Evaluates the program in the @racket[port] argument. If @racket[complete-program?]
  is @racket[#t], this method calls the
  @method[drracket:language:language<%> front-end/complete-program] to evaluate
  the program. If it is @racket[#f], it calls 
  @method[drracket:language:language<%> front-end/interaction] method.
  When evaluation finishes, it calls @racket[cleanup] on the user's main thread.

  Just before calling @racket[cleanup], this invokes the thunk in 
  @racket[drracket:rep:after-expression] (if any). It takes the value of
  the @racket[drracket:rep:after-expression] parameter on the DrRacket main thread,
  but invokes the thunk on the user's thread.
  
  This method must be called from the DrRacket main thread.
  }
                 
@defmethod[#:mode augment (after-many-evals) any]{
  Called from the DrRacket main thread after
  @method[drracket:rep:text% evaluate-from-port] finishes (no matter
  how it finishes).
}

@defmethod[(on-execute [run-on-user-thread (-> any)]) any]{

  Use @racket[run-on-user-thread] to initialize the user's parameters, etc.

  Called from the DrRacket thread after the language's
  @method[drracket:language:language<%> on-execute]
  method has been invoked, and after the
  special values have been setup (the ones registered
  via @racket[drracket:language:add-snip-value]).

  Do not print to @racket[current-output-port] or @racket[current-error-port]
  during the dynamic extent of the thunk passed to @racket[run-on-user-thread] becuase
  this can deadlock. IO is still, in general, fine, but the @racket[current-error-port]
  and @racket[current-output-port] are set to the user's ports that print
  into the interactions window and are not in a good state during those calls.
  
}

@defmethod[(get-error-range)
           (or/c false/c (list/c (is-a?/c text:basic%) number? number?))]{
@methspec{

Indicates the highlighted error range. The state for the
error range is shared across all instances of this class, so
there can only be one highlighted error region at a time.

}
@methimpl{

If @racket[#f], no region is highlighted. If a list, the first
element is the editor where the range is highlighted and the
second and third are the beginning and ending regions,
respectively.


}}

@defmethod[(get-user-custodian)
           (or/c false/c custodian?)]{
This is the custodian controlling the user's program.

}

@defmethod[(get-user-eventspace)
           (or/c false/c eventspace?)]{
This is the user's eventspace. The result of
@method[drracket:rep:text% get-user-thread] is the main thread of this eventspace.

}

@defmethod[(get-user-language-settings)
           language-settings]{
Returns the user's language-settings for the most recently
run program. Consider using
@method[drracket:unit:definitions-text<%> get-next-settings] instead, since the user may have selected a new language
since the program was last run.

}

@defmethod[(get-user-namespace)
           (or/c false/c namespace?)]{
Returns the user's namespace. This method
returns a new namespace each time Run is
clicked.

}

@defmethod[(get-user-thread)
           (or/c false/c thread?)]{
This method returns the thread that the user's code runs
in. It returns a different result each time the user
runs the program.

It is @racket[#f] before the first time the user click on
the Run button or the evaluation has been killed.

This thread has all of its parameters initialized according to the
settings of the current execution.
See @secref[#:doc '(lib "scribblings/reference/reference.scrbl")]{parameters}
for more information about parameters.

}

@defmethod[(highlight-errors 
            [locs (listof srcloc?)]
            [error-arrows (or/c #f (listof srcloc?)) #f])
           void?]{
Call this method to highlight errors associated with this repl.
See also
@method[drracket:rep:text% reset-highlighting], and
@method[drracket:rep:text% highlight-errors/exn].

This method highlights a series of dis-contiguous ranges in
the editor.

It puts the caret at the location of the first error.

}

@defmethod[(highlight-errors/exn [exn exn])
           void?]{

Highlights the errors associated with the exn (only syntax
and read errors -- does not extract any information from the
continuation marks)

See also 
@method[drracket:rep:text% highlight-errors].


}

@defmethod[(initialize-console) void?]{

This inserts the ``Welcome to DrRacket'' message into the interactions
buffer, calls
@method[drracket:rep:text% reset-console],
@method[drracket:rep:text% insert-prompt], and 
@method[editor<%> clear-undos].

Once the console is initialized, this method calls
@method[drracket:language:language<%> first-opened]. 
Accordingly, this method should not be called to initialize
a REPL when the user's evaluation is imminent. That is,
this method should be called when new tabs or new windows
are created, but not when the Run button is clicked. 

This method calls the 
@method[drracket:language:language<%> first-opened]
from the user's eventspace's main thread and, when 
@method[drracket:language:language<%> first-opened]
returns, it enqueue's a callback that ends
an edit sequence on the REPL and calls
@method[editor<%> clear-undos]. Accordingly, if the
@method[drracket:language:language<%> first-opened]
method does not return, the interactions text will
be in an unclosed edit sequence.
}

@defmethod[(insert-prompt)
           void?]{

Inserts a new prompt at the end of the text.
}

@defmethod[(kill-evaluation)
           void?]{
This method is called when the user chooses the kill menu item.

}

@defmethod[#:mode override 
           (on-close)
           void?]{

Calls 
@method[drracket:rep:text% shutdown].

Calls the super method.


}

@defmethod[(queue-output [thnk (-> void?)])
           void?]{
@methspec{

This method queues thunks for DrRacket's eventspace in a
special output-related queue.
}}

@defmethod[(reset-console)
           void?]{

Kills the old eventspace, and creates a new
parameterization for it.


}

@defmethod[(reset-highlighting)
           void?]{
This method resets the highlighting being displayed for this repl. See also:
@method[drracket:rep:text% highlight-errors], and
@method[drracket:rep:text% highlight-errors/exn].

}

@defmethod[(run-in-evaluation-thread [f ( -> void)])
           void?]{
@methspec{

This function runs its arguments in the user evaluation thread. This
thread is the same as the user's eventspace main thread.

}
@methimpl{

Calls @racket[f], after switching to the user's thread.


}}

@defmethod[(shutdown)
           void?]{
Shuts down the user's program and all windows. Reclaims any
resources the program allocated.  It is expected to be
called from DrRacket's main eventspace thread.

}

@defmethod[(wait-for-io-to-complete)
           void?]{
This waits for all pending IO in the rep to finish
and then returns.

This method must only be called from the main thread in
DrRacket's eventspace

}

@defmethod[(wait-for-io-to-complete/user)
           void?]{
This waits for all pending IO in the rep to finish
and then returns.

This method must only be called from the main thread
in the user's eventspace

}}


@defmixin[drracket:rep:drs-bindings-keymap-mixin (editor:keymap<%>) ()]{

This mixin adds some DrRacket-specific keybindings to the
editor it is mixed onto.



@defmethod[#:mode override 
           (get-keymaps)
           (listof (is-a?/c keymap%))]{

Calls the super method and adds in a keymap with the
DrRacket-specific keybindings:

@itemize[
@item{f5 - Run}

@item{c:x;o - toggles the focus between the definition and
interactions windows.}
]



}}


@definterface[drracket:rep:context<%> ()]{

Objects that match this interface provide all of the services that the 
@racket[drracket:rep:text%] class needs to connect with its context.



@defmethod[(clear-annotations)
           void?]{
@methspec{

Call this method to clear any annotations in the text before
executing or analyzing or other such activities that should
process the program.

Tools that annotate the program text should augment this
method to clear their own annotations on the program text.

DrRacket calls this method before a program is run (via the
Run button).

}
@methimpl{

Clears any error highlighting in the definitions window.



}}

@defmethod[(disable-evaluation)
           void?]{
Call this method to disable evaluation GUI evaluation while
some evaluation (or expansion) is taking place on another
thread.

Override this method if you add a GUI-based mechanism for
initiating evaluation in the frame.

This method is also called when the user switches tabs.

See also
@method[drracket:rep:context<%> enable-evaluation].

}

@defmethod[(enable-evaluation)
           void?]{
This method must disable the GUI controls that start
user-sponsored evaluation. It is called once the user starts
some evaluation to ensure that only one evaluation proceeds
at a time.

It is also called when the user switches tabs.

See also
@method[drracket:rep:context<%> disable-evaluation].

}

@defmethod[(ensure-rep-shown [rep (is-a?/c drracket:rep:text<%>)])
           void?]{

This method is called to force the rep window to be visible when, for
example, an error message is put into the rep. Also ensures
that the appropriate tab is visible, if necessary.


}

@defmethod[(get-breakables)
           (values (or/c thread? false/c) (or/c custodian? false/c))]{
Returns the last values passed to
@method[drracket:rep:context<%> set-breakables].

}

@defmethod[(get-directory)
           (union string false/c)]{
The result of this method is used as the initial directory for the
user's program to be evaluated in.

}

@defmethod[(needs-execution)
           (or/c string? false/c)]{
This method should return an explanatory string when the
state of the program that the repl reflects has changed. It
should return @racket[#f] otherwise.

}

@defmethod[(reset-offer-kill)
           void?]{
The break button typically offers to kill if it has been
pushed twice in a row. If this method is called, however, it
ignores any prior clicks.

}

@defmethod[(set-breakables [thread (or/c thread false/c)]
                           [custodian (or/c custodian false/c)])
           void?]{
Calling this method with a thread and a custodian means that
the next time the break button is clicked, it will either
break the thread or shutdown the custodian.

See also
@method[drracket:rep:context<%> get-breakables].

}

@defmethod[(update-running [running? any/c])
           void?]{

This method should update some display in the gui that
indicates whether or not evaluation is currently proceeding
in the user's world.


}}

@(tools-include "rep")
