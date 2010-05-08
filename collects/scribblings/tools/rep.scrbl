#lang scribble/doc
@(require "common.ss")
@(tools-title "rep")


@definterface[drracket:rep:text<%> ()]{
}


@defclass[drracket:rep:text% scheme:text% (drracket:rep:text<%>)]{

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

This displays each of the elements of @scheme[results] in the interactions
window, expect those elements of @scheme[results] that are void. Those
are just ignored.


}

@defmethod[(do-many-evals [run-loop (((-> void) -> void) -> void)])
           void?]{
@methspec{

Use this function to evaluate code or run actions that should mimic
the user's interactions. For example, DrRacket uses this function to
evaluate expressions in the definitions window and expressions
submitted at the prompt.

}
@methimpl{

The function @scheme[run-loop] is called. It is expected to loop, calling
it's argument with a thunk that corresponds to the user's
evaluation. It should call it's argument once for each expression the
user is evaluating.  It should pass a thunk to it's argument that
actually does the users's evaluation.


}}

@defmethod[(do-many-text-evals [text (is-a?/c text%)]
                               [start int]
                               [end int ]
                               [complete-program? any/c])
           void?]{
@methspec{

This function evaluates all of the expressions in a text.

}
@methimpl{

It evaluates all of the expressions in @scheme[text] starting at
@scheme[start] and ending at @scheme[end], calling
@method[drracket:rep:text% do-many-evals] to handle the evaluation.

The @scheme[complete-program?] argument determines if the
@method[drracket:language:language<%> front-end/complete-program] method or the
@method[drracket:language:language<%> front-end/interaction] method is called.


}}

@defmethod[(evaluate-from-port [port input-port?]
                               [complete-program? boolean?]
                               [cleanup (-> void)])
           any]{
  Evaluates the program in the @scheme[port] argument. If @scheme[complete-program?]
  is @scheme[#t], this method calls the
  @method[drracket:language:language<%> front-end/complete-program] to evaluate
  the program. If it is @scheme[#f], it calls 
  @method[drracket:language:language<%> front-end/interaction] method.
  When evaluation finishes, it calls @scheme[cleanup] on the user's main thread.

  This method must be called from the DrRacket main thread.
}
                 
@defmethod[#:mode augment (after-many-evals) any]{
  Called from the DrRacket main thread after
  @method[drracket:rep:text% evaluate-from-port] finishes (no matter
  how it finishes).
}

@defmethod[#:mode augment (on-execute [run-on-user-thread (-> any)]) any]{

  Called from the DrRacket thread after the language's
  @method[drracket:language:language<%> on-execute]
  method has been invoked, and after the
  special values have been setup (the ones registered
  via @scheme[drracket:language:add-snip-value]).

  Use @scheme[run-on-user-thread] to initialize the user's parameters, etc.

}

@defmethod[(get-error-range)
           (or/c false/c (list/c (is-a?/c text:basic%) number? number?))]{
@methspec{

Indicates the highlighted error range. The state for the
error range is shared across all instances of this class, so
there can only be one highlighted error region at a time.

}
@methimpl{

If @scheme[#f], no region is highlighted. If a list, the first
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
This method returns the thread that the users code runs
in. It is returns a different result, each time the user
runs the program.

It is @scheme[#f] before the first time the user click on
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

@defmethod[(initialize-console)
           void?]{

This inserts the ``Welcome to DrRacket'' message into the interactions
buffer, calls
@method[drracket:rep:text% reset-console],
@method[drracket:rep:text% insert-prompt], and 
@method[editor<%> clear-undos].

Once the console is initialized, this method calls
@method[drracket:language:language<%> first-opened]. Accordingly, this method should not be called to initialize
a REPL when the user's evaluation is imminent. That is,
this method should be called when new tabs or new windows
are created, but not when the Run button is clicked. 


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

This function runs it's arguments in the user evaluation thread. This
thread is the same as the user's eventspace main thread.

See also  
@method[drracket:rep:text% do-many-evals].

}
@methimpl{

Calls @scheme[f], after switching to the user's thread.


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
@scheme[drracket:rep:text%] class needs to connect with it's context.



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
should return @scheme[#f] otherwise.

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
