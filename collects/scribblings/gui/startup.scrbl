#lang scribble/doc
@(require "common.rkt" (for-label racket/gui/dynamic))

@title{Startup Actions}

The @racketmodname[racket/gui/base] module can be instantiated only
once per operating-system process, because it sets hooks in the Racket
run-time system to coordinate between Racket thread scheduling and GUI
events. Attempting to instantiate it a second time results in an
exception.

Loading @racketmodname[racket/gui/base] sets two parameters:

@itemlist[

@item{@racket[executable-yield-handler] --- The executable yield
      handler is set to evaluate @racket[(yield _initial-eventspace)]
      before chaining to the previously installed handler. As a
      result, the Racket process will normally wait until all
      top-level windows are closed, all callbacks are invoked, and all
      timers are stopped in the initial eventspace before the process
      exits.}

@item{@racket[current-get-interaction-input-port] --- The interaction
      port handler is set to wrap the previously installed handler's
      result to yield to GUI events when the input port blocks on
      reading. This extension of the default handler's behavior is
      triggered only when the current thread is the handler thread of
      some eventspace, in which case @racket[current-eventspace] is
      set to the eventspace before invoking @racket[yield]. As a
      result, GUI events normally can be handled while
      @racket[read-eval-print-loop] (such as run by the plain Racket
      executable) is blocked on input.}

]

