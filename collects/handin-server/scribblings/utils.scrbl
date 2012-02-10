#lang scribble/doc
@(require "common.rkt")

@title{Utilities}

@defmodule[handin-server/utils]

@; JBC: have eli verify these contracts?

@defproc[(get-conf [key symbol?]) any/c]{

  Returns a value from the configuration file (useful for reading things
  like field names, etc.).  Known keys (see @secref{server-setup}) have
  defaults and some have their values go through a translation (for
  example, @racket['active-dirs] produces a list of complete paths).
  Other keys get neither, and an exception is raised if the @racket[key]
  is not specified.}

@defproc[(unpack-submission [submission bytes?])
         (values (is-a?/c text%) (is-a?/c text%))]{

  Returns two @racket[text%] objects corresponding to the submitted
  definitions and interactions windows.}

@defproc[(make-evaluator/submission
          [language (or/c module-path?
                          (list/c (one-of/c 'special) symbol?)
                          (list/c (one-of/c 'module) module-path?)
                          (cons/c (one-of/c 'begin) list?))]
          [require-paths (listof path-string?)]
          [content bytes?])
         (any/c . -> . any)]{

  Like @racket[make-evaluator], but the definitions content is
  supplied as a submission byte string.  The byte string is opened for
  reading, with line-counting enabled.

  In addition to the language specification for
  @racket[make-evaluator], the @racket[language] argument can be a
  list that begins with @racket['module].  In this case,
  @racket[make-module-language] is used to create an evaluator, and
  the module code must be using the specified language in its
  language position.  In this case, the @racket[requires-paths]
  argument is used only for paths that are allowed to be accessed (the
  @racket[_allow-read] argument to @racket[make-evaluator], since the
  submission is expected to be a complete submission.)}

@defproc[(call-with-evaluator
          [language (or/c module-path?
                          (list/c (one-of/c 'special) symbol?)
                          (list/c (one-of/c 'module) module-path?)
                          (cons/c (one-of/c 'begin) list?))]
          [require-paths (listof path-string?)]
          [input-program any/c]
          [proc (any/c . -> . any)])
         any]{

  Calls @racket[proc] with an evaluator for the given language,
  teachpack paths, and initial definition content as supplied by
  @racket[input-program] (see @racket[make-evaluator]).  It also sets
  the current error-value print handler to print values in a way
  suitable for @racket[language], it initializes
  @racket[set-run-status] with @racket["executing your code"], and it
  catches all exceptions to re-raise them in a form suitable as a
  submission error.  See @racket[make-evaluator/submission] for
  further details.}

@defproc[(call-with-evaluator/submission
          [language (or/c module-path?
                          (list/c (one-of/c 'special) symbol?)
                          (list/c (one-of/c 'module) module-path?)
                          (cons/c (one-of/c 'begin) list?))]
          [require-paths (listof path-string?)]
          [submission bytes?]
          [proc (any/c . -> . any)])
         any]{

  Like @racket[call-with-evaluator], but the definitions content is
  supplied as a byte string.  The byte string is opened for reading,
  with line-counting enabled.  See @racket[call-with-evaluator] and
  @racket[make-evaluator/submission] for further details.}

@; JBC: this contract is probably wrong
@; JBC: does this eval accept an optional namespace?
@defproc[(evaluate-all [source any]
                       [input-port port?]
                       [eval (any/c . -> . any)]) any]{
  Like @racket[load] on an input port.}

@defproc[(evaluate-submission [submission bytes?]
                              [eval (any/c . -> . any)])
         any]{

  Like @racket[load] on a submission byte string.}

@defproc[(check-proc [eval (any/c . -> . any)]
                     [expect-v any/c]
                     [compare-proc (any/c any/c . -> . any)]
                     [proc-name symbol?]
                     [arg any/c] ...)
         any]{

  Calls the function named @racket[proc-name] using the evaluator
  @racket[eval], giving it the (unquoted) arguments @racket[arg ...]
  Let @racket[result-v] be the result of the call; unless
  @racket[(compare-proc result-v expect-v)] is true, an exception is
  raised.}

Every exception or result mismatch during the call to
@racket[compare-proc] is phrased suitably for the handin client.

@defproc[(check-defined [eval (any/c . -> . any)]
                        [name symbol?])
         any]{

  Checks whether @racket[name] is defined in the evaluator
  @racket[eval], and raises an error if not (suitably phrased for the
  handin client).  If it is defined as non-syntax, its value is
  returned.  Warning: in the beginner language level, procedure
  definitions are bound as syntax.}

@; JBC: returns what? signals error?

@defproc[(look-for-tests [text (is-a?/c text%)] [name symbol?] [n number?])
         any]{

  Inspects the given @racket[text%] object to determine whether it
  contains at least @racket[n] tests for the function @racket[name].
  The tests must be top-level expressions.}

@defproc[(user-construct [eval (any/c . -> . any)]
                         [name symbol?]
                         [arg any/c] ...)
         any]{

  Like @racket[check-proc], but with no result checking.  This
  function is often useful for calling a student-defined constructor.}

@defparam[test-history-enabled on? any/c]{

  Controls how run-time errors are reported to the handin client.  If
  the parameter's value is true, then the complete sequence of tested
  expressions is reported to the handin client for any test failure.
  Set this parameter to true when testing programs that use state.}

@defproc*[([(message [string string?]) void?]
           [(message [string string?] [styles 'final]) void?]
           [(message [string string?]
                     [styles (listof (or/c 'ok 'ok-cancel 'yes-no
                                           'caution 'stop))])
            any])]{
  The first case of @racket[message] is intended to update the client on
  the current activity --- it updates the status line in the submission
  dialog box on the client.  Use it to indicate operations that might
  take a while and/or indicate progress during these operations.

  In the second case, where @racket['final] is used as a flag, does not
  show the text immediately --- instead, it causes it to be displayed in
  the status line after a successful submission instead of the usual
  ``Handin successful'' message.  This is useful for submissions that
  are accepted but had some problems.

  The third case, when @racket[styles] is a list of symbols, opens a
  @racket[message-box] dialog on the client side, and the resulting
  value is returned as the result of @racket[message]. The
  @racket[styles] list is passed as the @racket[style] argument to
  @racket[message-box].  You can use this to send warnings to the
  student or to ask for confirmation, for example, ``your submission
  does not pass 3 tests, continue?''.}

@defproc[(set-run-status [status (or/c false? string?)]) void?]{
  Registers information about the current actions of the checker, in
  case the session is terminated due to excessive memory consumption
  or a timeout.  For example, a checker might set the status to
  indicate which instructor-supplied test was being executed when the
  session aborted.}

@defparam[current-value-printer proc (any/c . -> . string?)]{
  Controls how values are printed. The @racket[proc] must be a
  procedure that expects a Racket value and returns a string
  representation for it.  The default value printer uses
  @racket[pretty-print], with DrRacket-like settings.}

@defproc[(reraise-exn-as-submission-problem [thunk (-> any)]) any]{

  Calls @racket[thunk] in a context that catches exceptions and
  re-raises them in a form suitable as a submission error.  It returns
  the value returned by @racket[thunk] if no exception occurs.}

@defproc[(log-line [fmt string?] [args any/c] ...) void?]{
  Produces a line in the server log file, using the given format
  string and arguments.  This function arranges to print the line fast
  (to avoid mixing lines from different threads) to the error port,
  and flush it.  (The log port will prefix all lines with a time stamp
  and a connection identifier.)}

@defproc[(timeout-control [msg any?]) void?]{

  Controls the timeout for this session.  The timeout is initialized
  by the value of the @racket[session-timeout] configuration entry,
  and the checker can use this procedure to further control it: if
  @racket[msg] is @racket['reset] the timeout is reset to
  @racket[session-timeout] seconds; if @racket[msg] is a number the
  timeout will be set to that many seconds in the future.  The timeout
  can be completely disabled using @racket['disable] or @racket[#f].
  (Note that before the checker is used (after the pre-checker, if
  specified), the timer will be reset to the @racket['session-timeout]
  value.)}

@defthing[server-dir path-string?]{

  The main directory the server is running from.  Useful for getting
  to files that are not local to a specific submission.}
