#lang scribble/doc
@(require "common.ss")

@title{Utilities}

@defmodule[handin-server/utils]

@; JBC: have eli verify these contracts?

@defproc[(get-conf [key symbol?]) any/c]{

  Returns a value from the configuration file (useful for reading
  things like field names, etc.).}

@defproc[(unpack-submission [submission bytes?])
         (values (is-a?/c text%) (is-a?/c text%))]{

  Returns two @scheme[text%] objects corresponding to the submitted
  definitions and interactions windows.}

@defproc[(make-evaluator/submission
          [language (or/c module-path?
                          (list/c (one-of/c 'special) symbol?)
                          (list/c (one-of/c 'module) module-path?)
                          (cons/c (one-of/c 'begin) list?))]
          [require-paths (listof path-string?)]
          [content bytes?])
         (any/c . -> . any)]{

  Like @scheme[make-evaluator], but the definitions content is
  supplied as a submission byte string.  The byte string is opened for
  reading, with line-counting enabled.

  In addition to the language specification for
  @scheme[make-evaluator], the @scheme[language] argument can be a
  list that begins with @scheme['module].  In this case,
  @scheme[make-module-language] is used to create an evaluator, and
  the module code must be using the specified language in its
  language position.  In this case, the @scheme[requires-paths]
  argument is used only for paths that are allowed to be accessed (the
  @scheme[_allow-read] argument to @scheme[make-evaluator], since the
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

  Calls @scheme[proc] with an evaluator for the given language,
  teachpack paths, and initial definition content as supplied by
  @scheme[input-program] (see @scheme[make-evaluator]).  It also sets
  the current error-value print handler to print values in a way
  suitable for @scheme[language], it initializes
  @scheme[set-run-status] with @scheme["executing your code"], and it
  catches all exceptions to re-raise them in a form suitable as a
  submission error.  See @scheme[make-evaluator/submission] for
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

  Like @scheme[call-with-evaluator], but the definitions content is
  supplied as a byte string.  The byte string is opened for reading,
  with line-counting enabled.  See @scheme[call-with-evaluator] and
  @scheme[make-evaluator/submission] for further details.}

@; JBC: this contract is probably wrong
@; JBC: does this eval accept an optional namespace?
@defproc[(evaluate-all [source any]
                       [input-port port?]
                       [eval (any/c . -> . any)]) any]{
  Like @scheme[load] on an input port.}

@defproc[(evaluate-submission [submission bytes?]
                              [eval (any/c . -> . any)])
         any]{

  Like @scheme[load] on a submission byte string.}

@defproc[(check-proc [eval (any/c . -> . any)]
                     [expect-v any/c]
                     [compare-proc (any/c any/c . -> . any)]
                     [proc-name symbol?]
                     [arg any/c] ...)
         any]{

  Calls the function named @scheme[proc-name] using the evaluator
  @scheme[eval], giving it the (unquoted) arguments @scheme[arg ...]
  Let @scheme[result-v] be the result of the call; unless
  @scheme[(compare-proc result-v expect-v)] is true, an exception is
  raised.}

Every exception or result mismatch during the call to
@scheme[compare-proc] is phrased suitably for the handin client.

@defproc[(check-defined [eval (any/c . -> . any)]
                        [name symbol?])
         any]{

  Checks whether @scheme[name] is defined in the evaluator
  @scheme[eval], and raises an error if not (suitably phrased for the
  handin client).  If it is defined as non-syntax, its value is
  returned.  Warning: in the beginner language level, procedure
  definitions are bound as syntax.}

@; JBC: returns what? signals error?

@defproc[(look-for-tests [text (is-a?/c text%)] [name symbol?] [n number?])
         any]{

  Inspects the given @scheme[text%] object to determine whether it
  contains at least @scheme[n] tests for the function @scheme[name].
  The tests must be top-level expressions.}

@defproc[(user-construct [eval (any/c . -> . any)]
                         [name symbol?]
                         [arg any/c] ...)
         any]{

  Like @scheme[check-proc], but with no result checking.  This
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
  The first case of @scheme[message] is intended to update the client on
  the current activity --- it updates the status line in the submission
  dialog box on the client.  Use it to indicate operations that might
  take a while and/or indicate progress during these operations.

  In the second case, where @scheme['final] is used as a flag, does not
  show the text immediately --- instead, it causes it to be displayed in
  the status line after a successful submission instead of the usual
  ``Handin successful'' message.  This is useful for submissions that
  are accepted but had some problems.

  The third case, when @scheme[styles] is a list of symbols, opens a
  @scheme[message-box] dialog on the client side, and the resulting
  value is returned as the result of @scheme[message]. The
  @scheme[styles] list is passed as the @scheme[style] argument to
  @scheme[message-box].  You can use this to send warnings to the
  student or to ask for confirmation, for example, ``your submission
  does not pass 3 tests, continue?''.}

@defproc[(set-run-status [status (or/c false? string?)]) void?]{
  Registers information about the current actions of the checker, in
  case the session is terminated due to excessive memory consumption
  or a timeout.  For example, a checker might set the status to
  indicate which instructor-supplied test was being executed when the
  session aborted.}

@defparam[current-value-printer proc (any/c . -> . string?)]{
  Controls how values are printed. The @scheme[proc] must be a
  procedure that expects a Scheme value and returns a string
  representation for it.  The default value printer uses
  @scheme[pretty-print], with DrRacket-like settings.}

@defproc[(reraise-exn-as-submission-problem [thunk (-> any)]) any]{

  Calls @scheme[thunk] in a context that catches exceptions and
  re-raises them in a form suitable as a submission error.  It returns
  the value returned by @scheme[thunk] if no exception occurs.}

@defproc[(log-line [fmt string?] [args any/c] ...) void?]{
  Produces a line in the server log file, using the given format
  string and arguments.  This function arranges to print the line fast
  (to avoid mixing lines from different threads) to the error port,
  and flush it.  (The log port will prefix all lines with a time stamp
  and a connection identifier.)}

@defproc[(timeout-control [msg string?]) void?]{

  Controls the timeout for this session.  The timeout is initialized
  by the value of the @scheme[session-timeout] configuration entry,
  and the checker can use this procedure to further control it: if
  @scheme[msg] is @scheme['reset] the timeout is reset to
  @scheme[session-timeout] seconds; if @scheme[msg] is a number the
  timeout will be set to that many seconds in the future.  The timeout
  can be completely disabled by @scheme[(timeout-control #f)].  (Note
  that before the checker is used (after the pre-checker, if
  specified), the timer will be reset to the @scheme['session-timeout]
  value.)}

@defthing[server-dir path-string?]{

  The main directory the server is running from.  Useful for getting
  to files that are not local to a specific submission.}
