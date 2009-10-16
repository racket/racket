#lang scribble/doc
@(require "base.ss")

@title{Checks}

Checks are the basic building block of SchemeUnit.  A check
checks some condition.  If the condition holds the check
evaluates to @scheme[#t].  If the condition doesn't hold the
check raises an instance of @scheme[exn:test:check] with
information detailing the failure.

Although checks are implemented as macros, which is
necessary to grab source location, they are conceptually
functions.  This means, for instance, checks always evaluate
their arguments.  You can use check as first class
functions, though you will lose precision in the reported
source locations if you do so.

The following are the basic checks SchemeUnit provides.  You
can create your own checks using @scheme[define-check].

@defproc[(check (op (-> any any any))
                (v1 any)
                (v2 any)
		(message string? ""))
         any]{

The simplest check.  Succeeds if @scheme[op] applied to @scheme[v1] and @scheme[v2] is not @scheme[#f], otherwise raises an exception of type @scheme[exn:test:check].  The optional @scheme[message] is included in the output if the check fails. If the check succeeds, the value returned by @scheme[op] is the value returned by the check.}

For example, the following check succeeds:

@schemeblock[
  (check < 2 3)
]

@defproc*[([(check-eq? (v1 any) (v2 any) (message string? "")) #t]
           [(check-not-eq? (v1 any) (v2 any) (message string? "")) #t]
           [(check-eqv? (v1 any) (v2 any) (message string? "")) #t]
           [(check-equal? (v1 any) (v2 any) (message string? "")) #t]
           [(check-not-equal? (v1 any) (v2 any) (message string? "")) #t])]{

Checks that @scheme[v1] is (not) @scheme[eq?],
@scheme[eqv?], or @scheme[equal?] to @scheme[v2]. The
optional @scheme[message] is included in the output if the
check fails.}

For example, the following checks all fail:

@schemeblock[
  (check-eq? (list 1) (list 1) "allocated data not eq?")
  (check-not-eq? 1 1 "integers are eq?")
  (check-eqv? 1 1.0 "not eqv?")
  (check-equal? 1 1.0 "not equal?")
  (check-not-equal? (list 1) (list 1) "equal?")
]

@defproc[(check-pred (pred (-> any any)) (v any) (message string? ""))
        #t]{Checks that @scheme[pred] returns a value that is not @scheme[#f] when applied to @scheme[v].  The optional @scheme[message] is included in the output if the check fails. The value returned by a successful check is the value returned by @scheme[pred].}

Here's an example that passes and an example that fails:

@schemeblock[
  (check-pred string? "I work")
  (check-pred number? "I fail")
]


@defproc[(check-= (v1 any) (v2 any) (epsilon number?) (message string? "")) #t]{

Checks that @scheme[v1] and @scheme[v2] are within
@scheme[epsilon] of one another.  The optional
@scheme[message] is included in the output if the check
fails.}

Here's an example that passes and an example that fails:

@schemeblock[
  (check-= 1.0 1.01 0.01 "I work")
  (check-= 1.0 1.01 0.005 "I fail")
]

@defproc*[([(check-true (v any) (message string? "")) #t]
           [(check-false (v any) (message string? "")) #t]
           [(check-not-false (v any) (message string? "")) #t])]{

Checks that @scheme[v] is @scheme[#t], @scheme[#f], or not
@scheme[#f] as appropriate.  The optional @scheme[message]
is included in the output if the check fails.}

For example, the following checks all fail:

@schemeblock[
  (check-true 1)
  (check-false 1)
  (check-not-false #f)
]


@defproc[(check-exn (exn-predicate (-> any (or/c #t #f))) (thunk (-> any)) (message string? ""))
         #t]{

Checks that @scheme[thunk] raises an exception for which
@scheme[exn-predicate] returns @scheme[#t].  The optional
@scheme[message] is included in the output if the check
fails.  A common error is to use an expression instead of a
function of no arguments for @scheme[thunk].  Remember that
checks are conceptually functions.}

Here are two example, one showing a test that succeeds, and one showing a common error:

@schemeblock[
  (check-exn exn? 
             (lambda ()
               (raise (make-exn "Hi there"
                                (current-continuation-marks)))))
  (code:comment "Forgot to wrap the expression in a thunk.  Don't do this!")
  (check-exn exn?
             (raise (make-exn "Hi there"
                              (current-continuation-marks))))
]

@defproc[(check-not-exn (thunk (-> any)) (message string? "")) #t]{

Checks that @scheme[thunk] does not raise any exceptions.
The optional @scheme[message] is included in the output if
the check fails.}

@defproc[(fail (message string? "")) #t]{This checks fails unconditionally.  Good for creating test stubs that youintend to fill out later.  The optional @scheme[message] is included in the output if the check fails.}


@defproc[(check-regexp-match (regexp regexp?) (string string?)) #t]{Checks that @scheme[regexp] matches the @scheme[string].}

The following check will succeed:

@schemeblock[(check-regexp-match "a+bba" "aaaaaabba")]

This check will fail:

@schemeblock[(check-regexp-match "a+bba" "aaaabbba")]



@section{Augmenting Information on Check Failure}

When an check fails it stores information including the name
of the check, the location and message (if available), the
expression the check is called with, and the parameters to
the check.  Additional information can be stored by using
the @scheme[with-check-info*] function, and the
@scheme[with-check-info] macro.

@defstruct[check-info ([name symbol?] [value any])]{

A check-info structure stores information associated
with the context of execution of an check.}

The are several predefined functions that create check
information structures with predefined names.  This avoids
misspelling errors:

@defproc*[([(make-check-name (name string?)) check-info?]
           [(make-check-params (params (listof any))) check-info?]
	   [(make-check-location (loc (list/c any (or/c number? #f) (or/c number? #f) 
                                                  (or/c number? #f) (or/c number? #f))))
            check-info?]
     	   [(make-check-expression (msg any)) check-info?]
	   [(make-check-message (msg string?)) check-info?]
	   [(make-check-actual (param any)) check-info?]
	   [(make-check-expected (param any)) check-info?])]{}

@defproc[(with-check-info* (info (listof check-info?)) (thunk (-> any))) any]{

Stores the given @scheme[info] on the check-info stack for
the duration (the dynamic extent) of the execution of
@scheme[thunk]}

Example:

@schemeblock[  
  (with-check-info*
   (list (make-check-info 'time (current-seconds)))
   (lambda () (check = 1 2)))
]

When this check fails the message

@verbatim{time: <current-seconds-at-time-of-running-check>}

will be printed along with the usual information on an
check failure.

@defform[(with-check-info ((name val) ...) body ...)]{

The @scheme[with-check-info] macro stores the given
information in the check information stack for the duration
of the execution of the body expressions.  @scheme[Name] is
a quoted symbol and @scheme[val] is any value.}

Example:

@schemeblock[
 (for-each
  (lambda (elt)
    (with-check-info
     (('current-element elt))
     (check-pred odd? elt)))
  (list 1 3 5 7 8))
]

When this test fails the message

@verbatim{current-element: 8}

will be displayed along with the usual information on an
check failure.



@section{Custom Checks}

Custom checks can be defined using @scheme[define-check] and
its variants.  To effectively use these macros it is useful
to understand a few details about a check's evaluation
model.

Firstly, a check should be considered a function, even
though most uses are actually macros.  In particular, checks
always evaluate their arguments exactly once before
executing any expressions in the body of the checks.  Hence
if you wish to write checks that evalute user defined code
that code must be wrapped in a thunk (a function of no
arguments) by the user.  The predefined @scheme[check-exn]
is an example of this type of check.

It is also useful to understand how the check information
stack operates.  The stack is stored in a parameter and the
@scheme[with-check-info] forms evaluate to calls to
@scheme[parameterize].  Hence check information has lexical
scope.  For this reason simple checks (see below) cannot
usefully contain calls to @scheme[with-check-info] to report
additional information.  All checks created using
@scheme[define-simple-check] or @scheme[define-check] grab
some information by default: the name of the checks and the
values of the parameters.  Additionally the macro forms of
checks grab location information and the expressions passed
as parameters.

@defform[(define-simple-check (name param ...) expr ...)]{

The @scheme[define-simple-check] macro constructs a check
called @scheme[name] that takes the params and an optional
message as arguments and evaluates the @scheme[expr]s.  The
check fails if the result of the @scheme[expr]s is
@scheme[#f].  Otherwise the check succeeds.  Note that
simple checks cannot report extra information using
@scheme[with-check-info].}

Example:

To define a check @scheme[check-odd?]

@schemeblock[
  (define-simple-check (check-odd? number)
    (odd? number))
]

We can use these checks in the usual way:

@schemeblock[
  (check-odd? 3)  (code:comment "Success")
  (check-odd? 2)  (code:comment "Failure")
]

@defform*[[(define-binary-check (name pred actual expected))
          (define-binary-check (name actual expected) expr ...)]]{

The @scheme[define-binary-check] macro constructs a check
that tests a binary predicate.  It's benefit over
@scheme[define-simple-check] is in better reporting on check
failure.  The first form of the macro accepts a binary
predicate and tests if the predicate holds for the given
values.  The second form tests if @scheme[expr] non-false.
}

Examples:

Here's the first form, where we use a predefined predicate
to construct a binary check:

@schemeblock[
  (define-binary-check (check-char=? char=? actual expected))
]

In use:

@schemeblock[
  (check-char=? (read-char a-port) #\a)
]

If the expression is more complicated the second form should
be used.  For example, below we define a binary check that
tests a number if within 0.01 of the expected value:

@schemeblock[
  (define-binary-check (check-in-tolerance actual expected)
    (< (abs (- actual expected)) 0.01))
]


@defform[(define-check (name param ...) expr ...)]{

The @scheme[define-check] macro acts in exactly the same way
as @scheme[define-simple-check], except the check only fails
if the macro @scheme[fail-check] is called in the body of
the check.  This allows more flexible checks, and in
particular more flexible reporting options.}

@defform[(fail-check)]{The @scheme[fail-check] macro raises an @scheme[exn:test:check] with
the contents of the check information stack.}


@section{The Check Evaluation Context}

The semantics of checks are determined by the parameters
@scheme[current-check-around] and
@scheme[current-check-handler].  Other testing form such as
@scheme[test-begin] and @scheme[test-suite] change the value
of these parameters.

@defparam[current-check-handler handler (-> any/c any/c)]{

Parameter containing the function that handles exceptions
raised by check failures.  The default behaviour is to print
an error message including the exception message and stack
trace.  }

@defparam[current-check-around check (-> thunk any/c)]{

Parameter containing the function that handles the execution
of checks.  The default value wraps the evaluation of
@scheme[thunk] in a @scheme[with-handlers] call that calls
@scheme[current-check-handler] if an exception is raised and then
(when an exception is not raised) discards the result, returning
@scheme[(void)]. 
}
