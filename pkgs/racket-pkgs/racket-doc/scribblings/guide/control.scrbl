#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define cc-eval (make-base-eval))

@title[#:tag "control" #:style 'toc]{Exceptions and Control}

Racket provides an especially rich set of control operations---not
only operations for raising and catching exceptions, but also
operations for grabbing and restoring portions of a computation.

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "exns"]{Exceptions}

Whenever a run-time error occurs, an @deftech{exception} is
raised. Unless the exception is caught, then it is handled by printing
a message associated with the exception, and then escaping from the
computation.

@interaction[
(/ 1 0)
(car 17)
]

To catch an exception, use the @racket[with-handlers] form:

@specform[
(with-handlers ([predicate-expr handler-expr] ...)
  body ...+)
]{}

Each @racket[_predicate-expr] in a handler determines a kind of
exception that is caught by the @racket[with-handlers] form, and the
value representing the exception is passed to the handler procedure
produced by @racket[_handler-expr].  The result of the
@racket[_handler-expr] is the result of the @racket[with-handlers]
expression.

For example, a divide-by-zero error raises an instance of the
@racket[exn:fail:contract:divide-by-zero] structure type:

@interaction[
(with-handlers ([exn:fail:contract:divide-by-zero?
                 (lambda (exn) +inf.0)])
  (/ 1 0))
(with-handlers ([exn:fail:contract:divide-by-zero?
                 (lambda (exn) +inf.0)])
  (car 17))
]

The @racket[error] function is one way to raise your own exception. It
packages an error message and other information into an
@racket[exn:fail] structure:

@interaction[
(error "crash!")
(with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
  (error "crash!"))
]

The @racket[exn:fail:contract:divide-by-zero] and @racket[exn:fail]
structure types are sub-types of the @racket[exn] structure
type. Exceptions raised by core forms and functions always raise an
instance of @racket[exn] or one of its sub-types, but an exception
does not have to be represented by a structure. The @racket[raise]
function lets you raise any value as an exception:

@interaction[
(raise 2)
(with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) 'two)])
  (raise 2))
(with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) 'two)])
  (/ 1 0))
]

Multiple @racket[_predicate-expr]s in a @racket[with-handlers] form
let you handle different kinds of exceptions in different ways. The
predicates are tried in order, and if none of them match, then the
exception is propagated to enclosing contexts.

@interaction[
(define (always-fail n)
  (with-handlers ([even? (lambda (v) 'even)]
                  [positive? (lambda (v) 'positive)])
    (raise n)))
(always-fail 2)
(always-fail 3)
(always-fail -3)
(with-handlers ([negative? (lambda (v) 'negative)])
 (always-fail -3))
]

Using @racket[(lambda (v) #t)] as a predicate captures all exceptions, of course:

@interaction[
(with-handlers ([(lambda (v) #t) (lambda (v) 'oops)])
  (car 17))
]

Capturing all exceptions is usually a bad idea, however. If the user
types Ctl-C in a terminal window or clicks the @onscreen{Stop} button
in DrRacket to interrupt a computation, then normally the
@racket[exn:break] exception should not be caught. To catch only
exceptions that represent errors, use @racket[exn:fail?] as the
predicate:

@interaction[
(with-handlers ([exn:fail? (lambda (v) 'oops)])
  (car 17))
(eval:alts ; `examples' doesn't catch break exceptions!
 (with-handlers ([exn:fail? (lambda (v) 'oops)])
   (break-thread (current-thread)) (code:comment @#,t{simulate Ctl-C})
   (car 17))
 (error "user break"))
]

@; ----------------------------------------

@section[#:tag "prompt"]{Prompts and Aborts}

When an exception is raised, control escapes out of an arbitrary deep
evaluation context to the point where the exception is caught---or all
the way out if the exception is never caught:

@interaction[
(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (/ 1 0)))))))
]

But if control escapes ``all the way out,'' why does the @tech{REPL}
keep going after an error is printed? You might think that it's
because the @tech{REPL} wraps every interaction in a
@racket[with-handlers] form that catches all exceptions, but that's
not quite the reason.

The actual reason is that the @tech{REPL} wraps the interaction with a
@deftech{prompt}, which effectively marks the evaluation context with
an escape point. If an exception is not caught, then information about
the exception is printed, and then evaluation @deftech{aborts} to the
nearest enclosing prompt. More precisely, each prompt has a
@deftech{prompt tag}, and there is a designated @deftech{default
prompt tag} that the uncaught-exception handler uses to @tech{abort}.

The @racket[call-with-continuation-prompt] function installs a prompt
with a given @tech{prompt tag}, and then it evaluates a given thunk
under the prompt. The @racket[default-continuation-prompt-tag]
function returns the @tech{default prompt tag}. The
@racket[abort-current-continuation] function escapes to the nearest
enclosing prompt that has a given @tech{prompt tag}.

@interaction[
(define (escape v)
  (abort-current-continuation
   (default-continuation-prompt-tag)
   (lambda () v)))
(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))
(+ 1
   (call-with-continuation-prompt
    (lambda ()
      (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0))))))))
    (default-continuation-prompt-tag)))
]

In @racket[escape] above, the value @racket[v] is wrapped in a
procedure that is called after escaping to the enclosing prompt.

@tech{Prompts} and @tech{aborts} look very much like exception
handling and raising. Indeed, prompts and aborts are essentially a
more primitive form of exceptions, and @racket[with-handlers] and
@racket[raise] are implemented in terms of prompts and aborts. The
power of the more primitive forms is related to the word
``continuation'' in the operator names, as we discuss in the next
section.

@; ----------------------------------------------------------------------

@section[#:tag "conts"]{Continuations}


A @deftech{continuation} is a value that encapsulates a piece of an
expression's evaluation context. The @racket[call-with-composable-continuation]
function captures the @deftech{current continuation} starting outside
the current function call and running up to the nearest enclosing
prompt. (Keep in mind that each @tech{REPL} interaction is implicitly
wrapped in a prompt.)

For example, in

@racketblock[
(+ 1 (+ 1 (+ 1 0)))
]

at the point where @racket[0] is evaluated, the expression context
includes three nested addition expressions. We can grab that context by
changing @racket[0] to grab the continuation before returning 0:

@interaction[
#:eval cc-eval
(define saved-k #f)
(define (save-it!)
  (call-with-composable-continuation
   (lambda (k) (code:comment @#,t{@racket[k] is the captured continuation})
     (set! saved-k k)
     0)))
(+ 1 (+ 1 (+ 1 (save-it!))))
]

The @tech{continuation} saved in @racket[save-k] encapsulates the
program context @racket[(+ 1 (+ 1 (+ 1 _?)))], where @racket[_?]
represents a place to plug in a result value---because that was the
expression context when @racket[save-it!] was called. The
@tech{continuation} is encapsulated so that it behaves like the
function @racket[(lambda (v) (+ 1 (+ 1 (+ 1 v))))]:

@interaction[
#:eval cc-eval
(saved-k 0)
(saved-k 10)
(saved-k (saved-k 0))
]

The continuation captured by
@racket[call-with-composable-continuation] is determined dynamically,
not syntactically. For example, with

@interaction[
#:eval cc-eval
(define (sum n)
  (if (zero? n)
      (save-it!)
      (+ n (sum (sub1 n)))))
(sum 5)
]

the continuation in @racket[saved-k] becomes @racket[(lambda (x) (+ 5
(+ 4 (+ 3 (+ 2 (+ 1 x))))))]:

@interaction[
#:eval cc-eval
(saved-k 0)
(saved-k 10)
]

A more traditional continuation operator in Racket (or Scheme) is
@racket[call-with-current-continuation], which is usually abbreviated
@racket[call/cc]. It is like
@racket[call-with-composable-continuation], but applying the captured
continuation first @tech{aborts} (to the current @tech{prompt}) before
restoring the saved continuation. In addition, Scheme systems
traditionally support a single prompt at the program start, instead of
allowing new prompts via
@racket[call-with-continuation-prompt]. Continuations as in Racket
are sometimes called @deftech{delimited continuations}, since a
program can introduce new delimiting prompts, and continuations as
captured by @racket[call-with-composable-continuation] are sometimes
called @deftech{composable continuations}, because they do not have a
built-in @tech{abort}.

For an example of how @tech{continuations} are useful, see
@other-manual['(lib "scribblings/more/more.scrbl")]. For specific
control operators that have more convenient names than the primitives
described here, see @racketmodname[racket/control].

@; ----------------------------------------------------------------------

@close-eval[cc-eval]
