#lang scribble/manual
@(require scribble/manual
          scribble/eval
          "guide-utils.rkt"
          (for-label syntax/parse))

@title[#:tag "phases"]{General Phase Levels}

A @deftech{phase} can be thought of as a way to separate computations in
a pipeline of processes where one produces code that is used by the
next.  (E.g., a pipeline that consists of a preprocessor process, a
compiler, and an assembler.)

Imagine starting two Racket processes for this purpose.  If you ignore
inter-process communication channels like sockets and files, the
processes will have no way to share anything other than the text that is
piped from the standard output of one process into the standard input of
the other.  Similarly, Racket effectively allows multiple invocations of
a module to exist in the same process but separated by phase.  Racket
enforces @emph{separation} of such phases, where different phases cannot
communicate in any way other than via the protocol of macro expansion,
where the output of one phases is the code used in the next.

@section{Phases and Bindings}

Every binding of an identifier exists in a particular phase.  The link
between a binding and its phase is represented by an integer
@deftech{phase level}.  Phase level 0 is the phase used for ``plain''
(or ``runtime'') definitions, so

@racketblock[
(define age 5)
]

adds a binding for @racket[age] into phase level 0.  The identifier
@racket[age] can be defined at a higher phase level using
@racket[begin-for-syntax]:

@racketblock[
(begin-for-syntax
  (define age 5))
]

With a single @racket[begin-for-syntax] wrapper, @racket[age] is
defined at phase level 1.  We can easily mix these two definitions in
the same module or in a top-level namespace, and there is no clash
between the two @racket[age]s that are defined at different phase
levels:

@(define age-eval (make-base-eval))
@(interaction-eval #:eval age-eval (require (for-syntax racket/base)))

@interaction[#:eval age-eval
(define age 3)
(begin-for-syntax
  (define age 9))
]

The @racket[age] binding at phase level 0 has a value of 3, and the
@racket[age] binding at phase level 1 has a value of 9.

Syntax objects capture binding information as a first-class value.
Thus,

@racketblock[#'age]

is a syntax object that represents the @racket[age] binding---but
since there are two @racket[age]s (one at phase level 0 and one at
phase level 1), which one does it capture?  In fact, Racket imbues
@racket[#'age] with lexical information for all phase levels, so the
answer is that @racket[#'age] captures both.

The relevant binding of @racket[age] captured by @racket[#'age] is
determined when @racket[#'age] is eventually used.  As an example, we
bind @racket[#'age] to a pattern variable so we can use it in a
template, and then we @racket[eval]utae the template: @margin-note*{We
  use @racket[eval] here to demonstrate phases, but see
  @secref["reflection"] for caveats about @racket[eval].}

@interaction[#:eval age-eval
(eval (with-syntax ([age #'age])
        #'(displayln age)))
]

The result is @racket[3] because @racket[age] is used at phase 0 level.
We can try again with the use of @racket[age] inside
@racket[begin-for-syntax]:

@interaction[#:eval age-eval
(eval (with-syntax ([age #'age])
        #'(begin-for-syntax
            (displayln age))))
]

In this case, the answer is @racket[9], because we are using
@racket[age] at phase level 1 instead of 0 (i.e.,
@racket[begin-for-syntax] evaluates its expressions at phase level 1).
So, you can see that we started with the same syntax object,
@racket[#'age], and we were able to use it in two different ways: at
phase level 0 and at phase level 1.

A syntax object has a lexical context from the moment it first exists.
A syntax object that is provided from a module retains its lexical
context, and so it references bindings in the context of its source
module, not the context of its use.  The following example defines
@racket[button] at phase level 0 and binds it to @racket[0], while
@racket[see-button] binds the syntax object for @racket[button] in
module @racket[a]:

@interaction[
(module a racket
  (define button 0)
  (provide (for-syntax see-button))
  @code:comment[@#,t{Why not @racket[(define see-button #'button)]? We explain later.}]
  (define-for-syntax see-button #'button))

(module b racket
  (require 'a)
  (define button 8)
  (define-syntax (m stx)
    see-button)
  (m))

(require 'b)
]

The result of the @racket[m] macro is the value of @racket[see-button],
which is @racket[#'button] with the lexical context of the @racket[a]
module.  Even though there is another @racket[button] in @racket[b], the
second @racket[button] will not confuse Racket, because the lexical
context of @racket[#'button] (the value bound to @racket[see-button]) is
@racket[a].

Note that @racket[see-button] is bound at phase level 1 by virtue of
defining it with @racket[define-for-syntax].  Phase level 1 is needed
because @racket[m] is a macro, so its body executes at one phase higher
than the context of its definition.  Since @racket[m] is defined at
phase level 0, its body is at phase level 1, so any bindings referenced
by the body must be at phase level 1.

@; ======================================================================

@section{Phases and Modules}

A @tech{phase level} is a module-relative concept.  When importing from
another module via @racket[require], Racket lets us shift imported
bindings to a phase level that is different from the original one:

@racketblock[
(require "a.rkt")                @code:comment{import with no phase shift}
(require (for-syntax "a.rkt"))   @code:comment{shift phase by +1}
(require (for-template "a.rkt")) @code:comment{shift phase by -1}
(require (for-meta 5 "a.rkt" ))  @code:comment{shift phase by +5}
]

That is, using @racket[for-syntax] in @racket[require] means that all of
the bindings from that module will have their phase levels increased by
one.  A binding that is @racket[define]d at phase level 0 and imported
with @racket[for-syntax] becomes a phase-level 1 binding:

@interaction[
(module c racket
  (define x 0) @code:comment{defined at phase level 0}
  (provide x))

(module d racket
  (require (for-syntax 'c))
  @code:comment{has a binding at phase level 1, not 0:}
  #'x)
]

Let's see what happens if we try to create a binding for the
@racket[#'button] syntax object at phase level 0:

@(define button-eval (make-base-eval))
@(interaction-eval #:eval button-eval
                   (require (for-syntax racket/base)))
@interaction[#:eval button-eval
(define button 0)
(define see-button #'button)
]

Now both @racket[button] and @racket[see-button] are defined at phase
0.  The lexical context of @racket[#'button] will know that there is a
binding for @racket[button] at phase 0.  In fact, it seems like things
are working just fine if we try to @racket[eval] @racket[see-button]:

@interaction[#:eval button-eval
(eval see-button)
]

Now, let's use @racket[see-button] in a macro:

@interaction[#:eval button-eval
(define-syntax (m stx)
  see-button)
(m)
]

Clearly, @racket[see-button] is not defined at phase level 1, so we
cannot refer to it inside the macro body.  Let's try to use
@racket[see-button] in another module by putting the button definitions
in a module and importing it at phase level 1.  Then, we will get
@racket[see-button] at phase level 1:

@interaction[
(module a racket
  (define button 0)
  (define see-button #'button)
  (provide see-button))

(module b racket
  (require (for-syntax 'a)) @code:comment[@#,t{gets @racket[see-button] at phase level 1}]
  (define-syntax (m stx)
    see-button)
  (m))
]

Racket says that @racket[button] is unbound now!  When @racket[a] is
imported at phase level 1, we have the following bindings:

@racketblock[
button     @#,elem{at phase level 1}
see-button @#,elem{at phase level 1}
]

So the macro @racket[m] can see a binding for @racket[see-button] at
phase level 1 and will return the @racket[#'button] syntax object, which
refers to @racket[button] binding at phase level 1.  But the use of
@racket[m] is at phase level 0, and there is no @racket[button] at phase
level 0 in @racket[b].  That is why @racket[see-button] needs to be
bound at phase level 1, as in the original @racket[a].  In the original
@racket[b], then, we have the following bindings:

@racketblock[
button     @#,elem{at phase level 0}
see-button @#,elem{at phase level 1}
]

In this scenario, we can use @racket[see-button] in the macro, since
@racket[see-button] is bound at phase level 1.  When the macro expands,
it will refer to a @racket[button] binding at phase level 0.

Defining @racket[see-button] with @racket[(define see-button
#'button)] isn't inherently wrong; it depends on how we intend to use
@racket[see-button].  For example, we can arrange for @racket[m] to
sensibly use @racket[see-button] because it puts it in a phase level 1
context using @racket[begin-for-syntax]:

@interaction[
(module a racket
  (define button 0)
  (define see-button #'button)
  (provide see-button))

(module b racket
  (require (for-syntax 'a))
  (define-syntax (m stx)
    (with-syntax ([x see-button])
      #'(begin-for-syntax
          (displayln x))))
  (m))
]

In this case, module @racket[b] has both @racket[button] and
@racket[see-button] bound at phase level 1.  The expansion of the macro
is

@racketblock[
(begin-for-syntax
  (displayln button))
]

which works, because @racket[button] is bound at phase level 1.

Now, you might try to cheat the phase system by importing @racket[a] at
both phase level 0 and phase level 1.  Then you would have the following
bindings

@racketblock[
button     @#,elem{at phase level 0}
see-button @#,elem{at phase level 0}
button     @#,elem{at phase level 1}
see-button @#,elem{at phase level 1}
]

You might expect now that @racket[see-button] in a macro would work, but
it doesn't:

@interaction[
(module a racket
  (define button 0)
  (define see-button #'button)
  (provide see-button))

(module b racket
  (require 'a
           (for-syntax 'a))
  (define-syntax (m stx)
    see-button)
  (m))
]

The @racket[see-button] inside the @racket[m] macro comes from the
@racket[(for-syntax 'a)] import.  For the macro to work, there must be a
@racket[button] at phase 0 bound, and there is such a binding implied by
@racket[(require 'a)].  However, @racket[(require 'a)] and
@racket[(require (for-syntax 'a))] are @emph{different instantiations}
of the same module.  The @racket[see-button] at phase 1 only refers to
the @racket[button] at phase level 1, not the @racket[button] bound at
phase 0 from a different instantiation---even from the same source
module.

Mismatches like the one above can show up when a macro tries to match
literal bindings---using @racket[syntax-case] or @racket[syntax-parse].

@interaction[
(module x racket
  (require (for-syntax syntax/parse)
           (for-template racket/base))

  (provide (all-defined-out))

  (define button 0)
  (define (make) #'button)
  (define-syntax (process stx)
    (define-literal-set locals (button))
    (syntax-parse stx
      [(_ (n (~literal button))) #'#''ok])))

(module y racket
  (require (for-meta 1 'x)
           (for-meta 2 'x racket/base))

  (begin-for-syntax
    (define-syntax (m stx)
      (with-syntax ([out (make)])
        #'(process (0 out)))))

  (define-syntax (p stx)
    (m))

  (p))
]

In this example, @racket[make] is being used in @racket[y] at phase
level 2, and it returns the @racket[#'button] syntax object---which
refers to @racket[button] bound at phase level 0 inside @racket[x] and
at phase level 2 in @racket[y] from @racket[(for-meta 2 'x)].  The
@racket[process] macro is imported at phase level 1 from
@racket[(for-meta 1 'x)], and it knows that @racket[button] should be
bound at phase level 1.  When the @racket[syntax-parse] is executed
inside @racket[process], it is looking for @racket[button] bound at
phase level 1 but it sees only a phase level 2 binding and doesn't
match.

To fix the example, we can provide @racket[make] at phase level 1
relative to @racket[x], and then we import it at phase level 1 in
@racket[y]:

@interaction[
(module x racket
  (require (for-syntax syntax/parse)
           (for-template racket/base))

  (provide (all-defined-out))

  (define button 0)

  (provide (for-syntax make))
  (define-for-syntax (make) #'button)
  (define-syntax (process stx)
    (define-literal-set locals (button))
    (syntax-parse stx
      [(_ (n (~literal button))) #'#''ok])))

(module y racket
  (require (for-meta 1 'x)
           (for-meta 2 racket/base))

  (begin-for-syntax
    (define-syntax (m stx)
      (with-syntax ([out (make)])
        #'(process (0 out)))))

  (define-syntax (p stx)
    (m))

  (p))

(require 'y)
]

@(close-eval age-eval)
@(close-eval button-eval)
