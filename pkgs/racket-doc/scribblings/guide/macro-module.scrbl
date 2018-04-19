#lang scribble/manual
@(require scribble/manual
          scribble/examples
          "guide-utils.rkt")

@(define visit-eval (make-base-eval))

@examples[
#:hidden
#:eval visit-eval
(current-pseudo-random-generator (make-pseudo-random-generator))
;; Make the output deterministic:
(random-seed 11)
]

@title[#:tag "macro-module"]{Module Instantiations and Visits}

Modules often contain just function and structure-type definitions, in
which case the module itself behaves in a purely functional way, and
the time when the functions are created is not observable. If a
module's top-level expressions include side effects, however, then the
timing of the effects can matter. The distinction between module
declaration and @tech{instantiation} provides some control over that
timing. The concept of module @tech{visits} further explains the
interaction of effects with macro implementations.

@; ----------------------------------------
@section{Declaration versus Instantiation}

Declaring a module does not immediately evaluate expressions in the
module's body. For example, evaluating

@examples[
#:label #f
#:eval visit-eval
(module number-n racket/base
  (provide n)
  (define n (random 10))
  (printf "picked ~a\n" n))
]

declares the module @racket[number-n], but it doesn't immediately pick a
random number for @racket[n] or display the number. A @racket[require]
of @racket[number-n] causes the module to be @deftech{instantiated}
(i.e., it triggers an @deftech{instantiation}), which implies that the
expressions in the body of the module are evaluated:

@examples[
#:label #f
#:eval visit-eval
(require 'number-n)
n
]

After a module is instantiated in a particular @tech{namespace},
further @racket[require]s of the module use the same instance, as
opposed to instantiating the module again:

@examples[
#:label #f
#:eval visit-eval
(require 'number-n)
n
(module use-n racket/base
  (require 'number-n)
  (printf "still ~a\n" n))
(require 'use-n)
]

The @racket[dynamic-require] function, like @racket[require], triggers
instantion of a module if it is not already instantiated, so
@racket[dynamic-require] with @racket[#f] as a second argument is
useful to just trigger the instantion effects of a module:

@examples[
#:label #f
#:eval visit-eval
(module use-n-again racket/base
  (require 'number-n)
  (printf "also still ~a\n" n))
(dynamic-require ''use-n-again #f)
]

Instantiation of modules by @racket[require] is transitive. That is,
if @racket[require] of a module instantiates it, then any module
@racket[require]d by that one is also instantiated (if it's not
instantiated already):

@examples[
#:label #f
#:eval visit-eval
(module number-m racket/base
  (provide m)
  (define m (random 10))
  (printf "picked ~a\n" m))
(module use-m racket/base
  (require 'number-m)
  (printf "still ~a\n" m))
(require 'use-m)
]

@; ----------------------------------------
@section[#:tag "compile-time-instantiation"]{Compile-Time Instantiation}

In the same way that declaring a module does not by itself instantiate
a module, declaring a module that @racket[require]s another module
does not by itself instantiate the @racket[require]d module, as
illustrated in the preceding example. However, declaring a module
@emph{does} expand and compile the module. If a module imports another
with @racket[(require (for-syntax ....))], then module that is
imported @racket[for-syntax] must be instantiated during expansion:

@examples[
#:label #f
#:eval visit-eval
#:escape UNSYNTAX
(module number-p racket/base
  (provide p)
  (define p (random 10))
  (printf "picked ~a\n" p))
(module use-p-at-compile-time racket/base
  (require (for-syntax racket/base
                       'number-p))
  (define-syntax (pm stx)
    #`#,p)
  (printf "was ~a at compile time\n" (pm)))
]

Unlike run-time instantiation in a namespace, when a module is used
@racket[for-syntax] for another module expansion in the same
namespace, the @racket[for-syntax]ed module is instantiated separately
for each expansion. Continuing the previous example, if
@racket[number-p] is used a second time @racket[for-syntax], then a
second random number is selected for a new @racket[p]:

@examples[
#:label #f
#:eval visit-eval
#:escape UNSYNTAX
(module use-p-again-at-compile-time racket/base
  (require (for-syntax racket/base
                       'number-p))
  (define-syntax (pm stx)
    #`#,p)
  (printf "was ~a at second compile time\n" (pm)))
]

Separate compile-time instantiations of @racket[number-p] helps
prevent accidental propagation of effects from one module's
compilation to another module's compilation. Preventing those effects
make compilation reliably separate and more deterministic.

The expanded forms of @racket[use-p-at-compile-time] and
@racket[use-p-again-at-compile-time] record the number that was
selected each time, so those two different numbers are printed when the
modules are instantiated:

@examples[
#:label #f
#:eval visit-eval
(dynamic-require ''use-p-at-compile-time #f)
(dynamic-require ''use-p-again-at-compile-time #f)
]

A namespace's top level behaves like a separate module, where multiple
interactions in the top level conceptually extend a single expansion
of the module. So, when using @racket[(require (for-syntax ....))]
twice in the top level, the second use does not trigger a new
compile-time instance:

@examples[
#:label #f
#:eval visit-eval
(begin (require (for-syntax 'number-p)) 'done)
(begin (require (for-syntax 'number-p)) 'done-again)
]

However, a run-time instance of a module is kept separate from all
compile-time instances, including at the top level, so a
non-@racket[for-syntax] use of @racket[number-p] will pick another
random number:

@examples[
#:label #f
#:eval visit-eval
(require 'number-p)
]

@; ----------------------------------------
@section{Visiting Modules}

When a module @racket[provide]s a macro for use by other modules, the
other modules use the macro by directly @racket[require]ing the macro
provider---i.e., without @racket[for-syntax]. That's because the macro
is being imported for use in a run-time position (even though the
macro's implementation lives at compile time), while
@racket[for-syntax] would import a binding for use in compile-time
position.

The module implementing a macro, meanwhile, might @racket[require]
another module @racket[for-syntax] to implement the macro. The
@racket[for-syntax] module needs a compile-time instantiation during
any module expansion that might use the macro. That requirement sets
up a kind of transitivity through @racket[require] that is similar to
instantiation transitivity, but ``off by one'' at the point where the
@racket[for-syntax] shift occurs in the chain.

Here's an example to make that scenario concrete:

@examples[
#:label #f
#:eval visit-eval
#:escape UNSYNTAX
(module number-q racket/base
  (provide q)
  (define q (random 10))
  (printf "picked ~a\n" q))
(module use-q-at-compile-time racket/base
  (require (for-syntax racket/base
                       'number-q))
  (provide qm)
  (define-syntax (qm stx)
    #`#,q)
  (printf "was ~a at compile time\n" (qm)))
(module use-qm racket/base
  (require 'use-q-at-compile-time)
  (printf "was ~a at second compile time\n" (qm)))
(dynamic-require ''use-qm #f)
]

In this example, when @racket[use-q-at-compile-time] is expanded and
compiled, @racket[number-q] is instantiated once. In this case, that
instantion is needed to expand the @racket[(qm)] macro, but the module
system would proactively create a compile-time instantiation of
@racket[number-q] even if the @racket[qm] macro turned out not to be
used.

Then, as @racket[use-qm] is expanded and compiled, a second
compile-time instantiation of @racket[number-q] is created. That
compile-time instantion is needed to expand the @racket[(qm)] form
within @racket[use-qm].

Instantiating @racket[use-qm] correctly reports the number that was
picked during that second module's compilation. First, though, the
@racket[require] of @racket[use-q-at-compile-time] in @racket[use-qm]
triggers a transitive instantiation of @racket[use-q-at-compile-time],
which correctly reports the number that was picked in its compilation.

Overall, the example illustrates a transitive effect of
@racket[require] that we had already seen:

@itemlist[

 @item{When a module is @tech{instantiated}, the run-time expressions
            in its body are evaluated.}

 @item{When a module is @tech{instantiated}, then any module that it @racket[require]s
            (without @racket[for-syntax]) is also @tech{instantiated}.}

]

This rule does not explain the compile-time instantiations of
@racket[number-q], however. To explain that, we need a new word,
@deftech{visit}, for the concept that we saw in
@secref["compile-time-instantiation"]:

@itemlist[

@item{When a module is @tech{visit}ed, the compile-time expressions
           (such as macro definition) in its body are evaluated.}

@item{As a module is expanded, it is @tech{visit}ed.}

@item{When a module is @tech{visit}ed, then any module that it @racket[require]s
            (without @racket[for-syntax]) is also @tech{visit}ed.}

@item{When a module is @tech{visit}ed, then any module that it @racket[require]s
           @racket[for-syntax] is @tech{instantiated} at compile time.}

]

Note that when visiting one module causes a compile-time instantion of
another module, the transitiveness of @tech{instantiated} through
regular @racket[require]s can trigger more compile-time instantiations.
Instantiation itself won't trigger further visits, however, because
any instantiated module has already been expanded and compiled.

The compile-time expressions of a module that are evaluated by
@tech{visit}ing include both the right-hand sides of
@racket[define-syntax] forms and the body of @racket[begin-for-syntax]
forms. That's why a randomly selected number is printed immediately in
the following example:

@examples[
#:label #f
#:eval visit-eval
(module compile-time-number racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (printf "picked ~a\n" (random)))
  (printf "running\n"))
]

Instantiating the module evaluates only the run-time expressions,
which prints ``running'' but not a new random number:

@examples[
#:label #f
#:eval visit-eval
(dynamic-require ''compile-time-number #f)
]

The description of @tech{instantiates} and @tech{visit} above is
phrased in terms of normal @racket[require]s and @racket[for-syntax]
@racket[require]s, but a more precise specification is in terms of
module phases. For example, if module @racket[_A] has @racket[(require
(for-syntax _B))] and module @racket[_B] has @racket[(require
(for-template _C))], then module @racket[_C] is @tech{instantiated}
when module @racket[_A] is instantiated, because the
@racket[for-syntax] and @racket[for-template] shifts cancel. We have
not yet specified what happens with @racket[for-meta 2] for when
@racket[for-syntax]es combine; we leave that to the next section,
@secref["stx-available-module"].

If you think of the top-level as a kind of module that is continuously
expanded, the above rules imply that @racket[require] of another
module at the top level both instantiates and visits the other module
(if it is not already instantiated and visited). That's roughly true,
but the visit is made lazy in a way that is also explained in the next
section, @secref["stx-available-module"].

Meanwhile, @racket[dynamic-require] only instantiates a module; it
does not visit the module. That simplification is why some of the
preceding examples use @racket[dynamic-require] instead of
@racket[require]. The extra visits of a top-level @racket[require]
would make the earlier examples less clear.

@; ----------------------------------------
@section[#:tag "stx-available-module"]{Lazy Visits via Available Modules}

A top-level @racket[require] of a module does not actually
@tech{visit} the module. Instead, it makes the module
@deftech{available}. An @tech{available} module will be @tech{visit}ed
when a future expression needs to be expanded in the same context. The
next expression may or may not involve some imported macro that needs
its compile-time helpers evaluated by @tech{visit}ing, but the module
system proactively @tech{visit}s the module, just in case.

In the following example, a random number is picked as a result of
visiting a module's own body while that module is being expanded. A
@racket[require] of the module instantiates it, printing ``running'',
and also makes the module @tech{available}. Evaluating any other
expression implies expanding the expression, and that expansion
triggers a @tech{visit} of the @tech{available} module---which picks
another random number:

@examples[
#:label #f
#:eval visit-eval
(module another-compile-time-number racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (printf "picked ~a\n" (random)))
  (printf "running\n"))
(require 'another-compile-time-number)
'next
'another
]

@margin-note{Beware that the expander flattens the content of a
top-level @racket[begin] into the top level as soon as the
@racket[begin] is discovered. So, @racket[(begin (require
'another-compile-time-number) 'next)] would still have printed
``picked'' before ``next``.}

The final evaluation of @racket['another] also visits any available
modules, but no modules were made newly available by simply evaluating
@racket['next].

When a module @racket[require]s another module using @racket[for-meta
_n] for some @racket[_n] greater than 1, the @racket[require]d module
is made @tech{available} at phase @racket[_n]. A module that is
@tech{available} at phase @racket[_n] is @tech{visit}ed when some
expression at phase @math{@racket[_n]-1} is expanded.

To help illustrate, the following examples use
@racket[(variable-reference->module-base-phase
(#%variable-reference))], which returns a number for the phase at
which the enclosing module is instantiated:


@examples[
#:label #f
#:eval visit-eval
(module show-phase racket/base
  (printf "running at ~a\n"
          (variable-reference->module-base-phase (#%variable-reference))))
(require 'show-phase)
(module use-at-phase-1 racket/base
  (require (for-syntax 'show-phase)))
(module unused-at-phase-2 racket/base
  (require (for-meta 2 'show-phase)))
]

For the last module above, @racket[show-phase] is made
@tech{available} at phase 2, but no expressions within the module are
ever expanded at phase 1, so there's no phase-2 printout. The
following module includes a phase-1 expression after the phase-2
@racket[require], so there's a printout:

@examples[
#:label #f
#:eval visit-eval
(module use-at-phase-2 racket/base
  (require (for-meta 2 'show-phase)
           (for-syntax racket/base))
  (define-syntax x 'ok))
]

If we @racket[require] the module @racket[use-at-phase-1] at the top
level, then @racket[show-phase] is made @tech{available} at phase 1.
Evaluating another expression causes @racket[use-at-phase-1] to be
@tech{visit}ed, which in turn instantitates @racket[show-phase]:

@examples[
#:label #f
#:eval visit-eval
(require 'use-at-phase-1)
'next
]

A @racket[require] of @racket[use-at-phase-2] is similar, except that
@racket[show-phase] is made @tech{available} at phase 2, so it is not
instantiated until some expression is expanded at phase 1:

@examples[
#:label #f
#:eval visit-eval
(require 'use-at-phase-2)
'next
(require (for-syntax racket/base))
(begin-for-syntax 'compile-time-next)
]

@; ----------------------------------------------------------------------

@close-eval[visit-eval]
