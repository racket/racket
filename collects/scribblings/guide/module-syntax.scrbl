#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define cake-eval (make-base-eval))

@title{Module Syntax}

The @litchar{#lang} at the start of a module file begins a shorthand
for a @racket[module] form, much like @litchar{'} is a shorthand for a
@racket[quote] form. Unlike @litchar{'}, the @litchar{#lang}
shorthand does not work well in a @tech{REPL}, in part because it must be
terminated by an end-of-file, but also because the longhand expansion
of @litchar{#lang} depends on the name of the enclosing file.

@;------------------------------------------------------------------------
@section[#:tag "module-syntax"]{The @racket[module] Form}

The longhand form of a module declaration, which works in a
@tech{REPL} as well as a file, is

@specform[
(module name-id initial-module-path
  decl ...)
]

where the @racket[_name-id] is a name for the module,
@racket[_initial-module-path] is an initial import, and each
@racket[_decl] is an import, export, definition, or expression.  In
the case of a file, @racket[_name-id] normally matches the name of the
containing file, minus its directory path or file extension, but
@racket[_name-id] is ignored when the module is @racket[require]d
through its file's path.

The @racket[_initial-module-path] is needed because even the
@racket[require] form must be imported for further use in the module
body. In other words, the @racket[_initial-module-path] import
bootstraps the syntax that is available in the body. The most commonly used
@racket[_initial-module-path] is @racketmodname[racket], which supplies most
of the bindings described in this guide, including @racket[require],
@racket[define], and @racket[provide]. Another commonly used
@racket[_initial-module-path] is @racketmodname[racket/base], which provides
less functionality, but still much of the most commonly needed
functions and syntax.

For example, the @filepath{cake.rkt} example of the
@seclink["module-basics"]{previous section} could be written as

@racketblock+eval[
#:eval cake-eval
(module cake racket
  (provide print-cake)

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline)))
]

Furthermore, this @racket[module] form can be evaluated in a
@tech{REPL} to declare a @racket[cake] module that is not associated
with any file. To refer to such an unassociated module, quote the
module name:

@examples[
#:eval cake-eval
(require 'cake)
(eval:alts (print-cake 3) (eval '(print-cake 3)))
]

Declaring a module does not immediately evaluate the body definitions
and expressions of the module. The module must be explicitly
@racket[require]d at the top level to trigger evaluation. After
evaluation is triggered once, later @racket[require]s do not
re-evaluate the module body.

@examples[
(module hi racket
  (printf "Hello\n"))
(require 'hi)
(require 'hi)
]

@;------------------------------------------------------------------------
@section[#:tag "hash-lang"]{The @racketmodfont{#lang} Shorthand}

The body of a @racketmodfont{#lang} shorthand has no specific syntax,
because the syntax is determined by the language name that follows
@racketmodfont{#lang}.

In the case of @racketmodfont{#lang} @racketmodname[racket], the syntax
is

@racketmod[
racket
_decl ...]

which reads the same as

@racketblock[
(module _name racket
  _decl ...)
]

where @racket[_name] is derived from the name of the file that
contains the @racketmodfont{#lang} form.

The @racketmodfont{#lang} @racketmodname[racket/base] form has the same
syntax as @racketmodfont{#lang} @racketmodname[racket], except that
the longhand expansion uses @racketmodname[racket/base] instead of
@racketmodname[racket]. The @racketmodfont{#lang} @racket[honu] form, in
contrast, has a completely different syntax that doesn't even look
like Racket, and which we do not attempt to describe in this guide.

Unless otherwise specified, a module that is documented as a
``language'' using the @racketmodfont{#lang} notation will expand to
@racket[module] in the same way as @racketmodfont{#lang}
@racketmodname[racket]. The documented language name can be used
directly with @racket[module] or @racket[require], too.

@; ----------------------------------------------------------------------
@section[#:tag "submodules"]{Submodules}

A @racket[module] form can be nested within a module, in which case
the nested @racket[module] form declares a
@deftech{submodule}. Submodules can be referenced directly by the
enclosing module using a quoted name. The following example prints
@racket["Tony"] by importing @racket[tiger] from the @racket[zoo]
submodule:

@racketmod[
  #:file "park.rkt"
  racket

  (module zoo racket
    (provide tiger)
    (define tiger "Tony"))

  (require 'zoo)

  tiger
]

Running a module does not necessarily run its submodules. In the above
example, running @filepath{park.rkt} runs its submodule @racket[zoo]
only because the @filepath{park.rkt} module @racket[require]s the
@racket[zoo] submodule. Otherwise, a module and each of its submodules can be run
independently. Furthermore, if @filepath{park.rkt} is compiled to a
bytecode file (via @exec{raco make}), then the code for
@filepath{park.rkt} or the code for @racket[zoo] can be loaded independently.

A @racket[module*] form is similar to a nested @racket[module] form,
but @racket[module*] inverts the possibilities for reference between
the submodule and enclosing module:

@itemlist[

 @item{A submodule declared with @racket[module] can be
       @racket[require]d by its enclosing module, but the submodule
       cannot @racket[require] the enclosing module or lexically
       reference the enclosing module's bindings.}

 @item{A submodule declared with @racket[module*] can @racket[require]
       its enclosing module, but the enclosing module cannot
       @racket[require] the submodule. In addition, a @racket[module*]
       form can specify @racket[#f] as its
       @racket[_initial-module-path], in which case the submodule sees
       all of the enclosing module's bindings---including bindings
       that are not exported via @racket[provide].}

]

As an example of @racket[module*], the following variant of
@filepath{cake.rkt} includes a @racket[main] submodule that calls
@racket[print-cake]:

@racketmod[
#:file "cake.rkt"
racket

(provide print-cake)

(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))

(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))

(module* main #f
  (print-cake 10))
]

Running a module does not run its @racket[module*]-defined submodules,
since the enclosing module cannot directly reference
@racket[module*]-defined submodules. Nevertheless, running the above
module via @exec{racket} or DrRacket prints a cake with 10 candles,
because the @racket[main] submodule} is a special case.

When a module is provided as a program name to the @exec{racket}
executable or run directly within DrRacket, if the module has a
@as-index{@racket[main] submodule}, the @racket[main] submodule is run after its
enclosing module. Declaring a @racket[main] submodule is often a
useful describe tests or other extra actions to be performed when a
module is run directly instead of @racket[required] as a library
within a larger program.

A @racket[main] submodule does not have to be declared with
@racket[module*]. If the @racket[main] module does not need to use
bindings from its enclosing module, it can be declared with
@racket[module]. A @racket[main] submodule typically uses the
bindings of its enclosing module, however, so @racket[main] is usually
declared with @racket[module*].

Submodules can be nested within submodules, and a submodule can be
referenced directly by a module other than its enclosing module by
using a @racket[submod] path as described in the
@seclink["module-paths"]{next section}.

@; ----------------------------------------------------------------------

@close-eval[cake-eval]
