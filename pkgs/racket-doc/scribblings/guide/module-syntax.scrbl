#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label rackunit))

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
@racketmodname[racket]. The @racketmodfont{#lang} @racketmodname[scribble/manual] form, in
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

Submodules can be nested within submodules, and a submodule can be
referenced directly by a module other than its enclosing module by
using a @elemref["submod"]{submodule path}.

A @racket[module*] form is similar to a nested @racket[module] form:

@specform[
(module* name-id initial-module-path-or-#f
  decl ...)
]

The @racket[module*] form differs from @racket[module] in that it
inverts the possibilities for reference between the submodule and
enclosing module:

@itemlist[

 @item{A submodule declared with @racket[module] can be
       @racket[require]d by its enclosing module, but the submodule
       cannot @racket[require] the enclosing module or lexically
       reference the enclosing module's bindings.}

 @item{A submodule declared with @racket[module*] can @racket[require]
       its enclosing module, but the enclosing module cannot
       @racket[require] the submodule.}

]

In addition, a @racket[module*] form can specify @racket[#f] in place of an
@racket[_initial-module-path], in which case the submodule sees all of
the enclosing module's bindings---including bindings that are not
exported via @racket[provide].

One use of submodules declared with @racket[module*] and @racket[#f] is
to export additional bindings through a submodule that are not
normally exported from the module:

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

(module* extras #f
  (provide show))
]

In this revised @filepath{cake.rkt} module, @racket[show] is not
imported by a module that uses @racket[(require "cake.rkt")], since
most clients of @filepath{cake.rkt} will not want the extra function.  A
module can require the @racket[extra] @tech{submodule} using
@racket[(require (submod "cake.rkt" extras))] to access the otherwise
hidden @racket[show] function.@margin-note*{See @elemref["submod"]{submodule paths}
for more information on @racket[submod].}

@; ----------------------------------------------------------------------
@section[#:tag "main-and-test"]{Main and Test Submodules}

The following variant of @filepath{cake.rkt} includes a @racket[main]
submodule that calls @racket[print-cake]:

@racketmod[
#:file "cake.rkt"
racket

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

Running a module does not run its @racket[module*]-defined
submodules. Nevertheless, running the above module via @exec{racket}
or DrRacket prints a cake with 10 candles, because the @racket[main]
@tech{submodule} is a special case.

When a module is provided as a program name to the @exec{racket}
executable or run directly within DrRacket, if the module has a
@as-index{@racket[main] submodule}, the @racket[main] submodule is run
after its enclosing module. Declaring a @racket[main] submodule
thus specifies extra actions to be performed when a module is run directly,
instead of @racket[require]d as a library within a larger program.

A @racket[main] submodule does not have to be declared with
@racket[module*]. If the @racket[main] module does not need to use
bindings from its enclosing module, it can be declared with
@racket[module]. More commonly, @racket[main] is declared using
@racket[module+]:

@specform[
(module+ name-id
  decl ...)
]

A submodule declared with @racket[module+] is like one declared with
@racket[module*] using @racket[#f] as its
@racket[_initial-module-path].  In addition,
multiple @racket[module+] forms can specify the same submodule name,
in which case the bodies of the @racket[module+] forms are combined to
create a single submodule.

The combining behavior of @racket[module+] is particularly useful for
defining a @racket[test] submodule, which can be conveniently run
using @exec{raco test} in much the same way that @racket[main] is
conveniently run with @exec{racket}. For example, the following
@filepath{physics.rkt} module exports @racket[drop] and
@racket[to-energy] functions, and it defines a @racket[test] module to
hold unit tests:

@racketmod[
#:file "physics.rkt"
racket
(module+ test
  (require rackunit)
  (define ε 1e-10))

(provide drop
         to-energy)

(define (drop t)
  (* 1/2 9.8 t t))

(module+ test
  (check-= (drop 0) 0 ε)
  (check-= (drop 10) 490 ε))

(define (to-energy m)
  (* m (expt 299792458.0 2)))

(module+ test
  (check-= (to-energy 0) 0 ε)
  (check-= (to-energy 1) 9e+16 1e+15))
]

Importing @filepath{physics.rkt} into a larger program does not run
the @racket[drop] and @racket[to-energy] tests---or even trigger the
loading of the test code, if the module is compiled---but running
@exec{raco test physics.rkt} at a command line runs the tests.

The above @filepath{physics.rkt} module is equivalent to using
@racket[module*]:

@racketmod[
#:file "physics.rkt"
racket

(provide drop
         to-energy)

(define (drop t)
  (* 1/2 #e9.8 t t))

(define (to-energy m)
  (* m (expt 299792458 2)))

(module* test #f
  (require rackunit)
  (define ε 1e-10)
  (check-= (drop 0) 0 ε)
  (check-= (drop 10) 490 ε)
  (check-= (to-energy 0) 0 ε)
  (check-= (to-energy 1) 9e+16 1e+15))
]

Using @racket[module+] instead of @racket[module*] allows tests to be
interleaved with function definitions.

The combining behavior of @racket[module+] is also sometimes helpful
for a @racket[main] module. Even when combining is not needed,
@racket[(module+ main ....)] is preferred as it is more readable than
@racket[(module* main #f ....)].

@; ----------------------------------------------------------------------

@close-eval[cake-eval]
