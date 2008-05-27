#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@(define cake-eval (make-base-eval))

@title{Module Syntax}

The @litchar{#lang} at the start of a module file begins a shorthand
for a @scheme[module] form, much like @litchar{'} is a shorthand for a
@scheme[quote] form. Unlike @litchar{'}, the @litchar{#lang}
shorthand does not work well in a @tech{REPL}, in part because it must be
terminated by an end-of-file, but also because the longhand expansion
of @litchar{#lang} depends on the name of the enclosing file.

@;------------------------------------------------------------------------
@section[#:tag "module-syntax"]{The @scheme[module] Form}

The longhand form of a module declaration, which works in a
@tech{REPL} as well as a file, is

@specform[
(module name-id initial-module-path
  decl ...)
]

where the @scheme[_name-id] is a name for the module,
@scheme[_initial-module-path] is an initial import, and each
@scheme[_decl] is an import, export, definition, or expression.  In
the case of a file, @scheme[_name-id] must match the name of the
containing file, minus its directory path or file extension.

The @scheme[_initial-module-path] is needed because even the
@scheme[require] form must be imported for further use in the module
body. In other words, the @scheme[_initial-module-path] import
bootstraps the syntax available in the body. The most commonly used
@scheme[_initial-module-path] is @scheme[scheme], which supplies most
of the bindings described in this guide, including @scheme[require],
@scheme[define], and @scheme[provide]. Another commonly used
@scheme[_initial-module-path] is @scheme[scheme/base], which provides
less functionality, but still much of the most commonly needed
functions and syntax.

For example, the @filepath{cake.ss} example of the
@seclink["module-basics"]{previous section} could be written as

@schemeblock+eval[
#:eval cake-eval
(module cake scheme
  (provide print-cake)

  (define (print-cake n)
    (printf "   ~a  \n" (make-string n #\.))
    (printf " .-~a-.\n" (make-string n #\|))
    (printf " | ~a |\n" (make-string n #\space))
    (printf "---~a---\n" (make-string n #\-))))
]

Furthermore, this @scheme[module] form can be evaluated in a
@tech{REPL} to declare a @scheme[cake] module that is not associated
with any file. To refer to such an unassociated module, quote the
module name:

@examples[
#:eval cake-eval
(require 'cake)
(eval:alts (print-cake 3) (eval '(print-cake 3)))
]

Declaring a module does not immediately evaluate the body definitions
and expressions of the module. The module must be explicitly
@scheme[require]d at the top level to trigger evaluation. After
evaluation is triggered once, later @scheme[require]s do not
re-evaluate the module body.

@examples[
(module hi scheme
  (printf "Hello\n"))
(require 'hi)
(require 'hi)
]

@;------------------------------------------------------------------------
@section[#:tag "hash-lang"]{The @schememodfont{#lang} Shorthand}

The body of a @schememodfont{#lang} shorthand has no specific syntax,
because the syntax is determined by the language name that follows
@schememodfont{#lang}.

In the case of @schememodfont{#lang} @schememodname[scheme], the syntax
is

@schememod[
scheme
_decl ...]

which reads the same as

@schemeblock[
(module _name scheme
  _decl ...)
]

where @scheme[_name] is derived from the name of the file that
contains the @schememodfont{#lang} form.

The @schememodfont{#lang} @scheme[scheme/base] form has the same
syntax as @schememodfont{#lang} @schememodname[scheme], except that
the longhand expansion uses @scheme[scheme/base] instead of
@scheme[scheme]. The @schememodfont{#lang} @scheme[honu] form, in
contrast, has a completely different syntax that doesn't even look
like Scheme, and which we do not attempt to describe in this guide.

Unless otherwise specified, a module that is documented as a
``language'' using the @schememodfont{#lang} notation will expand to
@scheme[module] in the same way as @schememodfont{#lang}
@schememodname[scheme]. The documented language name can be used
directly with @scheme[module] or @scheme[require], too.

@; ----------------------------------------------------------------------

@close-eval[cake-eval]
