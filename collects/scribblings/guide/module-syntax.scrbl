#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Module Syntax}

The @litchar{#module} at the start of a module file begins a shorthand
for a @scheme[module] form, much like @litchar{'} is a shorthand for a
@scheme[quote] form. Unlike @litchar{'}, the @litchar{#module}
shorthand does not work well in a @tech{REPL}, in part because it must be
terminated by an end-of-file, but also because the longhand expansion
of @litchar{#module} depends on the name of the enclosing file.

@;------------------------------------------------------------------------
@section{The @scheme[module] Form}

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
@scheme[_initial-module-path] is @scheme[(lib "big/lang.ss")], which
supplies most of the bindings described in this guide, including
@scheme[require], @scheme[define], and @scheme[provide].

For example, the @file{cake.ss} example of the
@seclink["module-basics"]{previous section} could be written as

@schemeblock[
(module cake (lib "big/lang.ss")
  (provide print-cake)

  (define (print-cake n)
    (printf "   ~a  \n" (make-string n #\.))
    (printf " .-~a-.\n" (make-string n #\|))
    (printf " | ~a |\n" (make-string n #\space))
    (printf "---~a---\n" (make-string n #\-))))
]

Furthermore, this @scheme[module] form can be evaluated in a
@tech{REPL} to declare a @scheme[cake] module that is not associated
with any file.

Declaring a module does not immediately evaluate the body definitions
and expressions of the module. The most must be explicitly
@scheme[require]d at the top level to trigger evaluation. After
evaluation is triggered once, later @scheme[require]s have no effect
(other than binding).

@examples[
(module hi (lib "big/lang.ss")
  (printf "Hello\n"))
(require hi)
(require hi)
]

@;------------------------------------------------------------------------
@section{The @schememodfont{#module} Shorthand}

Unlike @litchar{'}, there is no fixed syntax for the body of a
@litchar{#module} shorthand. In general, the syntax is determined by
the language name that follows @litchar{#module}.

In the case of @schememodfont{#module} @schememodname[big], the syntax
is

@schememod[
big
_decl ...]

which reads the same as

@schemeblock[
(module _name (lib "big/lang.ss")
  _decl ...)
]

where @scheme[_name] is derived from the name of the file that
contains the @schememodfont{#module} form.

The @schememodfont{#module} @scheme[little] form has the same syntax
as @schememodfont{#module} @schememodname[big], except that the
longhand expansion uses @scheme[(lib "little/lang.ss")] instead of
@scheme[(lib "big/lang.ss")]. The @schememodfont{#module}
@scheme[honu] form, in contrast, has a completely different syntax
that doesn't even look like Scheme, and which we do not attempt to
describe in this guide.
