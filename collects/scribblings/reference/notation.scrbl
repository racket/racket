#lang scribble/doc
@(require scribble/struct scribble/racket "mz.rkt")

@title[#:tag "notation"]{Notation}

This chapter introduces essential terminology and notation that is
used throughout the rest of the document and other Racket reference
manuals.


@section{Notation for Module Declarations}

Racket programs are usually organized into @tech{module}s. Documentation
reflects this organization with a notation for module declarations.
A module declaration often prefaces the beginning of a section
or subsection:

@; TODO: change `defmodule` to support this
@(make-table "defmodule"
  (list
   (list
    (make-flow
     (list
      (make-omitable-paragraph
       (list (hspace 1) (racket (require racket/list)))))))))

The preceding @racket[require] statement in a section indicates that the bindings that are
documented in the section are available from the @racketmodname[racket/list] module.

Instead of @racket[require], some module declarations are written with
@hash-lang[]:

@; TODO: change `defmodule` to support this
@(make-table "defmodule"
  (list
   (list
    (make-flow
     (list
      (make-omitable-paragraph
       (list (hspace 1) @hash-lang[]  (hspace 1) (racket racket/base))))))))

Using @hash-lang[] means that the module is normally used as a
language, instead of imported with @racket[require]. Unless otherwise
specified, however, a module name documented with @hash-lang[] can
also be used with @racket[require] to obtain the same bindings.

Sometimes, a module specification appears at the beginning of a
document or at the start of a section that contains many subsections.
The document's section or section's subsections are meant to
``inherit'' the module declaration of the enclosing document or
section. Thus, bindings documented in @other-doc['(lib
"scribblings/reference/reference.scrbl")] are available from
@racketmodname[racket] and @racket[base/base] unless otherwise
specified in a section or subsection.


@section{Notation for Syntactic Forms}

Syntactic forms, whether provided from the base language or via
syntactic extensions, are specified using the same format that is
described in the @guidesecref{syntax-notation} chapter of @|Guide|.

Note that @racket[_number] in a grammar specification means that a
literal number must appear in the syntactic form, while
@racket[_number-expr] would allow any expression that produces a
number. Similarly, @racket[_module-path] in a grammar corresponds to
the non-terminal described for @racket[require], while
@racket[_module-path-expr] would allow an arbitrary expression that
produces a value for which @racket[module-path?] returns true.


@section{Notation for Contracts for Functions}

Procedures and other values are described using a notation based on
@tech{contract}s. In essence, these contracts describe the interfaces of
the documented library using Racket predicates and expressions.

For example, the following is the header of the definition of a
typical procedure:

@defproc[#:link-target? #f
         (char->integer [char char?]) exact-integer?]{}

The function being defined, @racket[char->integer], is typeset as if it
were being applied. The metavariables that come after the function name
stand in for arguments. The white text in the corner identifies the
kind of value that is being documented.

Each metavariable is described with a contract. In the preceding
example, the metavariable @racket[_char] has the contract
@racket[char?]. This contract specifies that any argument
@racket[_char] that answers true to the @racket[char?] predicate is
valid. The documented function may or may not actually check this
property, but the contract signals the intent of the implementer.

The contract on the right of the arrow, @racket[exact-integer?] in this case,
specifies the expected result that is produced by the function.

Contract specifications can be more expressive than just names of
predicates. Consider the following header for @racket[argmax]:

@defproc[#:link-target? #f
         (argmax [proc (-> any/c real?)]
                 [lst (and/c pair? list?)])
         any]{}

The contract @racket[(-> any/c real?)] denotes a function contract specifying
that @racket[proc]'s argument can be any single value and the result should be
a real number. The contract @racket[(and/c pair? list?)] for @racket[_lst]
specifies that @racket[_lst] should pass both @racket[pair?] and @racket[list?]
(i.e., that it is a non-empty list).

Both @racket[->] and @racket[and/c] are examples of @tech{contract combinator}s.
Contract combinators such as @racket[or/c], @racket[cons/c], @racket[listof],
and others are used throughout the documentation. Clicking on the hyperlinked
combinator name will provide more information on its meaning.

A Racket function may be documented as having one or more optional arguments.
The @racket[read] function is an example of such a function:

@defproc[#:link-target? #f
         (read [in input-port? (current-input-port)])
         any]{}

The brackets surrounding the @racket[_in] argument in the application
syntax indicates that it is an optional argument.

The header for @racket[read] specifies a contract for the parameter
@racket[_in] as usual. To the right of the contract, it also specifies
a default value @racket[(current-input-port)] that is used if
@racket[read] is called with no arguments.

Functions may also be documented as accepting mandatory or optional
keyword-based arguments.  For example, the @racket[sort] function has
two optional, keyword-based arguments:

@defproc[#:link-target? #f
         (sort [lst list?] [less-than? (any/c any/c . -> . any/c)]
               [#:key extract-key (any/c . -> . any/c) (lambda (x) x)]
               [#:cache-keys? cache-keys? boolean? #f]) list?]{}

The brackets around the @racket[_extract-key] and
@racket[_cache-keys?]  arguments indicate that they are optional as
before. The contract section of the header shows the default values
that are provided for these keyword arguments.

@section{Notation for Parameter Contracts}

Parameters are used in Racket for dynamically customizable arguments
to code. They are documented with a notation similar to function
contracts:

@defparam[#:link-target? #f
          current-command-line-arguments
          argv
          (vectorof (and/c string? immutable?))]{}

Since parameters can be read or written, there are two entries in the
header above. Calling @racket[current-command-line-arguments] with no
arguments is documented to return a vector that contains objects that
pass both @racket[string?] and @racket[immutable?]. Similarly, the
single argument case takes a vector with the same specification and
returns an object satisfying @racket[void?].

@section{Notation for Contracts on Other Values}

Some libraries provide bindings to constant values. These values are
documented with a separate header:

@defthing[#:link-target? #f object% class?]{}

The @racketmodname[racket/class] library provides the @racket[object%]
value, which is the root of the class hierarchy in Racket. Its
documentation header just indicates that it is a value that satisfies
the predicate @racket[class?].

