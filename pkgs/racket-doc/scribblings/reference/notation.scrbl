#lang scribble/doc
@(require scribble/struct scribble/racket "mz.rkt")

@title[#:tag "notation"]{Notation for Documentation}

This chapter introduces essential terminology and notation that is
used throughout Racket documentation.

@; ----------------------------------------
@section{Notation for Module Documentation}

Since Racket programs are organized into @tech{module}s, documentation
reflects that organization with an annotation at the beginning of a
section or subsection that describes the bindings that a particular
module provides.

For example, the section that describes the functionality provided by
@racketmodname[racket/list] starts

@nested[#:style 'inset
        (defmodule racket/list #:no-declare #:link-target? #f)]

Instead of @racket[require], some modules are introduced with
@hash-lang[]:

@nested[#:style 'inset
        (defmodule racket/base #:lang #:no-declare #:link-target? #f)]

Using @hash-lang[] means that the module is normally used as the
language of a whole module---that is, by a module that starts
@hash-lang[] followed by the language---instead of imported with
@racket[require]. Unless otherwise specified, however, a module name
documented with @hash-lang[] can also be used with @racket[require] to
obtain the language's bindings.

The module annotation also shows the
@tech[#:doc '(lib "pkg/scribblings/pkg.scrbl")]{package}
that the module belongs to on the right-hand side. For more details
about packages, see
@(other-manual '(lib "pkg/scribblings/pkg.scrbl")).

Sometimes, a module specification appears at the beginning of a
document or at the start of a section that contains many subsections.
The document's section or section's subsections are meant to
``inherit'' the module declaration of the enclosing document or
section. Thus, bindings documented in @other-doc['(lib
"scribblings/reference/reference.scrbl")] are available from
@racketmodname[racket] and @racket[racket/base] unless otherwise
specified in a section or subsection.

@; ----------------------------------------
@section{Notation for Syntactic Form Documentation}

@guideintro["syntax-notation"]{this notation for syntactic forms}

Syntactic forms are specified with a grammar. Typically, the grammar
starts with an open parenthesis followed by the syntactic form's name,
as in the grammar for @racket[if]:

@nested[#:style 'inset
@defform[#:link-target? #f
         (if test-expr then-expr else-expr)]
]

Since every @deftech{form} is expressed in terms of @tech{syntax
objects}, parentheses in a grammar specification indicate a @tech{syntax
object} wrapping a list, and the leading @racket[if] is an identifier
that starts the list whose @tech{binding} is the @racket[if] binding
of the module being documented---in this case,
@racketmodname[racket/base].  Square brackets in the grammar indicate
a @tech{syntax-object} list in the same way as parentheses, but in
places square brackets are normally used by convention in a program's
source.

Italic @tech{identifiers} in the grammar are @deftech{metavariables}
that correspond to other grammar productions. Certain metavariable
names have implicit grammar productions:

@itemize[

 @item{A metavariable that ends in @racket[_id] stands for an
       @tech{identifier}.}

 @item{A metavariable that ends in @racket[_keyword] stands
       for a @tech{syntax-object} @tech{keyword}.}

 @item{A metavariable that ends with @racket[_expr] stands for any
       form, and the form will be parsed as an expression.}

 @item{A metavariable that ends with @racket[_body] stands for any
       @tech{form}; the form will be parsed as either a local definition or
       an expression. A @racket[_body] can parse as a definition only
       if it is not preceded by any expression, and the last
       @racket[_body] must be an expression; see also
       @secref["intdef-body"].}

 @item{A metavariable that ends with @racket[_datum] stands for any
       @tech{form}, and the form is normally uninterpreted (e.g.,
       @racket[quote]d).}

 @item{A metavariable that ends with @racket[_number] or
       @racket[_boolean] stands for any @tech{syntax-object} (i.e.,
       literal) @tech{number} or @tech{boolean}, respectively.}

]

In a grammar, @racket[_form ...] stands for any number of forms
(possibly zero) matching @racket[_form], while @racket[_form ...+]
stands for one or more forms matching @racket[_form].

Metavariables without an implicit grammar are defined by productions
alongside the syntactic form's overall grammar. For example, in

@nested[#:style 'inset
@defform[#:link-target? #f
         (lambda formals body ...+)
         #:grammar ([formals id
                             (id ...)
                             (id ...+ . rest-id)])]
]

the @racket[_formals] metavariable stands for either an
@tech{identifier}, zero or more @tech{identifiers} in a
@tech{syntax-object} list, or a @tech{syntax object} corresponding to
a chain of one or more pairs where the chain ends in an
@tech{identifier} instead of an empty list.

Some syntactic forms have multiple top-level grammars, in which case
the documentation of the syntactic forms shows multiple grammars. For
example,

@nested[#:style 'inset
@defform*[#:link-target? #f
          ((init-rest id)
           (init-rest))]
]

indicates that @racket[init-rest] can either be alone in its
@tech{syntax-object} list or followed by a single @tech{identifier}.

Finally, a grammar specification that includes @racket[_expr]
metavariables may be augmented with run-time @tech{contract}s on some
of the metavariables, which indicate a predicate that the result of
the expression must satisfy at run time. For example,

@nested[#:style 'inset
@defform[#:link-target? #f
         (parameterize ([parameter-expr value-expr] ...)
           body ...+)
         #:contracts
         ([parameter-expr parameter?])]
]

indicates that the result of each @racket[_parameter-expr] must be a
value @racket[_v] for which @racket[(parameter? _v)] returns true.


@; ----------------------------------------
@section{Notation for Function Documentation}

Procedures and other values are described using a notation based on
@tech{contract}s. In essence, these contracts describe the interfaces of
the documented library using Racket predicates and expressions.

For example, the following is the header of the definition of a
typical procedure:

@nested[#:style 'inset
@defproc[#:link-target? #f
         (char->integer [char char?]) exact-integer?]
]

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

@nested[#:style 'inset
@defproc[#:link-target? #f
         (argmax [proc (-> any/c real?)]
                 [lst (and/c pair? list?)])
         any]
]

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

@nested[#:style 'inset
@defproc[#:link-target? #f
         (read [in input-port? (current-input-port)])
         any]
]

The brackets surrounding the @racket[_in] argument in the application
syntax indicates that it is an optional argument.

The header for @racket[read] specifies a contract for the parameter
@racket[_in] as usual. To the right of the contract, it also specifies
a default value @racket[(current-input-port)] that is used if
@racket[read] is called with no arguments.

Functions may also be documented as accepting mandatory or optional
keyword-based arguments.  For example, the @racket[sort] function has
two optional, keyword-based arguments:

@nested[#:style 'inset
@defproc[#:link-target? #f
         (sort [lst list?] [less-than? (any/c any/c . -> . any/c)]
               [#:key extract-key (any/c . -> . any/c) (lambda (x) x)]
               [#:cache-keys? cache-keys? boolean? #f]) list?]
]

The brackets around the @racket[_extract-key] and
@racket[_cache-keys?]  arguments indicate that they are optional as
before. The contract section of the header shows the default values
that are provided for these keyword arguments.

@; ----------------------------------------
@section{Notation for Structure Type Documentation}

A @tech{structure type} is also documented using contract notation:

@nested[#:style 'inset
@defstruct*[#:link-target? #f
            color ([red (and/c natural-number/c (<=/c 255))]
                   [green (and/c natural-number/c (<=/c 255))]
                   [blue (and/c natural-number/c (<=/c 255))]
                   [alpha (and/c natural-number/c (<=/c 255))])]
]

The structure type is typeset as it were declared in
the source code of a program using the @racket[struct] form.
Each field of the structure is documented with a corresponding
contract that specifies the values that are accepted for that field.

In the example above, the structure type @racket[_color] has
four fields: @racket[_red], @racket[_green], @racket[_blue],
and @racket[_alpha]. The constructor for the structure type
accepts field values that satisfy
@racket[(and/c natural-number/c (<=/c 255))], i.e., non-negative
exact integers up to 255.

Additional keywords may appear after the field names in the
documentation for a structure type:

@nested[#:style 'inset
@defstruct*[#:link-target? #f
            data-source
              ([connector (or/c 'postgresql 'mysql 'sqlite3 'odbc)]
               [args list?]
               [extensions (listof (list/c symbol? any/c))])
            #:mutable]
]

Here, the @racket[#:mutable] keyword indicates that the fields of
instances of the @racket[_data-source] structure type can be
mutated with their respective setter functions.

@; ----------------------------------------
@section{Notation for Parameter Documentation}

A @tech{parameter} is documented the same way as a function:

@nested[#:style 'inset
@defparam*[#:link-target? #f
           current-command-line-arguments
           argv
           (vectorof (and/c string? immutable?))
           (vectorof string?)]
]

Since @tech{parameters} can be referenced or set, there are two entries in the
header above. Calling @racket[current-command-line-arguments] with no
arguments accesses the parameter's value, which must be a vector whose elements
pass both @racket[string?] and @racket[immutable?]. Calling
@racket[current-command-line-arguments] with a single argument
sets the parameter's value, where the value must be a vector whose
elements pass @racket[string?] (and a guard on the @tech{parameter}
coerces the strings to immutable form, if necessary).

@; ----------------------------------------
@section{Notation for Other Documentation}

Some libraries provide bindings to constant values. These values are
documented with a separate header:

@nested[#:style 'inset
@defthing[#:link-target? #f object% class?]
]

The @racketmodname[racket/class] library provides the @racket[object%]
value, which is the root of the class hierarchy in Racket. Its
documentation header just indicates that it is a value that satisfies
the predicate @racket[class?].

