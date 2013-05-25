#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label syntax/kerncase))

@title{Library Syntax Classes and Literal Sets}

@section{Syntax Classes}

@(begin
   (define-syntax-rule (defstxclass name . pre-flows)
     (defidform name . pre-flows))
   (define-syntax-rule (defstxclass* (name arg ...) . pre-flows)
     (defform (name arg ...) . pre-flows)))

@defstxclass[expr]{

Matches anything except a keyword literal (to distinguish expressions
from the start of a keyword argument sequence). The term is not
otherwise inspected, since it is not feasible to check if it is
actually a valid expression.
}

@deftogether[(
@defstxclass[identifier]
@defstxclass[boolean]
@defstxclass[str]
@defstxclass[char]
@defstxclass[keyword]
@defstxclass[number]
@defstxclass[integer]
@defstxclass[exact-integer]
@defstxclass[exact-nonnegative-integer]
@defstxclass[exact-positive-integer])]{

Match syntax satisfying the corresponding predicates.
}

@defstxclass[id]{ Alias for @racket[identifier]. }
@defstxclass[nat]{ Alias for @racket[exact-nonnegative-integer]. }

@defproc[(static [predicate (-> any/c any/c)]
                 [description (or/c string? #f)])
         (attributes value)]{

The @racket[static] syntax class matches an
identifier that is bound in the syntactic environment to static
information (see @racket[syntax-local-value]) satisfying the given
@racket[predicate]. If the term does not match, the
@racket[description] argument is used to describe the expected syntax.

When used outside of the dynamic extent of a macro transformer (see
@racket[syntax-transforming?]), matching fails.

The attribute @var[value] contains the value the name is bound to.
}

@defproc[(expr/c [contract-expr syntax?]
                 [#:positive pos-blame
                  (or/c syntax? string? module-path-index? 'from-macro 'use-site 'unknown)
                  'use-site]
                 [#:negative neg-blame
                  (or/c syntax? string? module-path-index? 'from-macro 'use-site 'unknown)
                  'from-macro]
                 [#:name expr-name (or/c identifier? string? symbol?) #f]
                 [#:macro macro-name (or/c identifier? string? symbol?) #f]
                 [#:context ctx (or/c syntax? #f) #, @elem{determined automatically}])
         (attributes c)]{

Accepts an expression (@racket[expr]) and computes an attribute
@racket[c] that represents the expression wrapped with the contract
represented by @racket[contract-expr].

The contract's positive blame represents the obligations of the
expression being wrapped. The negative blame represents the
obligations of the macro imposing the contract---the ultimate user
of @racket[expr/c]. By default, the positive blame is taken as
the module currently being expanded, and the negative blame is
inferred from the definition site of the macro (itself inferred from
the @racket[context] argument), but both blame locations can be
overridden.

The @racket[pos-blame] and @racket[neg-blame] arguments are turned
into blame locations as follows:
@itemize[
@item{If the argument is a string, it is used directly as the blame
  label.}
@item{If the argument is syntax, its source location is used
  to produce the blame label.}
@item{If the argument is a module path index, its resolved module path
  is used.}
@item{If the argument is @racket['from-macro], the macro is inferred
  from either the @racket[macro-name] argument (if @racket[macro-name]
  is an identifier) or the @racket[context] argument, and the module
  where it is @emph{defined} is used as the blame location. If
  neither an identifier @racket[macro-name] nor a @racket[context]
  argument is given, the location is @racket["unknown"].}
@item{If the argument is @racket['use-site], the module being
  expanded is used.}
@item{If the argument is @racket['unknown], the blame label is
  @racket["unknown"].}
]

The @racket[macro-name] argument is used to determine the macro's
binding, if it is an identifier. If @racket[expr-name] is given,
@racket[macro-name] is also included in the contract error message. If
@racket[macro-name] is omitted or @racket[#f], but @racket[context] is
a syntax object, then @racket[macro-name] is determined from
@racket[context].

If @racket[expr-name] is not @racket[#f], it is used in the contract's
error message to describe the expression the contract is applied to.

The @racket[context] argument is used, when necessary, to infer the
macro name for the negative blame party and the contract error
message. The @racket[context] should be either an identifier or a
syntax pair with an identifier in operator position; in either case,
that identifier is taken as the macro ultimately requesting the
contract wrapping.

See @secref{exprc} for an example.

@bold{Important:} Make sure when using @racket[expr/c] to use the
@racket[c] attribute. The @racket[expr/c] syntax class does not change how
pattern variables are bound; it only computes an attribute that
represents the checked expression.
}


@section{Literal Sets}

@defidform[kernel-literals]{

Literal set containing the identifiers for fully-expanded code
(@secref[#:doc '(lib "scribblings/reference/reference.scrbl")
"fully-expanded"]). The set contains all of the forms listed by
@racket[kernel-form-identifier-list], plus @racket[module],
@racket[#%plain-module-begin], @racket[#%require], and
@racket[#%provide].

Note that the literal-set uses the names @racket[#%plain-lambda] and
@racket[#%plain-app], not @racket[lambda] and @racket[#%app].
}
