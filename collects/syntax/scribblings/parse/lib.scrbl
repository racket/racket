#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label syntax/kerncase))

@title{Library syntax classes and literal sets}

@section{Syntax classes}

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

@defstxclass[id]{ Alias for @scheme[identifier]. }
@defstxclass[nat]{ Alias for @scheme[exact-nonnegative-integer]. }

@defproc[(static [predicate (-> any/c any/c)]
                 [description (or/c string? #f)])
         (attributes value)]{

The @racket[static] syntax class matches an
identifier that is bound in the syntactic environment to static
information (see @scheme[syntax-local-value]) satisfying the given
@scheme[predicate]. If the term does not match, the
@scheme[description] argument is used to describe the expected syntax.

When used outside of the dynamic extent of a macro transformer (see
@scheme[syntax-transforming?]), matching fails.

The attribute @var[value] contains the value the name is bound to.
}


@section{Literal sets}

@defidform[kernel-literals]{

Literal set containing the identifiers for fully-expanded code
(@secref[#:doc '(lib "scribblings/reference/reference.scrbl")
"fully-expanded"]). The set contains all of the forms listed by
@scheme[kernel-form-identifier-list], plus @scheme[module],
@scheme[#%plain-module-begin], @scheme[#%require], and
@scheme[#%provide].

Note that the literal-set uses the names @scheme[#%plain-lambda] and
@scheme[#%plain-app], not @scheme[lambda] and @scheme[#%app].
}
