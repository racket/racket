#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          (for-label scheme/base
                     scheme/contract
                     stxclass 
                     stxclass/util))

@(define ellipses @scheme[...])
@(define (TODO . pre-flow)
   (make-splice
    (cons (bold "TODO: ")
          (decode-content pre-flow))))

@title{Library syntax classes}
@declare-exporting[stxclass]

@(define-syntax-rule (defstxclass name . pre-flows)
   (defidform name . pre-flows))

@(define-syntax-rule (defstxclass* (name arg ...) . pre-flows)
   (defform (name arg ...) . pre-flows))

@defstxclass[expr]{

Matches anything except a keyword literal (to distinguish expressions
from the start of a keyword argument sequence). Does not expand or
otherwise inspect the term.

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

The following syntax classes mirror parts of the macro API. They may
only be used during transformation (when @scheme[syntax-transforming?]
returns true). Otherwise they may raise an error.

@defstxclass[static]{

Matches identifiers that are bound in the syntactic environment to
static information (see @scheme[syntax-local-value]). Attribute
@scheme[_value] contains the value the name is bound to.

}

@defform[(static-of description predicate)]{

Refines @scheme[static]: matches identifiers that are bound in the
syntactic environment to static information satisfying the given
@scheme[predicate]. Attribute @scheme[_value] contains the value the
name is bound to. The @scheme[description] argument is used for error
reporting.

}

@;{
@defstxclass[struct-name]{

Matches identifiers bound to static struct information. Attributes are
@scheme[_descriptor], @scheme[_constructor], @scheme[_predicate],
@scheme[(_accessor ...)], @scheme[_super], and @scheme[_complete?].

}
}
@;{
@defstxclass[expr/local-expand]{

Matches any term and @scheme[local-expand]s it as an expression with
an empty stop list. Attribute @scheme[_expanded] is the expanded form.

}

@defstxclass[expr/head-local-expand]
@defstxclass[block/head-local-expand]
@defstxclass[internal-definitions]
}

@;{
@defform[(expr/c contract-expr-stx)]{

Accepts any term and returns as the match that term wrapped in a
@scheme[contract] expression enforcing @scheme[contract-expr-stx].

}
}
