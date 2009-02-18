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

@title{Parsing Syntax}
@declare-exporting[stxclass]

This section describes @schememodname[stxclass]'s facilities for
parsing syntax.

@defform/subs[(syntax-parse stx-expr maybe-literals clause ...)
              ([maybe-literals code:blank
                               (code:line #:literals (literal ...))]
               [literal id
                        (internal-id external-id)]
               [clause (syntax-pattern pattern-directive ... expr)])]{

Evaluates @scheme[stx-expr], which should produce a syntax object, and
matches it against the patterns in order. If some pattern matches, its
pattern variables are bound to the corresponding subterms of the
syntax object and that clause's side conditions and @scheme[expr] is
evaluated. The result is the result of @scheme[expr].

If the syntax object fails to match any of the patterns (or all
matches fail the corresponding clauses' side conditions), a syntax
error is raised. The syntax error indicates the first specific subterm
for which no pattern matches.

A literal in the literals list has two components: the identifier used
within the pattern to signify the positions to be matched, and the
identifier expected to occur in those positions. If the
single-identifier form is used, the same identifier is used for both
purposes.

}

@defform[(syntax-parser maybe-literals clause ...)]{

Like @scheme[syntax-parse], but produces a matching procedure. The
procedure accepts a single argument, which should be a syntax object.

}

The grammar of patterns accepted by @scheme[syntax-parse] and
@scheme[syntax-parser] follows:

@schemegrammar*[#:literals (_ ~or ~and)
                [syntax-pattern
                 pvar-id
                 pvar-id:syntax-class-id
                 literal-id
                 atomic-datum
                 (syntax-pattern . syntax-pattern)
                 (ellipsis-head-pattern #,ellipses . syntax-pattern)
                 (~and maybe-description syntax-pattern ...)]
                [ellipsis-head-pattern
                 (~or head ...+)
                 syntax-pattern]
                [maybe-description
                 (code:line)
                 (code:line #:description string)]
                [pvar-id
                 _
                 id]]

Here are the variants of @scheme[syntax-pattern]:

@specsubform[pvar-id]{

Matches anything. The pattern variable is bound to the matched
subterm, unless the pattern variable is the wildcard (@scheme[_]), in
which case no binding occurs.

}
@specsubform[pvar-id:syntax-class-id]{

Matches only subterms specified by the @scheme[_syntax-class-id]. The
syntax class's attributes are computed for the subterm and bound to
the pattern variables formed by prefixing @scheme[_pvar-id.] to the
name of the attribute. @scheme[_pvar-id] is typically bound to the
matched subterm, but the syntax class can substitute a transformed
subterm instead.

@;{(for example, @scheme[expr/c] wraps the matched
subterm in a @scheme[contract] expression).}

If @scheme[_pvar-id] is @scheme[_], no pattern variables are bound.

}
@specsubform[literal-id]{

An identifier that appears in the literals list is not a pattern
variable; instead, it is a literal that matches any identifier
@scheme[free-identifier=?] to it.

Specifically, if @scheme[literal-id] is the ``internal'' name of an
entry in the literals list, then it represents a pattern that matches
only identifiers @scheme[free-identifier=?] to the ``external''
name. These identifiers are often the same.

}
@specsubform[atomic-datum]{

The empty list, numbers, strings, booleans, and keywords match as
literals.

}
@specsubform[(syntax-pattern . syntax-pattern)]{

Matches a syntax pair whose head matches the first pattern and whose
tail matches the second.

}

@specsubform[(ellipsis-head-pattern #,ellipses . syntax-pattern)]{

Matches a sequence of the first pattern ending in a tail matching the
second pattern.

That is, the sequence pattern matches either the second pattern (which
need not be a list) or a pair whose head matches the first pattern and
whose tail recursively matches the whole sequence pattern.

The head pattern can be either an ordinary pattern or an
or/sequence-pattern:

@specsubform/subs[#:literals (~or)
                  (~or head ...+)
                  ([head
                    (code:line (syntax-pattern ...+) head-directive ...)]
                   [head-directive
                    (code:line #:min min-reps)
                    (code:line #:max max-reps)
                    (code:line #:mand)])]{

If the head is an or/sequence-pattern (introduced by @scheme[~or]),
then the whole sequence pattern matches any combination of the head
sequences followed by a tail matching the final pattern.

@specsubform[(code:line #:min min-reps)]{

Requires at least @scheme[min-reps] occurrences of the preceding head
to match. @scheme[min-reps] must be a literal exact nonnegative
integer.

}
@specsubform[(code:line #:max max-reps)]{

Requires that no more than @scheme[max-reps] occurrences of the
preceeding head to match. @scheme[max-reps] must be a literal exact
nonnegative integer, and it must be greater than or equal to
@scheme[min-reps].

}
@specsubform[#:mand]{

Requires that the preceding head occur exactly once. Pattern variables
in the preceding head are not bound at a higher ellipsis nesting
depth.

}
}
}
@specsubform/subs[#:literals (~and)
                  (~and maybe-description syntax-pattern ...)
                  ([maybe-description
                    (code:line)
                    (code:line #:description string)])]{

Matches any syntax that matches all of the included patterns.

}

Both @scheme[syntax-parse] and @scheme[syntax-parser] support
directives for annotating the pattern and specifying side
conditions. The grammar for pattern directives follows:

@schemegrammar[pattern-directive
               (code:line #:declare pattern-id syntax-class-id)
               (code:line #:declare pattern-id (syntax-class-id expr ...))
               (code:line #:with syntax-pattern expr)
               (code:line #:when expr)]

@specsubform[(code:line #:declare pvar-id syntax-class-id)]
@specsubform[(code:line #:declare pvar-id (syntax-class-id expr ...))]{

The first form is equivalent to using the
@scheme[_pvar-id:syntax-class-id] form in the pattern (but it is
illegal to use both for a single pattern variable). The
@scheme[#:declare] form may be preferred when writing macro-defining
macros or to avoid dealing with structured identifiers.

The second form allows the use of parameterized syntax classes, which
cannot be expressed using the ``colon'' notation. The @scheme[expr]s
are evaluated outside the scope of the pattern variable bindings.

}
@specsubform[(code:line #:with syntax-pattern expr)]{

Evaluates the @scheme[expr] in the context of all previous pattern
variable bindings and matches it against the pattern. If the match
succeeds, the new pattern variables are added to the environment for
the evaluation of subsequent side conditions. If the @scheme[#:with]
match fails, the matching process backtracks. Since a syntax object
may match a pattern in several ways, backtracking may cause the same
clause to be tried multiple times before the next clause is reached.

}
@specsubform[(code:line #:when expr)]{

Evaluates the @scheme[expr] in the context of all previous pattern
variable bindings. If it produces a false value, the matching process
backtracks as described above; otherwise, it continues.

}


@defidform[~and]{

Keyword recognized by @scheme[syntax-parse] etc as notation for
and-patterns.

}

@defidform[~or]{

Keyword recognized by @scheme[syntax-parse] etc as notation for
or/sequence-patterns (within sequences). It may not be used as an
expression.

}

@defform[(attribute attr-id)]{

Returns the value associated with the attribute named
@scheme[attr-id]. If @scheme[attr-id] is not bound as an attribute, an
error is raised. If @scheme[attr-id] is an attribute with a nonzero
ellipsis depth, then the result has the corresponding level of list
nesting.

The values returned by @scheme[attribute] never undergo additional
wrapping as syntax objects, unlike values produced by some uses of
@scheme[syntax], @scheme[quasisyntax], etc. Consequently, the
@scheme[attribute] form is preferred when the attribute value is used
as data, not placed in a syntax object.

}
