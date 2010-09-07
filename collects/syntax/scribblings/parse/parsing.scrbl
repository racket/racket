#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@title{Parsing syntax}

This section describes @scheme[syntax-parse], the
@schememodname[syntax/parse] library's facility for parsing
syntax. Both @scheme[syntax-parse] and the specification facility,
@seclink["stxparse-specifying"]{syntax classes}, use a common language
of @tech{syntax patterns}, which is described in detail in
@secref{stxparse-patterns}.

@declare-exporting[syntax/parse]

Two parsing forms are provided: @scheme[syntax-parse] and
@scheme[syntax-parser].

@defform/subs[(syntax-parse stx-expr parse-option ... clause ...+)
              ([parse-option (code:line #:context context-expr)
                             (code:line #:literals (literal ...))
                             (code:line #:literal-sets (literal-set ...))
                             (code:line #:conventions (convention-id ...))
                             (code:line #:local-conventions (convention-rule ...))]
               [literal literal-id
                        (pattern-id literal-id)
                        (pattern-id literal-id #:phase phase-expr)]
               [literal-set literal-set-id
                            (literal-set-id literal-set-option ...)]
               [literal-set-option (code:line #:at context-id)
                                   (code:line #:phase phase-expr)]
               [clause (syntax-pattern pattern-directive ... expr ...+)])
              #:contracts ([stx-expr syntax?]
                           [context-expr syntax?]
                           [phase-expr (or/c exact-integer? #f)])]{

Evaluates @scheme[stx-expr], which should produce a syntax object, and
matches it against the @scheme[clause]s in order. If some clause's
pattern matches, its attributes are bound to the corresponding
subterms of the syntax object and that clause's side conditions and
@scheme[expr] is evaluated. The result is the result of @scheme[expr].

If the syntax object fails to match any of the patterns (or all
matches fail the corresponding clauses' side conditions), a syntax
error is raised. 

The following options are supported:

@specsubform[(code:line #:context context-expr)
             #:contracts ([context-expr syntax?])]{

When present, @scheme[context-expr] is used in reporting parse
failures; otherwise @scheme[stx-expr] is used.

@(myexamples
  (syntax-parse #'(a b 3)
    [(x:id ...) 'ok])
  (syntax-parse #'(a b 3)
    #:context #'(lambda (a b 3) (+ a b))
    [(x:id ...) 'ok]))
}

@specsubform/subs[(code:line #:literals (literal ...))
                  ([literal literal-id
                            (pattern-id literal-id)
                            (pattern-id literal-id #:phase phase-expr)])
                  #:contracts ([phase-expr (or/c exact-integer? #f)])]{
@margin-note*{
  Unlike @scheme[syntax-case], @scheme[syntax-parse] requires all
  literals to have a binding. To match identifiers by their symbolic
  names, use the @scheme[~datum] pattern form instead.
}
@;
The @scheme[#:literals] option specifies identifiers that should be
treated as @tech{literals} rather than @tech{pattern variables}. An
entry in the literals list has two components: the identifier used
within the pattern to signify the positions to be matched
(@scheme[pattern-id]), and the identifier expected to occur in those
positions (@scheme[literal-id]). If the entry is a single identifier,
that identifier is used for both purposes.

If the @scheme[#:phase] option is given, then the literal is compared
at phase @scheme[phase-expr]. Specifically, the binding of the
@scheme[literal-id] at phase @scheme[phase-expr] must match the
input's binding at phase @scheme[phase-expr].
}

@specsubform/subs[(code:line #:literal-sets (literal-set ...))
                  ([literal-set literal-set-id
                                (literal-set-id literal-set-option ...)]
                   [literal-set-option (code:line #:at context-id)
                                       (code:line #:phase phase-expr)])
                  #:contracts ([phase-expr (or/c exact-integer? #f)])]{

Many literals can be declared at once via one or more @tech{literal
sets}, imported with the @scheme[#:literal-sets] option. See
@tech{literal sets} for more information.
}

@specsubform[(code:line #:conventions (conventions-id ...))]{

Imports @tech{convention}s that give default syntax classes to pattern
variables that do not explicitly specify a syntax class.
}

@specsubform[(code:line #:local-conventions (convention-rule ...))]{

Uses the @tech{conventions} specified. The advantage of
@scheme[#:local-conventions] over @scheme[#:conventions] is that local
conventions can be in the scope of syntax-class parameter
bindings. See the section on @tech{conventions} for examples.
}

Each clause consists of a @tech{syntax pattern}, an optional sequence
of @tech{pattern directives}, and a non-empty sequence of body
expressions.
}

@defform[(syntax-parser parse-option ... clause ...+)]{

Like @scheme[syntax-parse], but produces a matching procedure. The
procedure accepts a single argument, which should be a syntax object.
}
