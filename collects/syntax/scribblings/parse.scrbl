#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          scheme/sandbox
          (for-label scheme/base
                     scheme/contract
                     syntax/parse
                     syntax/kerncase))

@(define ellipses @scheme[...])

@(begin
   (define (fixup exn)
     (let ([src (ormap values (exn:fail:syntax-exprs exn))])
       (if src
           (make-exn:fail:syntax
            (format "~a at: ~a" (exn-message exn) (syntax->datum src))
            (exn-continuation-marks exn)
            (exn:fail:syntax-exprs exn))
           exn)))
   (define the-eval
     (parameterize ((sandbox-output 'string)
                    (sandbox-error-output 'string)
                    (sandbox-make-code-inspector current-code-inspector)
                    (sandbox-eval-handlers
                     (list #f
                           (lambda (thunk)
                             (with-handlers ([exn:fail:syntax?
                                              (lambda (e) (raise (fixup e)))])
                               (thunk))))))
       (make-evaluator 'scheme/base
                       #:requires '(syntax/parse (for-syntax scheme/base)))))
   (the-eval '(error-print-source-location #f))
   (define-syntax-rule (myexamples e ...)
     (examples #:eval the-eval e ...)))

@title[#:tag "stxparse" #:style '(toc)]{Parsing and classifying syntax}

The @schememodname[syntax/parse] library provides a framework for
describing and parsing syntax. Using @schememodname[syntax/parse],
macro writers can define new syntactic categories, specify their legal
syntax, and use them to write clear, concise, and robust macros. The
library also provides a pattern-matching form, @scheme[syntax-parse],
which offers many improvements over @scheme[syntax-case].

@defmodule[syntax/parse]

@local-table-of-contents[]

@;{----------}

@section{Quick Start}

This section provides a rapid introduction to the
@schememodname[syntax/parse] library for the macro programmer.

To use @scheme[syntax-parse] to write a macro transformer, import it
@scheme[for-syntax]:

@schemeblock[(require (for-syntax syntax/parse))]

For example, here is is a module that defines
@schemekeywordfont{mylet}, a macro that has the same behavior as the
standard @scheme[let] form (including ``named @scheme[let]''):

@schemeblock[
(module example scheme/base
  (require (for-syntax scheme/base syntax/parse))
  (define-syntax (mylet stx)
    (syntax-parse stx
      [(_ loop:id ((x:id e:expr) ...) . body)
       #'(letrec ([loop (lambda (x ...) . body)])
           (loop e ...))]
      [(_ ((x:id e:expr) ...) . body)
       #'((lambda (x ...) . body) e ...)])))
]

The macro is defined as a procedure that takes one argument,
@scheme[stx]. The @scheme[syntax-parse] form is similar to
@scheme[syntax-case], except that there is no literals list between
the syntax argument and the sequence of clauses.

@bold{Note: } Remember not to put a literals list between the syntax
argument and the clauses!

The patterns contain identifiers consisting of two parts separated by
a colon character, such as @scheme[loop:id] or @scheme[e:expr]. These
are pattern variables annotated with syntax classes. For example,
@scheme[loop:id] is a pattern variable named @scheme[loop] with the
syntax class @scheme[id] (identifier). Note that only the pattern
variable part is used in the syntax template.

Syntax classes restrict what a pattern variable can match. Above,
@scheme[loop] only matches an identifier, so the first clause only
matches the ``named-let'' syntax. Syntax classes replace some uses of
@scheme[syntax-case]'s ``fenders'' or guard expressions. They also
enable @scheme[syntax-parse] to automatically give specific error
messages.

The @schememodname[syntax/parse] library provides several built-in
syntax classes (see @secref{lib} for a list). Programmers can also
define their own using @scheme[define-syntax-class]:

@schemeblock[
(module example-syntax scheme/base
  (require syntax/parse)
  (provide binding)
  (define-syntax-class binding
    #:attributes (x e)
    (pattern (x:id e:expr))))

(module example scheme/base
  (require (for-syntax scheme/base
                       syntax/parse
                       'example-syntax))
  (define-syntax (mylet stx)
    (syntax-parse stx
      [(_ loop:id (b:binding ...) . body)
       #'(letrec ([loop (lambda (b.x ...) . body)])
           (loop b.e ...))]
      [(_ (b:binding ...) . body)
       #'((lambda (b.x ...) . body) b.e ...)])))
]

@bold{Note:} Syntax classes must be defined in the same phase as the
@scheme[syntax-parse] expression they're used in. The right-hand side
of a macro is at phase 1, so syntax classes it uses must be defined in
a separate module and required @scheme[for-syntax]. Since the
auxiliary module uses @scheme[define-syntax-class] at phase 0, it has
@scheme[(require syntax/parse)], with no @scheme[for-syntax].

Alternatively, the syntax class could be made a local definition,
thus:

@schemeblock[
(module example scheme/base
  (require (for-syntax scheme/base
                       syntax/parse))
  (define-syntax (mylet stx)
    (define-syntax-class binding
      #:attributes (x e)
      (pattern (x:id e:expr)))
    (syntax-parse stx
      [(_ loop:id (b:binding ...) . body)
       #'(letrec ([loop (lambda (b.x ...) . body)])
           (loop b.e ...))]
      [(_ (b:binding ...) . body)
       #'((lambda (b.x ...) . body) b.e ...)])))
]

A syntax class is an abstraction of a syntax pattern. The syntax class
@scheme[binding] gives a name to the repeated pattern fragment
@scheme[(x:id e:expr)]. The components of the fragment, @scheme[x] and
@scheme[e], become @tech{attributes} of the syntax class. When
@scheme[b:binding] matches, @scheme[b] gets bound to the whole binding
pair, and @scheme[b.x] and @scheme[b.e] get bound to the variable name
and expression, respectively. Actually, all of them are bound to
sequences, because of the ellipses.

Syntax classes can have multiple alternative patterns. Suppose we
wanted to extend @schemekeywordfont{mylet} to allow a simple
identifier as a binding, in which case it would get the value
@scheme[#f]:

@schemeblock[
(mylet ([a 1] b [c 'foo]) ....)
]

Here's how the syntax class would change:

@SCHEMEBLOCK[
(module example-syntax scheme/base
  (require syntax/parse)
  (require (for-template scheme/base))
  (provide binding)
  (define-syntax-class binding
    #:attributes (x e)
    (pattern (x:id e:expr))
    (pattern x:id
             #:with e #'(quote #f))))
]

@bold{Note: } The @scheme[(require (for-template scheme/base))] is
needed for the @scheme[quote] expression.

If the syntax class definition were a local definition in the same
module, the @scheme[for-template] would be unnecessary.

The second pattern matches unparenthesized identifiers. The @scheme[e]
attribute is bound using a @scheme[#:with] clause, which matches the
pattern @scheme[e] against the syntax from evaluating @scheme[#'#f].

Optional keyword arguments are supported via ``head patterns'' (called
@tech{H-patterns} in the reference documentation). Unlike normal
patterns, which match one term, head patterns can match a variable
number of subterms in a list.

Suppose @schemekeywordfont{mylet} accepted an optional
@scheme[#:check] keyword with one argument, a procedure that would be
applied to every variable's value. Here's one way to write it
(dropping the named-let variant for simplicity):

@SCHEMEBLOCK[
(define-syntax (mylet stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:check pred)) (b:binding ...) . body)
     #`((lambda (b.x ...)
          #,(if (attribute pred)
                #'(unless (and (pred b.x) ...) (error 'check))
                #'(void))
          . body)
        b.e ...)]))
]

An optional subpattern might not match, so attributes within an
@scheme[~optional] form might not be bound to syntax. Such
non-syntax-valued attributes may not be used within syntax
templates. The @scheme[attribute] special form is used to get the
value of an attribute; if the attribute didn't get matched, the value
is @scheme[#f].

Here's another way write it, using @scheme[#:defaults] to give the
@scheme[pred] attribute a default value:

@schemeblock[
(define-syntax (mylet stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:check pred)
                   #:defaults ([pred #'(lambda (x) #t)]))
        (b:binding ...) . body)
     #`((lambda (b.x ...)
          (unless (and (pred b.x) ...) (error 'check))
          . body)
        b.e ...)]))
]

Programmers can also create abstractions over head patterns, using
@scheme[define-splicing-syntax-class]. Here it is, rewritten to use
multiple alternatives instead of @scheme[~optional]:

@schemeblock[
(define-splicing-syntax-class optional-check
  #:attributes (pred)
  (pattern (~seq #:check pred))
  (pattern (~seq)
           #:with pred #'(lambda (x) #t)))
]

@bold{Note: } When defining a splicing syntax class, remember to
include @scheme[~seq] in the pattern!

Here is the corresponding macro:

@schemeblock[
(define-syntax (mylet stx)
  (syntax-parse stx
    [(_ c:optional-check (b:binding ...) . body)
     #'((lambda (b.x ...)
          (unless (and (c.pred b.x) ...) (error 'check))
          . body)
        b.e ...)]))
]

The documentation in the following sections contains additional
examples of @schememodname[syntax/parse] features.


@;{----------}

@section{Parsing syntax}

This section describes the @scheme[syntax-parse] pattern matching
form, syntax patterns, and attributes.

@defform/subs[(syntax-parse stx-expr parse-option ... clause ...+)
              ([parse-option (code:line #:context context-expr)
                             (code:line #:literals (literal ...))
                             (code:line #:literal-sets (literal-set ...))
                             (code:line #:conventions (convention-id ...))]
               [literal literal-id
                        (pattern-id literal-id)]
               [literal-set literal-set-id
                            [literal-set-id #:at context-id]]
               [clause (syntax-pattern pattern-directive ... expr)])
              #:contracts ([stx-expr syntax?])]{

Evaluates @scheme[stx-expr], which should produce a syntax object, and
matches it against the @scheme[clause]s in order. If some clause's
pattern matches, its attributes are bound to the corresponding
subterms of the syntax object and that clause's side conditions and
@scheme[expr] is evaluated. The result is the result of @scheme[expr].

If the syntax object fails to match any of the patterns (or all
matches fail the corresponding clauses' side conditions), a syntax
error is raised. If the @scheme[#:context] argument is given,
@scheme[context-expr] is used in reporting the error; otherwise
@scheme[stx-expr] is used.

@(myexamples
  (syntax-parse #'(a b 3)
    [(x:id ...) 'ok])
  (syntax-parse #'(a b 3)
    #:context #'(lambda (a b 3) (+ a b))
    [(x:id ...) 'ok]))

The @scheme[#:literals] option specifies identifiers that should be
treated as @tech{literals} rather than @tech{pattern variables}. An
entry in the literals list has two components: the identifier used
within the pattern to signify the positions to be matched
(@scheme[pattern-id]), and the identifier expected to occur in those
positions (@scheme[literal-id]). If the entry is a single identifier,
that identifier is used for both purposes.

@bold{Note:} Unlike @scheme[syntax-case], @scheme[syntax-parse]
requires all literals to have a binding. To match identifiers by their
symbolic names, consider using the @scheme[atom-in-list] syntax class
instead.

Many literals can be declared at once via one or more @tech{literal
sets}, imported with the @scheme[#:literal-sets] option. The
literal-set definition determines the literal identifiers to recognize
and the names used in the patterns to recognize those literals.

The @scheme[#:conventions] option imports @tech{convention}s that give
default syntax classes to pattern variables that do not explicitly
specify a syntax class.
}

@defform[(syntax-parser parse-option ... clause ...+)]{

Like @scheme[syntax-parse], but produces a matching procedure. The
procedure accepts a single argument, which should be a syntax object.

}

The grammar of @deftech{syntax patterns} accepted by
@scheme[syntax-parse] and @scheme[syntax-parser] is given in the
following table:

@schemegrammar*[#:literals (_ ~var ~literal ~or ~and ~not ~seq 
                            ~rep ~once ~optional
                            ~rest ~struct ~! ~describe ~bind ~fail)
                [S-pattern
                 pvar-id
                 pvar-id:syntax-class-id
                 literal-id
                 (~var id)
                 (~var id syntax-class)
                 (~literal literal-id)
                 atomic-datum
                 (H-pattern . S-pattern)
                 ((~or EH-pattern ...+) #,ellipses . S-pattern)
                 (EH-pattern #,ellipses . S-pattern)
                 (~and S-pattern ...+)
                 (~or S-pattern ...+)
                 (~not S-pattern)
                 #((unsyntax @svar[pattern-part]) ...)
                 #s(prefab-struct-key (unsyntax @svar[pattern-part]) ...)
                 (~rest S-pattern)
                 (~describe expr S-pattern)
                 (~! . S-pattern)
                 (~bind [attr-id expr] ...)
                 (~fail maybe-fail-condition message-expr)]
                [L-pattern
                 ()
                 (H-pattern . L-pattern)
                 ((~or EH-pattern ...+) #,ellipses . L-pattern)
                 (EH-pattern #,ellipses . L-pattern)
                 (~rest L-pattern)
                 (~! . L-pattern)]
                [H-pattern
                 pvar-id:splicing-syntax-class-id
                 (~var id splicing-syntax-class)
                 (~seq . L-pattern)
                 (~and H-pattern ...+)
                 (~or H-pattern ...+)
                 (~optional H-pattern maybe-optional-option)
                 (~describe expr H-pattern)
                 S-pattern]
                [EH-pattern
                 (~once H-pattern once-option ...)
                 (~optional H-pattern optional-option ...)
                 H-pattern]]

There are three main kinds of syntax pattern: @tech{S-patterns} (for
``single patterns''), @tech{H-patterns} (for ``head patterns''), and
@tech{EH-patterns} (for ``ellipsis head patterns''). A fourth kind,
@tech{L-patterns} (for ``list patterns''), is a restricted subset of
@tech{S-patterns}. When a special form in this manual refers to
@svar[syntax-pattern] (eg, the description of the
@scheme[syntax-parse] special form), it means specifically
@tech{S-pattern}.

@subsection{S-pattern variants}

An @deftech{S-pattern} (for ``single pattern'') is a pattern that
describes a single term. The pattern may, of course, consist of other
parts. For example, @scheme[(17 ...)] is an @tech{S-pattern}
that matches any term that is a proper list of repeated
@schemeresult[17] numerals. The @deftech{L-pattern}s (for ``list
pattern'') are @tech{S-pattern} having a restricted structure that
constrains it to match only terms that are proper lists.

Here are the variants of @tech{S-pattern}:

@specsubform[id]{

An identifier can be either a @tech{pattern variable}, an
@tech{annotated pattern variable}, or a @tech{literal}:

@itemize[

@item{If @scheme[id] is the ``pattern'' name of an entry in the
  literals list, it is a @tech{literal} pattern that behaves
  like @scheme[(~literal id)].

  @myexamples[
    (syntax-parse #'(define x 12)
      #:literals (define)
      [(define var:id body:expr) 'ok])
    (syntax-parse #'(lambda x 12)
      #:literals (define)
      [(define var:id body:expr) 'ok])
    (syntax-parse #'(define x 12)
      #:literals ([def define])
      [(def var:id body:expr) 'ok])
    (syntax-parse #'(lambda x 12)
      #:literals ([def define])
      [(def var:id body:expr) 'ok])
  ]
}

@item{If @scheme[id] is of the form @scheme[_pvar-id:syntax-class-id]
  (that is, two names joined by a colon character), it is an
  @tech{annotated pattern variable}, and the pattern is equivalent to
  @scheme[(~var pvar-id syntax-class-id)].

  @myexamples[
    (syntax-parse #'a
      [var:id (syntax-e #'var)])
    (syntax-parse #'12
      [var:id (syntax-e #'var)])
    (define-syntax-class two
      #:attributes (x y)
      (pattern (x y)))
  (syntax-parse #'(a b)
    [t:two (syntax->datum #'(t t.x t.y))])
  (syntax-parse #'(a b)
    [t
     #:declare t two
     (syntax->datum #'(t t.x t.y))])
  ]
}

@item{Otherwise, @scheme[id] is a @tech{pattern variable}, and the
pattern is equivalent to @scheme[(~var id)].
}
]
}

@specsubform[#:literals (~var) (~var pvar-id)]{

A @deftech{pattern variable}. If @scheme[pvar-id] has no syntax class
(by @scheme[#:convention]), the pattern variable matches anything. The
pattern variable is bound to the matched subterm, unless the pattern
variable is the wildcard (@scheme[_]), in which case no binding
occurs.

If @scheme[pvar-id] does have an associated syntax class, it behaves
like an @tech{annotated pattern variable} with the implicit syntax
class inserted.
}

@specsubform/subs[#:literals (~var) (~var pvar-id syntax-class)
                  ([syntax-class syntax-class-id
                                 (syntax-class-id arg-expr ...)])]{

An @deftech{annotated pattern variable}. The pattern matches only
terms accepted by @svar[syntax-class-id] (parameterized by the
@scheme[arg-expr]s, if present).

The syntax class's @tech{attributes} are computed for the term and
bound to the names formed by prefixing @svar[pvar-id.] to the name of
the attribute. The name @svar[pvar-id] itself is bound to the whole
term.

If @svar[pvar-id] is @scheme[_], no attributes are bound.

@myexamples[
(syntax-parse #'a
  [(~var var id) (syntax-e #'var)])
(syntax-parse #'12
  [(~var var id) (syntax-e #'var)])
(define-syntax-class two
  #:attributes (x y)
  (pattern (x y)))
(syntax-parse #'(a b)
  [(~var t two) (syntax->datum #'(t t.x t.y))])
(define-syntax-class (nat-less-than n)
  (pattern x:nat #:when (< (syntax-e #'x) n)))
(syntax-parse #'(1 2 3 4 5)
  [((~var small (nat-less-than 4)) ... large:nat ...)
   (list #'(small ...) #'(large ...))])
]
}

@specsubform[#:literals (~literal) (~literal literal-id)]{

A @deftech{literal} identifier pattern. Matches any identifier
@scheme[free-identifier=?] to @scheme[literal-id].

@myexamples[
(syntax-parse #'(define x 12)
  [((~literal define) var:id body:expr) 'ok])
(syntax-parse #'(lambda x 12)
  [((~literal define) var:id body:expr) 'ok])
]
}

@specsubform[atomic-datum]{

Numbers, strings, booleans, keywords, and the empty list match as
literals.

@myexamples[
(syntax-parse #'(a #:foo bar)
  [(x #:foo y) (syntax->datum #'y)])
(syntax-parse #'(a foo bar)
  [(x #:foo y) (syntax->datum #'y)])
]
}

@specsubform[(H-pattern . S-pattern)]{

Matches any term that can be decomposed into a list prefix matching
the @tech{H-pattern} and a suffix matching the S-pattern.

Note that the pattern may match terms that are not even improper
lists; if the head pattern can match a zero-length head, then the
whole pattern matches whatever the tail pattern accepts.

The first pattern can be an @tech{S-pattern}, in which case the whole
pattern matches any pair whose first element matches the first pattern
and whose rest matches the second.

See @tech{H-patterns} for more information.
}

@specsubform[#:literals (~or) ((~or EH-pattern ...+) #,ellipses . S-pattern)]{

Matches any term that can be decomposed into a list head matching some
number of repetitions of the @tech{EH-pattern} alternatives (subject
to its repetition constraints) followed by a list tail matching the
S-pattern.

In other words, the whole pattern matches either the second pattern
(which need not be a list) or a term whose head matches one of the
alternatives of the first pattern and whose tail recursively matches
the whole sequence pattern.

See @tech{EH-patterns} for more information.
}

@specsubform[(EH-pattern #,ellipses . S-pattern)]{

The @scheme[~or]-free variant of ellipses (@ellipses) pattern is
equivalent to the @scheme[~or] variant with just one alternative.
}

@specsubform[#:literals (~and) (~and S-pattern ...)]{

Matches any term that matches all of the subpatterns.

Attributes bound in subpatterns are available to subsequent
subpatterns. The whole pattern binds all of the subpatterns'
attributes.

One use for @scheme[~and]-patterns is preserving a whole
term (including its lexical context, source location, etc) while also
examining its structure. Syntax classes are useful for the same
purpose, but @scheme[~and] can be lighter weight.

@myexamples[
(define-syntax (import stx)
  (raise-syntax-error #f "illegal use of import" stx))
(eval:alts
  (define (check-imports stx) ....)
  (define (check-imports . _) #f))
(syntax-parse #'(m (import one two))
  #:literals (import)
  [(_ (~and import-clause (import i ...)))
   (let ([bad (check-imports
               (syntax->list #'(i ...)))])
     (when bad
       (raise-syntax-error
        #f "bad import" #'import-clause bad))
     'ok)])
]

}

@specsubform[#:literals (~or) (~or S-pattern ...)]{

Matches any term that matches one of the included patterns. The
alternatives are tried in order.

The whole pattern binds @emph{all} of the subpatterns' attributes. An
attribute that is not bound by the ``chosen'' subpattern has a value
of @scheme[#f]. The same attribute may be bound by multiple
subpatterns, and if it is bound by all of the subpatterns, it is sure
to have a value if the whole pattern matches.

@myexamples[
(syntax-parse #'a
  [(~or x:id (~and x #f)) (syntax->datum #'x)])
(syntax-parse #'#f
  [(~or x:id (~and x #f)) (syntax->datum #'x)])
]
}

@specsubform[#:literals (~not) (~not S-pattern)]{

Matches any term that does not match the subpattern. The subpattern's
attributes are @emph{not} bound outside of the @scheme[~not]-pattern.

@myexamples[
(syntax-parse #'(x y z => u v)
  #:literals (=>)
  [((~and before (~not =>)) ... => after ...)
   (list #'(before ...) #'(after ...))])
]
}

@specsubform[#(#, @svar[pattern-part] ...)]{

Matches a term that is a vector whose elements, when considered as a
list, match the @tech{S-pattern} corresponding to
@scheme[(pattern-part ...)].

@myexamples[
(syntax-parse #'#(1 2 3)
  [#(x y z) (syntax->datum #'z)])
(syntax-parse #'#(1 2 3)
  [#(x y ...) (syntax->datum #'(y ...))])
(syntax-parse #'#(1 2 3)
  [#(x ~rest y) (syntax->datum #'y)])
]

}

@specsubform[#s(prefab-struct-key #, @svar[pattern-part] ...)]{

Matches a term that is a prefab struct whose key is exactly the given
key and whose sequence of fields, when considered as a list, match the
@tech{S-pattern} corresponding to @scheme[(pattern-part ...)].

@myexamples[
(syntax-parse #'#s(point 1 2 3)
  [#s(point x y z) 'ok])
(syntax-parse #'#s(point 1 2 3)
  [#s(point x y ...) (syntax->datum #'(y ...))])
(syntax-parse #'#s(point 1 2 3)
  [#s(point x ~rest y) (syntax->datum #'y)])
]
}

@specsubform[#:literals (~rest) (~rest S-pattern)]{

Matches just like the inner @scheme[S-pattern]. The @scheme[~rest]
pattern form is useful in positions where improper lists (``dots'')
are not allowed by the reader, such as vector and structure patterns
(see above).

@myexamples[
(syntax-parse #'(1 2 3)
  [(x ~rest y) (syntax->datum #'y)])
(syntax-parse #'#(1 2 3)
  [#(x ~rest y) (syntax->datum #'y)])
]
}

@specsubform[#:literals (~describe) (~describe expr S-pattern)]{

The @scheme[~describe] pattern form annotates a pattern with a
description, a string expression that is evaluated in the scope of all
prior attribute bindings. If parsing the inner pattern fails, then the
description is used to synthesize the error message.

A describe-pattern also affects backtracking in two ways:

@itemize{

@item{A cut-pattern (@scheme[~!]) within a describe-pattern only
eliminates choice-points created within the describe-pattern.}

@item{If a describe-pattern succeeds, then all choice points created
within the describe-pattern are discarded, and a failure @emph{after}
the describe-pattern backtracks to a choice point @emph{before} the
describe-pattern, never one @emph{within} it.}}}

@specsubform[#:literals (~!) (~! . S-pattern)]{

The @scheme[~!] operator, pronounced ``cut'', eliminates backtracking
choice points and commits parsing to the current branch of the pattern
it is exploring.

Common opportunities for cut-patterns come from recognizing special
forms based on keywords. Consider the following expression:

@interaction[#:eval the-eval
(syntax-parse #'(define-values a 123)
  #:literals (define-values define-syntaxes)
  [(define-values (x:id ...) e) 'define-values]
  [(define-syntaxes (x:id ...) e) 'define-syntaxes]
  [e 'expression])]

Given the ill-formed term @scheme[(define-values a 123)], the
expression tries the first clause, fails to match @scheme[a] against
the pattern @scheme[(x:id ...)], and then backtracks to the second
clause and ultimately the third clause, producing the value
@scheme['expression]. But the term is not an expression; it is an
ill-formed use of @scheme[define-values]! The proper way to write the
@scheme[syntax-parse] expression follows:

@interaction[#:eval the-eval
(syntax-parse #'(define-values a 123)
  #:literals (define-values define-syntaxes)
  [(define-values ~! (x:id ...) e) 'define-values]
  [(define-syntaxes ~! (x:id ...) e) 'define-syntaxes]
  [e 'expression])]

Now, given the same term, @scheme[syntax-parse] tries the first
clause, and since the keyword @scheme[define-values] matches, the
cut-pattern commits to the current pattern, eliminating the choice
points for the second and third clauses. So when the clause fails to
match, the @scheme[syntax-parse] expression raises an error.

The effect of a @scheme[~!] pattern is delimited by the nearest
enclosing @scheme[~describe] pattern. If there is no enclosing
@scheme[~describe] pattern but the cut occurs within a syntax class
definition, then only choice points within the syntax class definition
are discarded.
}

@specsubform[#:literals (~bind) (~bind [attr-id expr] ...)]{

This pattern matches any term. Its effect is to evaluate the
@scheme[expr]s and bind them to the given @scheme[attr-id]s as
attributes.
}

@specsubform/subs[#:literals (~fail) (~fail maybe-fail-condition message-expr)
                  ([maybe-fail-condition (code:line)
                                         (code:line #:when condition-expr)
                                         (code:line #:unless condition-expr)])]{

This pattern succeeds or fails independent of the term being matched
against. If the condition is absent, or if the @scheme[#:when]
condition evaluates to a true value, or if the @scheme[#:unless]
condition evaluates to @scheme[#f], then the pattern fails with the
given message. Otherwise the pattern succeeds.

Fail patterns can be used together with cut patterns to recognize
specific ill-formed terms and address them with specially-created
failure messages.
}


@subsection{H-pattern variants}

An @deftech{H-pattern} (for ``head pattern'') is a pattern that
describes some number of terms that occur at the head of some list
(possibly an improper list). An H-pattern's usefulness comes from
being able to match heads of different lengths.  H-patterns are useful
for specifying optional forms such as keyword arguments.

Here are the variants of @tech{H-pattern}:

@specsubform[#:literals (~seq) (~seq . L-pattern)]{

Matches a head whose elements, if put in a list, would match the given
@tech{L-pattern}.

@myexamples[
(syntax-parse #'(1 2 3 4)
  [((~seq 1 2 3) 4) 'ok])
]

See also the section on @tech{EH-patterns} for more interesting
examples of @scheme[~seq].

}

@specsubform[#:literals (~and) (~and H-pattern ...)]{

Like the S-pattern version of @scheme[~and], but matches a term head
instead.

@myexamples[
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (~seq keyword-stuff ...))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
]

The H-pattern of @scheme[~and] requires that all of the subpatterns be
strictly @tech{H-patterns} and not @tech{S-patterns}. This is to
prevent typos like the following, a variant of the previous example
with the second @scheme[~seq] omitted:

@myexamples[
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (keyword-stuff ...))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
(code:comment "If the example above were allowed, it would be equivalent to this:")
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (~seq (keyword-stuff ...)))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
]
}

@specsubform[#:literals (~or) (~or H-pattern ...)]{

Like the S-pattern version of @scheme[~or], but matches a term head
instead.

@myexamples[
(syntax-parse #'(m #:foo 2 a b c)
  [(_ (~or (~seq #:foo x) (~seq)) y:id ...)
   (attribute x)])
(syntax-parse #'(m a b c)
  [(_ (~or (~seq #:foo x) (~seq)) y:id ...)
   (attribute x)])
]
}

@specsubform/subs[#:literals (~optional) (~optional H-pattern maybe-optional-option)
                  ([maybe-optional-option
                    (code:line)
                    (code:line #:defaults ([attr-id expr] ...))])]{

Matches either the given head subpattern or an empty head. If the
@scheme[#:defaults] option is given, the subsequent attribute bindings
are used if the subpattern does not match. The default attributes must
be a subset of the subpattern's attributes.

@myexamples[
(syntax-parse #'(m #:foo 2 a b c)
  [(_ (~optional (~seq #:foo x) #:defaults ([x #'#f])) y:id ...)
   (attribute x)])
(syntax-parse #'(m a b c)
  [(_ (~optional (~seq #:foo x) #:defaults ([x #'#f])) y:id ...)
   (attribute x)])
(syntax-parse #'(m a b c)
  [(_ (~optional (~seq #:foo x)) y:id ...)
   (attribute x)])
]

}

@specsubform[#:literals (~describe) (~describe expr H-pattern)]{

Like the S-pattern version of @scheme[~describe], but matches a head
pattern instead.
}

@specsubform[S-pattern]{

Matches a head of one element, which must be a term matching the given
@tech{S-pattern}.
}


@subsection{EH-pattern forms}

An @deftech{EH-pattern} (for ``ellipsis-head pattern'') is pattern
that describes some number of terms, like an @tech{H-pattern}, but may
also place contraints on the number of times it occurs in a
repetition. EH-patterns (and ellipses) are useful for matching keyword
arguments where the keywords may come in any order.

@myexamples[
(define parser1
  (syntax-parser
   [((~or (~once (~seq #:a x) #:name "#:a keyword")
          (~optional (~seq #:b y) #:name "#:b keyword")
          (~seq #:c z)) ...)
    'ok]))
(parser1 #'(#:a 1))
(parser1 #'(#:b 2 #:c 3 #:c 25 #:a 'hi))
(parser1 #'(#:a 1 #:a 2))
]

The pattern requires exactly one occurrence of the @scheme[#:a]
keyword and argument, at most one occurrence of the @scheme[#:b]
keyword and argument, and any number of @scheme[#:c] keywords and
arguments. The ``pieces'' can occur in any order.

Here are the variants of @tech{EH-pattern}:

@specsubform/subs[#:literals (~once) (~once H-pattern once-option ...)
                  ([once-option (code:line #:name name-expr)
                                (code:line #:too-few too-few-message-expr)
                                (code:line #:too-many too-many-message-expr)])]{

Matches if the inner H-pattern matches. This pattern must be selected
exactly once in the match of the entire repetition sequence.

If the pattern is not chosen in the repetition sequence, then an error
is raised with a message, either @scheme[too-few-message-expr] or
@schemevalfont{"missing required occurrence of @scheme[name-expr]"}.

If the pattern is chosen more than once in the repetition sequence,
then an error is raised with a message, either
@scheme[too-many-message-expr] or @schemevalfont{"too many occurrences
of @scheme[name-expr]"}.
}

@specsubform/subs[#:literals (~optional) (~optional H-pattern optional-option ...)
                  ([optional-option (code:line #:name name-expr)
                                    (code:line #:too-many too-many-message-expr)
                                    (code:line #:defaults ([attr-id expr] ...))])]{

Matches if the inner H-pattern matches. This pattern may be used at
most once in the match of the entire repetition.

If the pattern is chosen more than once in the repetition sequence,
then an error is raised with a message, either
@scheme[too-many-message-expr] or @schemevalfont{"too many occurrences
of @scheme[name-expr]"}.

If the @scheme[#:defaults] option is given, the following attribute
bindings are used if the subpattern does not match at all in the
sequence. The default attributes must be a subset of the subpattern's
attributes.
}


@subsection{Pattern directives}

Both @scheme[syntax-parse] and @scheme[syntax-parser] support
directives for annotating the pattern and specifying side
conditions. The grammar for pattern directives follows:

@schemegrammar[pattern-directive
               (code:line #:declare pattern-id syntax-class-id)
               (code:line #:declare pattern-id (syntax-class-id expr ...))
               (code:line #:with syntax-pattern expr)
               (code:line #:attr attr-id expr)
               (code:line #:fail-unless condition-expr message-expr)
               (code:line #:fail-when condition-expr message-expr)
               (code:line #:when condition-expr)]

@specsubform[(code:line #:declare pvar-id syntax-class-id)]
@specsubform[(code:line #:declare pvar-id (syntax-class-id expr ...))]{

The first form is equivalent to using the
@svar[pvar-id:syntax-class-id] form in the pattern (but it is
illegal to use both for a single pattern variable). The
@scheme[#:declare] form may be preferred when writing macro-defining
macros or to avoid dealing with structured identifiers.

The second form allows the use of parameterized syntax classes, which
cannot be expressed using the ``colon'' notation. The @scheme[expr]s
are evaluated outside the scope of any of the attribute bindings from
pattern that the @scheme[#:declare] directive applies to.

}

@specsubform[(code:line #:with syntax-pattern stx-expr)]{

Evaluates the @scheme[stx-expr] in the context of all previous
attribute bindings and matches it against the pattern. If the match
succeeds, the pattern's attributes are added to environment for the
evaluation of subsequent side conditions. If the @scheme[#:with] match
fails, the matching process backtracks. Since a syntax object may
match a pattern in several ways, backtracking may cause the same
clause to be tried multiple times before the next clause is reached.
}

@specsubform[(code:line #:attr attr-id expr)]{

Evaluates the @scheme[expr] in the context of all previous attribute
bindings and binds it to the attribute named by @scheme[attr-id]. The
value of @scheme[expr] need not be syntax.
}

@specsubform[(code:line #:fail-unless condition-expr message-expr)]{

Evaluates the @scheme[condition-expr] in the context of all previous
attribute bindings. If the value is any @scheme[#f], the matching
process backtracks (with the given message); otherwise, it continues.
}

@specsubform[(code:line #:fail-when condition-expr message-expr)]{

Like @scheme[#:fail-unless] with the condition negated.
}

@specsubform[(code:line #:when condition-expr)]{

Evaluates the @scheme[condition-expr] in the context of all previous
attribute bindings. If the value is @scheme[#f], the matching process
backtracks. In other words, @scheme[#:when] is like
@scheme[#:fail-unless] without the message argument.

}

@deftogether[[
@defidform[~var]
@defidform[~literal]
@defidform[~or]
@defidform[~and]
@defidform[~not]
@defidform[~seq]
@defidform[~once]
@defidform[~optional]
@defidform[~rest]
@defidform[~describe]
@defidform[~!]
@defidform[~bind]
@defidform[~fail]]]{

Syntax pattern keywords, recognized by @scheme[syntax-parse].

}

@;{----------}

@section{Pattern Variables and Attributes}

An @deftech{attribute} is a name bound by a syntax pattern. An
attribute can be a @tech{pattern variable} itself, or it can be a
@deftech{nested attribute} coming from the syntax class of some
pattern variable. The name of a nested attribute is computed by
concatenating the pattern variable name with the syntax class's
exported attribute's name, separated by a dot (see the example below).

Attribute names cannot be used directly as expressions (that is,
attributes are not variables). Instead, an attribute's value can be
gotten using the @scheme[attribute] special form.

@defform[(attribute attr-id)]{

Returns the value associated with the attribute named
@scheme[attr-id]. If @scheme[attr-id] is not bound as an attribute, an
error is raised. If @scheme[attr-id] is an attribute with a nonzero
ellipsis depth, then the result has the corresponding level of list
nesting.

}

The value of an attribute need not be syntax. Non-syntax-valued
attributes can be used to return a parsed representation of a subterm
or the results of an analysis on the subterm. A non-syntax-valued
attribute should be bound using the @scheme[#:attr] directive or a
@scheme[~bind] pattern.

@myexamples[
(define-syntax-class table
  (pattern ((key value) ...)
           #:attr hash
                  (for/hash ([k (syntax->datum #'(key ...))]
                             [v (syntax->datum #'(value ...))])
                    (values k v))))
(syntax-parse #'((a 1) (b 2) (c 3))
  [t:table
   (attribute t.hash)])
]

A syntax-valued attribute is an attribute whose value is a syntax
object or a syntax list of the appropriate @tech{ellipsis
depth}. Syntax-valued attributes can be used within @scheme[syntax],
@scheme[quasisyntax], etc as part of a syntax template. If a
non-syntax-valued attribute is used in a syntax template, a runtime
error is signalled.

@myexamples[
(syntax-parse #'((a 1) (b 2) (c 3))
  [t:table
   #'(t.key ...)])
(syntax-parse #'((a 1) (b 2) (c 3))
  [t:table
   #'t.hash])
]

Every attribute has an associated @deftech{ellipsis depth} that
determines how it can be used in a syntax template (see the discussion
of ellipses in @scheme[syntax]). For a pattern variable, the ellipsis
depth is the number of ellipses the pattern variable ``occurs under''
in the pattern. For a nested attribute the depth is the sum of the
pattern variable's depth and the depth of the attribute in the syntax
class. Consider the following code:

@schemeblock[
(define-syntax-class quark
  (pattern (a b ...)))
(syntax-parse some-term
  [(x (y:quark ...) ... z:quark)
   some-code])
]

The syntax class @scheme[quark] exports two attributes: @scheme[a] at
depth 0 and @scheme[b] at depth 1. The @scheme[syntax-parse] pattern
has three pattern variables: @scheme[x] at depth 0, @scheme[y] at
depth 2, and @scheme[z] at depth 0. Since @scheme[x] and @scheme[y]
are annotated with the @scheme[quark] syntax class, the pattern also
binds the following nested attributes: @scheme[y.a] at depth 2,
@scheme[y.b] at depth 3, @scheme[z.a] at depth 0, and @scheme[z.b] at
depth 1.

An attribute's ellipsis nesting depth is @emph{not} a guarantee that
its value has that level of list nesting. In particular, @scheme[~or]
and @scheme[~optional] patterns may result in attributes with fewer
than expected levels of list nesting.

@(myexamples
  (syntax-parse #'(1 2 3)
    [(~or (x:id ...) _)
     (attribute x)]))

@;{----------}

@section{Syntax Classes}

Syntax classes provide an abstraction mechanism for the specification
of syntax. Built-in syntax classes are supplied that recognize basic
classes such as @scheme[identifier] and @scheme[keyword].
Programmers can compose basic syntax classes to build specifications
of more complex syntax, such as lists of distinct identifiers and
formal arguments with keywords. Macros that manipulate the same
syntactic structures can share syntax class definitions. The structure
of syntax classes and patterns also allows @scheme[syntax-parse] to
automatically generate error messages for syntax errors.

When a syntax class accepts (matches) a syntax object, it computes and
provides attributes based on the contents of the matched syntax. While
the values of the attributes depend on the matched syntax, the set of
attributes and each attribute's ellipsis nesting depth is fixed for
each syntax class.

@defform*/subs[#:literals (pattern basic-syntax-class)
               [(define-syntax-class name-id stxclass-option ...
                  stxclass-variant ...+)
                (define-syntax-class (name-id arg-id ...) stxclass-option ... 
                  stxclass-variant ...+)]
               ([stxclass-option
                 (code:line #:attributes (attr-arity-decl ...))
                 (code:line #:description description)
                 (code:line #:transparent)
                 (code:line #:literals (literal-entry ...))
                 (code:line #:literal-sets (literal-set ...))
                 (code:line #:conventions (convention-id ...))]
                [attr-arity-decl
                 attr-name-id
                 (attr-name-id depth)]
                [stxclass-variant
                 (pattern syntax-pattern stxclass-pattern-directive ...)])]{

Defines @scheme[name-id] as a syntax class. When the @scheme[arg-id]s
are present, they are bound as variables (not pattern variables) in
the body. The body of the syntax-class definition contains a non-empty
sequence of @scheme[pattern] variants.

@specsubform[(code:line #:attributes (attr-arity-decl ...))]{

Declares the attributes of the syntax class. An attribute arity
declaration consists of the attribute name and optionally its ellipsis
depth (zero if not explicitly specified).

If the attributes are not explicitly listed, they are inferred as the
set of all pattern variables occurring in every variant of the syntax
class. Pattern variables that occur at different ellipsis depths are
not included. Only nested attributes from previously declared syntax
classes are included.
}

@specsubform[(code:line #:description description)]{

The @scheme[description] argument is an expression (evaluated in a
scope containing the syntax class's parameters) that should evaluate
to a string. It is used in error messages involving the syntax
class. For example, if a term is rejected by the syntax class, an
error of the form @schemevalfont{"expected @scheme[description]"} may
be synthesized.

If absent, the name of the syntax class is used instead.

}

@specsubform[#:transparent]{

Indicates that errors may be reported with respect to the internal
structure of the syntax class.
}

@specsubform[(code:line #:literals (literal-entry))]
@specsubform[(code:line #:literal-sets (literal-set ...))]
@specsubform[(code:line #:conventions (convention-id ...))]{

Declares the literals and conventions that apply to the syntax class's
variant patterns and their immediate @scheme[#:with] clauses. Patterns
occuring within subexpressions of the syntax class (for example, on
the right-hand side of a @scheme[#:fail-when] clause) are not
affected.

These options have the same meaning as under @scheme[syntax-parse].
}

@specsubform/subs[#:literals (pattern)
                  (pattern syntax-pattern stxclass-pattern-directive ...)
                  ([stxclass-pattern-directive
                    pattern-directive
                    (code:line #:rename internal-id external-id)])]{

Accepts syntax matching the given syntax pattern with the accompanying
pattern directives as in @scheme[syntax-parse].

The attributes of the variant are the attributes of the pattern
together with all attributes bound by @scheme[#:with] clauses,
including nested attributes produced by syntax classes associated with
the pattern variables.
}
}

@defform*/subs[#:literals (pattern)
               [(define-splicing-syntax-class name-id stxclass-option ...
                  stxclass-variant ...+)
                (define-splicing-syntax-class (name-id arg-id ...) stxclass-option ... 
                  stxclass-variant ...+)]
               ()]{

Defines @scheme[name-id] as a splicing syntax class. A splicing syntax
class encapsulates @tech{H-patterns} as an ordinary syntax class
encapsulates @tech{S-patterns}.

}

@defidform[pattern]{

Keyword recognized by @scheme[define-syntax-class]. It may not be
used as an expression.
}

@subsection{Attributes}

A syntax class has a set of @tech{attributes}. Each attribute has a
name, an ellipsis depth, and a set of nested attributes. When an
instance of the syntax class is parsed and bound to a pattern
variable, additional pattern variables are bound for each of the
syntax class's attributes. The name of these additional pattern
variables is the dotted concatenation of the primary pattern
variable with the name of the attribute.

For example, if pattern variable @scheme[p] is bound to an instance of
a syntax class with attribute @scheme[a], then the pattern variable
@scheme[p.a] is bound to the value of that attribute. The ellipsis
depth of @scheme[p.a] is the sum of the depths of @scheme[p] and
attribute @scheme[a].

The attributes of a syntax class are either given explicitly with an
@scheme[#:attributes] option or inferred from the pattern variables of
the syntax class's variants.


@subsection{Inspection tools}

The following special forms are for debugging syntax classes.

@defform[(syntax-class-attributes syntax-class-id)]{

Returns a list of the syntax class's attributes in flattened
form. Each attribute is listed by its name and ellipsis depth.
}

@defform[(syntax-class-parse syntax-class-id stx-expr arg-expr ...)]{

Runs the parser for the syntax class (parameterized by the
@scheme[arg-expr]s) on the syntax object produced by
@scheme[stx-expr]. On success, the result is a list of vectors
representing the attribute bindings of the syntax class. Each vector
contains the attribute name, depth, and associated value. On failure,
the result is some internal representation of the failure.
}


@;{----------}

@section{Literal sets and Conventions}

Sometimes the same literals are recognized in a number of different
places. The most common example is the literals for fully expanded
programs, which are used in many analysis and transformation
tools. Specifying literals individually is burdensome and error-prone.
As a remedy, @schememodname[syntax/parse] offers @deftech{literal
sets}. A literal set is defined via @scheme[define-literal-set] and
used via the @scheme[#:literal-set] option of @scheme[syntax-parse].

@defform/subs[(define-literal-set name-id (literal ...))
              ([literal literal-id
                        (pattern-id literal-id)])]{

Defines @scheme[name] as a @tech{literal set}. Each @scheme[literal]
can have a separate @scheme[pattern-id] and @scheme[literal-id]. The
@scheme[pattern-id] determines what identifiers in the pattern are
treated as literals. The @scheme[literal-id] determines what
identifiers the literal matches.

@myexamples[
(define-literal-set def-litset
  (define-values define-syntaxes))
(syntax-parse #'(define-syntaxes (x) 12)
  #:literal-sets (def-litset)
  [(define-values (x:id ...) e:expr) 'v]
  [(define-syntaxes (x:id ...) e:expr) 's])
]

}

@defform/subs[(define-conventions name-id (id-pattern syntax-class) ...)
              ([name-pattern exact-id
                             name-rx]
               [syntax-class syntax-class-id
                             (syntax-class-id expr ...)])]{

Defines @deftech{conventions} that supply default syntax classes for
pattern variables. A pattern variable that has no explicit syntax
class is checked against each @scheme[id-pattern], and the first one
that matches determines the syntax class for the pattern. If no
@scheme[id-pattern] matches, then the pattern variable has no syntax
class.

@myexamples[
(define-conventions xyz-as-ids
  [x id] [y id] [z id])
(syntax-parse #'(a b c 1 2 3)
  #:conventions (xyz-as-ids)
  [(x ... n ...) (syntax->datum #'(x ...))])
(define-conventions xn-prefixes
  [#rx"^x" id]
  [#rx"^n" nat])
(syntax-parse #'(a b c 1 2 3)
  #:conventions (xn-prefixes)
  [(x0 x ... n0 n ...)
   (syntax->datum #'(x0 (x ...) n0 (n ...)))])
]

}

@;{----------}

@section[#:tag "lib"]{Library syntax classes and literal sets}

@subsection{Syntax classes}

@(begin
   (define-syntax-rule (defstxclass name . pre-flows)
     (defidform name . pre-flows))
   (define-syntax-rule (defstxclass* (name arg ...) . pre-flows)
     (defform (name arg ...) . pre-flows)))

@defstxclass[expr]{

Matches anything except a keyword literal (to distinguish expressions
from the start of a keyword argument sequence). The term is not
otherwise inspected, and no guarantee is made that the term is
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

@defform[(static predicate description)]{

Matches an identifier that is bound in the syntactic environment to
static information (see @scheme[syntax-local-value]) satisfying the
given @scheme[predicate]. If the term does not match, the
@scheme[description] argument is used to describe the expected syntax.

When used outside of the dynamic extend of a macro transformer (see
@scheme[syntax-transforming?]), matching fails.

The attribute @var[value] contains the value the name is bound to.
}

@defform[(atom-in-list atoms description)]{

Matches a syntax object whose inner datum is @scheme[eqv?] to some
atom in the given list.

Use @scheme[atom-in-list] instead of a literals list when recognizing
identifier based on their symbolic names rather than their bindings.

}


@subsection{Literal sets}

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
