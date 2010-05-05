#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          scheme/sandbox
          (for-label scheme/base
                     scheme/contract
                     (except-in syntax/parse ...+)
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
(module example racket/base
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

@bold{Note: } Remember not to put a @scheme[syntax-case] style
literals list between the syntax argument and the clauses!

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
(module example-syntax racket/base
  (require syntax/parse)
  (provide binding)
  (define-syntax-class binding
    #:attributes (x e)
    (pattern (x:id e:expr))))

(module example racket/base
  (require (for-syntax racket/base
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
(module example racket/base
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

@margin-note{The @scheme[(require (for-template scheme/base))] is
needed for the @scheme[quote] expression. If the syntax class
definition were a local definition in the same module, the
@scheme[for-template] would be unnecessary.}
@;
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

The second pattern matches unparenthesized identifiers. The @scheme[e]
attribute is bound using a @scheme[#:with] clause, which matches the
pattern @scheme[e] against the syntax from evaluating @scheme[#'#f].

Optional keyword arguments are supported via @tech{head
patterns}. Unlike normal patterns, which match one term, head patterns
can match a variable number of subterms in a list.

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

@section{Parsing and classifying syntax}

This section describes @schememodname[syntax/parse]'s facilities for
parsing and classifying syntax. These facilities use a common language
of @tech{syntax patterns}, which is described in detail in the next
section, @secref{syntax-patterns}.

@subsection{Parsing syntax}

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
              #:contracts ([stx-expr syntax?])]{

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
                            (pattern-id literal-id #:phase phase-expr)])]{
@margin-note{
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
                                       (code:line #:phase phase-expr)])]{

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

@;{----------}

@subsection{Classifying syntax}

Syntax classes provide an abstraction mechanism for @tech{syntax
patterns}. Built-in syntax classes are supplied that recognize basic
classes such as @scheme[identifier] and @scheme[keyword].  Programmers
can compose basic syntax classes to build specifications of more
complex syntax, such as lists of distinct identifiers and formal
arguments with keywords. Macros that manipulate the same syntactic
structures can share syntax class definitions.

@defform*/subs[#:literals (pattern)
               [(define-syntax-class name-id stxclass-option ...
                  stxclass-variant ...+)
                (define-syntax-class (name-id arg-id ...) stxclass-option ... 
                  stxclass-variant ...+)]
               ([stxclass-option
                 (code:line #:attributes (attr-arity-decl ...))
                 (code:line #:description description-expr)
                 (code:line #:opaque)
                 (code:line #:literals (literal-entry ...))
                 (code:line #:literal-sets (literal-set ...))
                 (code:line #:conventions (convention-id ...))
                 (code:line #:local-conventions (convention-rule ...))]
                [attr-arity-decl
                 attr-name-id
                 (attr-name-id depth)]
                [stxclass-variant
                 (pattern syntax-pattern pattern-directive ...)])]{

Defines @scheme[name-id] as a @deftech{syntax class}, which
encapsulates one or more @tech{single-term patterns}.

When the @scheme[arg-id]s are present, they are bound as variables in
the body. The body of the syntax-class definition contains a non-empty
sequence of @scheme[pattern] variants.

The following options are supported:

@specsubform/subs[(code:line #:attributes (attr-arity-decl ...))
                  ([attr-arity-decl attr-id
                                    (attr-id depth)])]{

Declares the attributes of the syntax class. An attribute arity
declaration consists of the attribute name and optionally its ellipsis
depth (zero if not explicitly specified).

If the attributes are not explicitly listed, they are inferred as the
set of all @tech{pattern variables} occurring in every variant of the
syntax class. Pattern variables that occur at different ellipsis
depths are not included, nor are nested attributes from
@tech{annotated pattern variables}.
}

@specsubform[(code:line #:description description-expr)]{

The @scheme[description] argument is an expression (evaluated in a
scope containing the syntax class's parameters) that should evaluate
to a string. It is used in error messages involving the syntax
class. For example, if a term is rejected by the syntax class, an
error of the form @schemevalfont{"expected @scheme[description]"} may
be synthesized.

If absent, the name of the syntax class is used instead.
}

@specsubform[#:opaque]{

Indicates that errors should not be reported with respect to the
internal structure of the syntax class.
}

@specsubform[(code:line #:literals (literal-entry))]
@specsubform[(code:line #:literal-sets (literal-set ...))]
@specsubform[(code:line #:conventions (convention-id ...))]{

Declares the literals and conventions that apply to the syntax class's
variant patterns and their immediate @scheme[#:with] clauses. Patterns
occuring within subexpressions of the syntax class (for example, on
the right-hand side of a @scheme[#:fail-when] clause) are not
affected.

These options have the same meaning as in @scheme[syntax-parse].
}

Each variant of a syntax class is specified as a separate
@scheme[pattern]-form whose syntax pattern is a @tech{single-term
pattern}.
}

@defform*[#:literals (pattern)
          [(define-splicing-syntax-class name-id stxclass-option ...
             stxclass-variant ...+)
           (define-splicing-syntax-class (name-id arg-id ...) stxclass-option ... 
             stxclass-variant ...+)]]{

Defines @scheme[name-id] as a @deftech{splicing syntax class},
analogous to a @tech{syntax class} but encapsulating @tech{head
patterns} rather than @tech{single-term patterns}.

The options are the same as for @scheme[define-syntax-class].

Each variant of a splicing syntax class is specified as a separate
@scheme[pattern]-form whose syntax pattern is a @tech{head pattern}.
}

@defform[#:literals (pattern)
         (pattern syntax-pattern pattern-directive ...)]{

Used to indicate a variant of a syntax class or splicing syntax
class. The variant accepts syntax matching the given syntax pattern
with the accompanying @tech{pattern directives}.

When used within @scheme[define-syntax-class], @scheme[syntax-pattern]
should be a @tech{single-term pattern}; within
@scheme[define-splicing-syntax-class], it should be a @tech{head
pattern}.

The attributes of the variant are the attributes of the pattern
together with all attributes bound by @scheme[#:with] clauses,
including nested attributes produced by syntax classes associated with
the pattern variables.
}

@;{--------}

@subsection{Pattern directives}

Both the parsing forms and syntax class definition forms support
@deftech{pattern directives} for annotating syntax patterns and
specifying side conditions. The grammar for pattern directives
follows:

@schemegrammar[pattern-directive
               (code:line #:declare pattern-id syntax-class-id)
               (code:line #:declare pattern-id (syntax-class-id expr ...))
               (code:line #:with syntax-pattern expr)
               (code:line #:attr attr-id expr)
               (code:line #:fail-when condition-expr message-expr)
               (code:line #:fail-unless condition-expr message-expr)
               (code:line #:when condition-expr)]

@specsubform[(code:line #:declare pvar-id syntax-class-id)]
@specsubform[(code:line #:declare pvar-id (syntax-class-id expr ...))]{

The first form is equivalent to using the
@svar[pvar-id:syntax-class-id] form in the pattern (but it is illegal
to use both for the same pattern variable).

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

@specsubform[(code:line #:fail-when condition-expr message-expr)]{

Evaluates the @scheme[condition-expr] in the context of all previous
attribute bindings. If the value is any true value (not @scheme[#f]),
the matching process backtracks (with the given message); otherwise,
it continues. If the value of the condition expression is a syntax
object, it is indicated as the cause of the error.
}

@specsubform[(code:line #:fail-unless condition-expr message-expr)]{

Like @scheme[#:fail-when] with the condition negated.
}

@specsubform[(code:line #:when condition-expr)]{

Evaluates the @scheme[condition-expr] in the context of all previous
attribute bindings. If the value is @scheme[#f], the matching process
backtracks. In other words, @scheme[#:when] is like
@scheme[#:fail-unless] without the message argument.

}


@;{----------}

@subsection{Pattern variables and attributes}

An @deftech{attribute} is a name bound by a syntax pattern. An
attribute can be a @tech{pattern variable} itself, or it can be a
@tech{nested attribute} bound by an @tech{annotated pattern
variable}. The name of a nested attribute is computed by concatenating
the pattern variable name with the syntax class's exported attribute's
name, separated by a dot (see the example below).

Attribute names cannot be used directly as expressions; that is,
attributes are not variables. Instead, an attribute's value can be
gotten using the @scheme[attribute] special form.

@defform[(attribute attr-id)]{

Returns the value associated with the attribute named
@scheme[attr-id]. If @scheme[attr-id] is not bound as an attribute, an
error is raised.
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


@;{--------}

@subsection{Inspection tools}

The following special forms are for debugging syntax classes.

@defform[(syntax-class-attributes syntax-class-id)]{

Returns a list of the syntax class's attributes. Each attribute is
listed by its name and ellipsis depth.
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

@include-section["parse-patterns.scrbl"]


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

The literals in a literal set always refer to the phase-0 bindings of
the enclosing module. For example:

@myexamples[
(module common racket/base
  (define x 'something)
  (provide x))

(module lits racket/base
  (require syntax/parse 'common)
  (define-literal-set common-lits (x))
  (provide common-lits))
]

In the literal set @scheme[common-lits], the literal @scheme[x] always
recognizes identifiers bound to the variable @scheme[x] defined in
module @schememodname['common].

When a literal set is used with the @scheme[#:phase phase-expr]
option, the literals' fixed bindings are compared against the binding of
the input literal at the specified phase. Continuing the example:

@myexamples[
(require syntax/parse 'lits (for-syntax 'common))
(syntax-parse #'x #:literal-sets ([common-lits #:phase 1])
  [x 'yes]
  [_ 'no])
]

The occurrence of @scheme[x] in the pattern matches any identifier
whose binding at phase 1 is the @scheme[x] from module
@schememodname['common].
}

@defform/subs[(define-conventions name-id convention-rule ...)
              ([convention-rule (name-pattern syntax-class)]
               [name-pattern exact-id
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

Local conventions, introduced with the @scheme[#:local-conventions]
keyword argument of @scheme[syntax-parse] and syntax class
definitions, may refer to local bindings:

@myexamples[
(define-syntax-class (nat> bound)
  (pattern n:nat
           #:fail-unless (> (syntax-e #'n) bound)
                         (format "expected number > ~s" bound)))

(define-syntax-class (natlist> bound)
  #:local-conventions ([N (nat> bound)])
  (pattern (N ...)))

(define (parse-natlist> bound x)
  (syntax-parse x
    #:local-conventions ([NS (natlist> bound)])
    [NS 'ok]))
(parse-natlist> 0 #'(1 2 3))
(parse-natlist> 5 #'(8 6 4 2))
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

When used outside of the dynamic extent of a macro transformer (see
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
