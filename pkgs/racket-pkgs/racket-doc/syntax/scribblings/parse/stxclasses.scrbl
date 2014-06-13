#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@title[#:tag "stxparse-specifying"]{Specifying Syntax with Syntax Classes}

@declare-exporting[syntax/parse]

Syntax classes provide an abstraction mechanism for @tech{syntax
patterns}. Built-in syntax classes are supplied that recognize basic
classes such as @racket[identifier] and @racket[keyword].  Programmers
can compose basic syntax classes to build specifications of more
complex syntax, such as lists of distinct identifiers and formal
arguments with keywords. Macros that manipulate the same syntactic
structures can share syntax class definitions.

@defform*[#:literals (pattern)
          [(define-syntax-class name-id stxclass-option ...
             stxclass-variant ...+)
           (define-syntax-class (name-id . kw-formals) stxclass-option ... 
             stxclass-variant ...+)]
          #:grammar
          ([stxclass-option
            (code:line #:attributes (attr-arity-decl ...))
            (code:line #:description description-expr)
            (code:line #:opaque)
            (code:line #:commit)
            (code:line #:no-delimit-cut)
            (code:line #:literals (literal-entry ...))
            (code:line #:datum-literals (datum-literal-entry ...))
            (code:line #:literal-sets (literal-set ...))
            (code:line #:conventions (convention-id ...))
            (code:line #:local-conventions (convention-rule ...))
            (code:line #:disable-colon-notation)]
           [attr-arity-decl
            attr-name-id
            (attr-name-id depth)]
           [stxclass-variant
            (pattern syntax-pattern pattern-directive ...)])
          #:contracts ([description-expr (or/c string? #f)])]{

Defines @racket[name-id] as a @deftech{syntax class}, which
encapsulates one or more @tech{single-term patterns}.

A syntax class may have formal parameters, in which case they are
bound as variables in the body. Syntax classes support optional
arguments and keyword arguments using the same syntax as
@racket[lambda]. The body of the syntax-class definition contains a
non-empty sequence of @racket[pattern] variants.

The following options are supported:

@specsubform[(code:line #:attributes (attr-arity-decl ...))
             #:grammar
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

@specsubform[(code:line #:description description-expr)
             #:contracts ([description-expr (or/c string? #f)])]{

The @racket[description] argument is evaluated in a scope containing
the syntax class's parameters. If the result is a string, it is used
in error messages involving the syntax class. For example, if a term
is rejected by the syntax class, an error of the form
@racketvalfont{"expected @racket[description]"} may be synthesized. If
the result is @racket[#f], the syntax class is skipped in the search
for a description to report.

If the option is not given, the name of the syntax class is
used instead.
}

@specsubform[#:opaque]{

Indicates that errors should not be reported with respect to the
internal structure of the syntax class.
}

@specsubform[#:commit]{

Directs the syntax class to ``commit'' to the first successful
match. When a variant succeeds, all choice points within the syntax
class are discarded. See also @racket[~commit].
}

@specsubform[#:no-delimit-cut]{

By default, a cut (@racket[~!]) within a syntax class only discards
choice points within the syntax class. That is, the body of the syntax
class acts as though it is wrapped in a @racket[~delimit-cut] form. If
@racket[#:no-delimit-cut] is specified, a cut may affect choice points
of the syntax class's calling context (another syntax class's patterns
or a @racket[syntax-parse] form).

It is an error to use both @racket[#:commit] and
@racket[#:no-delimit-cut].
}

@specsubform[(code:line #:literals (literal-entry ...))]
@specsubform[(code:line #:datum-literals (datum-literal-entry ...))]
@specsubform[(code:line #:literal-sets (literal-set ...))]
@specsubform[(code:line #:conventions (convention-id ...))]{

Declares the literals and conventions that apply to the syntax class's
variant patterns and their immediate @racket[#:with] clauses. Patterns
occuring within subexpressions of the syntax class (for example, on
the right-hand side of a @racket[#:fail-when] clause) are not
affected.

These options have the same meaning as in @racket[syntax-parse].
}

Each variant of a syntax class is specified as a separate
@racket[pattern]-form whose syntax pattern is a @tech{single-term
pattern}.
}

@defform*[#:literals (pattern)
          [(define-splicing-syntax-class name-id stxclass-option ...
             stxclass-variant ...+)
           (define-splicing-syntax-class (name-id kw-formals) stxclass-option ... 
             stxclass-variant ...+)]]{

Defines @racket[name-id] as a @deftech{splicing syntax class},
analogous to a @tech{syntax class} but encapsulating @tech{head
patterns} rather than @tech{single-term patterns}.

The options are the same as for @racket[define-syntax-class].

Each variant of a splicing syntax class is specified as a separate
@racket[pattern]-form whose syntax pattern is a @tech{head pattern}.
}

@defform[#:literals (pattern)
         (pattern syntax-pattern pattern-directive ...)]{

Used to indicate a variant of a syntax class or splicing syntax
class. The variant accepts syntax matching the given syntax pattern
with the accompanying @tech{pattern directives}.

When used within @racket[define-syntax-class], @racket[syntax-pattern]
should be a @tech{single-term pattern}; within
@racket[define-splicing-syntax-class], it should be a @tech{head
pattern}.

The attributes of the variant are the attributes of the pattern
together with all attributes bound by @racket[#:with] clauses,
including nested attributes produced by syntax classes associated with
the pattern variables.
}

@;{--------}

@section{Pattern Directives}

@section-index{pattern-directive}

Both the parsing forms and syntax class definition forms support
@deftech{pattern directives} for annotating syntax patterns and
specifying side conditions. The grammar for pattern directives
follows:

@racketgrammar[pattern-directive
               (code:line #:declare pattern-id stxclass maybe-role)
               (code:line #:with syntax-pattern expr)
               (code:line #:attr attr-arity-decl expr)
               (code:line #:fail-when condition-expr message-expr)
               (code:line #:fail-unless condition-expr message-expr)
               (code:line #:when condition-expr)
               (code:line #:do [def-or-expr ...])]

@specsubform[(code:line #:declare pvar-id stxclass maybe-role)
             #:grammar
             ([stxclass syntax-class-id
                        (syntax-class-id arg ...)]
              [maybe-role (code:line)
                          (code:line #:role role-expr)])]{

Associates @racket[pvar-id] with a syntax class and possibly a role,
equivalent to replacing each occurrence of @racket[pvar-id] in the
pattern with @racket[(~var pvar-id stxclass maybe-role)].
The second form of @racket[stxclass] allows the use of parameterized
syntax classes, which cannot be expressed using the ``colon''
notation. The @racket[arg]s are evaluated in the scope where the
@racket[pvar-id] occurs in the pattern. Keyword arguments are
supported, using the same syntax as in @racket[#%app].

If a @racket[#:with] directive appears between the main pattern (e.g., in a 
@racket[syntax-parse] or @racket[define-syntax-class] clause) and a 
@racket[#:declare], then only pattern variables from the @racket[#:with] 
pattern may be declared.

@myexamples[
(syntax-parse #'P
  [x
   #:declare x id
   #'x])
(syntax-parse #'L
  [x
   #:with y #'x
   #:declare x id
   #'x])
(syntax-parse #'T
  [x
   #:with y #'x
   #:declare y id
   #'x])
]
}

@specsubform[(code:line #:with syntax-pattern stx-expr)]{

Evaluates the @racket[stx-expr] in the context of all previous
attribute bindings and matches it against the pattern. If the match
succeeds, the pattern's attributes are added to environment for the
evaluation of subsequent side conditions. If the @racket[#:with] match
fails, the matching process backtracks. Since a syntax object may
match a pattern in several ways, backtracking may cause the same
clause to be tried multiple times before the next clause is reached.

If the value of @racket[stx-expr] is not a syntax object, it is
implicitly converted to a syntax object. If the the conversion would
produce @deftech{3D syntax}---that is, syntax that contains unwritable
values such as procedures, non-prefab structures, etc---then an
exception is raised instead.
}

@specsubform[(code:line #:attr attr-arity-decl expr)]{

Evaluates the @racket[expr] in the context of all previous attribute
bindings and binds it to the given attribute. The value of
@racket[expr] need not be, or even contain, syntax---see
@racket[attribute] for details.
}

@specsubform[(code:line #:fail-when condition-expr message-expr)
             #:contracts ([message-expr (or/c string? #f)])]{

Evaluates the @racket[condition-expr] in the context of all previous
attribute bindings. If the value is any true value (not @racket[#f]),
the matching process backtracks (with the given message); otherwise,
it continues. If the value of the condition expression is a syntax
object, it is indicated as the cause of the error.

If the @racket[message-expr] produces a string it is used as the
failure message; otherwise the failure is reported in terms of the
enclosing descriptions.
}

@specsubform[(code:line #:fail-unless condition-expr message-expr)
             #:contracts ([message-expr (or/c string? #f)])]{

Like @racket[#:fail-when] with the condition negated.
}

@specsubform[(code:line #:when condition-expr)]{

Evaluates the @racket[condition-expr] in the context of all previous
attribute bindings. If the value is @racket[#f], the matching process
backtracks. In other words, @racket[#:when] is like
@racket[#:fail-unless] without the message argument.
}

@specsubform[(code:line #:do [def-or-expr ...])]{

Takes a sequence of definitions and expressions, which may be
intermixed, and evaluates them in the scope of all previous attribute
bindings. The names bound by the definitions are in scope in
the expressions of subsequent patterns and clauses.

There is currently no way to bind attributes using a @racket[#:do]
block. It is an error to shadow an attribute binding with a definition
in a @racket[#:do] block.
}


@;{----------}

@section[#:tag "stxparse-attrs"]{Pattern Variables and Attributes}

An @deftech{attribute} is a name bound by a syntax pattern. An
attribute can be a @tech{pattern variable} itself, or it can be a
@tech{nested attribute} bound by an @tech{annotated pattern
variable}. The name of a nested attribute is computed by concatenating
the pattern variable name with the syntax class's exported attribute's
name, separated by a dot (see the example below).

Attributes can be used in two ways: with the @racket[attribute] form,
and inside syntax templates via @racket[syntax], @racket[quasisyntax],
etc. Attribute names cannot be used directly as expressions; that is,
attributes are not variables.

A @deftech{syntax-valued attribute} is an attribute whose value is a
syntax object, a syntax list of the appropriate @tech{ellipsis depth},
or a tree containing @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{promises} that when
completely forced produces a suitable syntax object or syntax
list. Syntax-valued attributes can be used within @racket[syntax],
@racket[quasisyntax], etc as part of a syntax template. If an
attribute is used inside a syntax template but it is not
syntax-valued, an error is signaled.

The value of an attribute is not required to be syntax.
Non-syntax-valued attributes can be used to return a parsed
representation of a subterm or the results of an analysis on the
subterm. A non-syntax-valued attribute should be bound using the
@racket[#:attr] directive or a @racket[~bind] pattern; @racket[#:with]
and @racket[~parse] will convert the right-hand side to a (possibly
3D) syntax object.

@myexamples[
(define-syntax-class table
  (pattern ((key value) ...)
           #:attr hashtable
                  (for/hash ([k (syntax->datum #'(key ...))]
                             [v (syntax->datum #'(value ...))])
                    (values k v))
           #:attr [sorted-kv 1]
                  (delay
                   (printf "sorting!\n")
                   (sort (syntax->list #'((key value) ...))
                         <
                         #:key (lambda (kv) (cadr (syntax->datum kv)))))))
]

The @racket[table] syntax class provides four attributes:
@racket[key], @racket[value], @racket[hashtable], and
@racket[sorted-kv]. The @racket[hashtable] attribute has
@tech{ellipsis depth} 0 and the rest have depth 1; all but
@racket[hashtable] are syntax-valued. The @racket[sorted-kv]
attribute's value is a promise; it will be automatically forced if
used in a syntax template.

Syntax-valued attributes can be used in syntax templates:

@myinteraction[
(syntax-parse #'((a 3) (b 2) (c 1))
  [t:table
   #'(t.key ...)])
(syntax-parse #'((a 3) (b 2) (c 1))
  [t:table
   #'(t.sorted-kv ...)])]

But non-syntax-valued attributes cannot:

@myinteraction[
(syntax-parse #'((a 3) (b 2) (c 1))
  [t:table
   #'t.hashtable])
]

Use the @racket[attribute] form to get the value of an attribute
(syntax-valued or not).

@myinteraction[
(syntax-parse #'((a 1) (b 2) (c 3))
  [t:table
   (attribute t.hashtable)])
(syntax-parse #'((a 3) (b 2) (c 1))
  [t:table
   (attribute t.sorted-kv)])
]

Every attribute has an associated @deftech{ellipsis depth} that
determines how it can be used in a syntax template (see the discussion
of ellipses in @racket[syntax]). For a pattern variable, the ellipsis
depth is the number of ellipses the pattern variable ``occurs under''
in the pattern. An attribute bound by @racket[#:attr] has depth 0
unless declared otherwise. For a nested attribute the depth is the sum
of the annotated pattern variable's depth and the depth of the
attribute exported by the syntax class.

Consider the following code:

@racketblock[
(define-syntax-class quark
  (pattern (a b ...)))
(syntax-parse some-term
  [(x (y:quark ...) ... z:quark)
   some-code])
]

The syntax class @racket[quark] exports two attributes: @racket[a] at
depth 0 and @racket[b] at depth 1. The @racket[syntax-parse] pattern
has three pattern variables: @racket[x] at depth 0, @racket[y] at
depth 2, and @racket[z] at depth 0. Since @racket[x] and @racket[y]
are annotated with the @racket[quark] syntax class, the pattern also
binds the following nested attributes: @racket[y.a] at depth 2,
@racket[y.b] at depth 3, @racket[z.a] at depth 0, and @racket[z.b] at
depth 1.

An attribute's ellipsis nesting depth is @emph{not} a guarantee that
it is syntax-valued. In particular, @racket[~or] and
@racket[~optional] patterns may result in attributes with fewer than
expected levels of list nesting, and @racket[#:attr] and
@racket[~bind] can be used to bind attributes to arbitrary values.

@myexamples[
(syntax-parse #'(a b 3)
  [(~or (x:id ...) _)
   (attribute x)])
]

@defform[(attribute attr-id)]{

Returns the value associated with the @tech{attribute} named
@racket[attr-id]. If @racket[attr-id] is not bound as an attribute, an
error is raised.
}
