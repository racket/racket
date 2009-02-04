#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          (for-label scheme/base 
                     stxclass 
                     stxclass/util))

@(define ellipses @scheme[...])
@(define (TODO . pre-flow)
   (make-splice
    (cons (bold "TODO: ")
          (decode-content pre-flow))))

@title{Parsing Syntax and Syntax Classes}

@defmodule[stxclass]

@bold{Warning: This library is still very volatile! Its interface and
behavior are subject to frequent change. I highly recommend that you
avoid creating PLaneT packages that depend on this library.}

@section{Parsing Syntax}
@declare-exporting[stxclass]

@defform/subs[(syntax-parse stx-expr maybe-literals clause ...)
              ([maybe-literals code:blank
                               (code:line #:literals (literal-id ...))]
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

@TODO{Allow literal declarations of form @scheme[(_internal-name
_external-name)].}
}

@defform[(syntax-parser maybe-literals clause ...)]{

Like @scheme[syntax-parse], but produces a matching procedure. The
procedure accepts a single argument, which should be a syntax object.

}

The grammar of patterns accepted by @scheme[syntax-parse] and
@scheme[syntax-parser] follows:

@schemegrammar*[#:literals (_ ...*)
                [syntax-pattern
                 pvar-id
                 pvar-id:syntax-class-id
                 literal-id
                 atomic-datum
                 (syntax-pattern . syntax-pattern)
                 #|  (syntax-splice-pattern . syntax-pattern)  |#
                 (syntax-pattern #,ellipses . syntax-pattern)
                 ((head ...+) ...* . syntax-pattern)]
                #| [syntax-splice-pattern
                    pvar-id:syntax-splice-class-id] |#
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

}
@specsubform[atomic-datum]{

The empty list, numbers, strings, booleans, and keywords match as
literals.

}
@specsubform[(syntax-pattern . syntax-pattern)]{

Matches a syntax pair whose head matches the first pattern and whose
tail matches the second.

}
@;{
@specsubform[(syntax-splice-pattern . syntax-pattern)]{

Matches a syntax object which consists of any sequence of syntax
objects matching the splice pattern followed by a tail matching the
given tail pattern.

}
}

@specsubform[(syntax-pattern #,ellipses . syntax-pattern)]{

Matches a sequence of the first pattern ending in a tail matching the
second pattern.

That is, the sequence pattern matches either the second pattern (which
need not be a list) or a pair whose head matches the first pattern and
whose tail recursively matches the whole sequence pattern.

}
@specsubform/subs[#:literals (...*)
                  ((head ...+) ...* . syntax-pattern)
                  ([head
                    (code:line (syntax-pattern ...+) head-directive ...)]
                   [head-directive
                    (code:line #:min min-reps)
                    (code:line #:max max-reps)
                    (code:line #:mand)
                    #| (code:line #:opt)
                       (code:line #:occurs occurs-pvar-id)
                       (code:line #:default default-form)
                    |#])]{

Matches a sequence of any combination of the heads ending in a tail
matching the final pattern. The match is subject to constraints
specified on the heads.

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
@;{
@specsubform[#:opt]{

(Probably a bad idea.)

}
}
}
@;{
The variants of @scheme[_syntax-splice-pattern] follow:

@specsubform[pvar-id:syntax-splice-class-id]{

Matches a sequence of syntax objects described by
@scheme[_syntax-splice-class-id].

The name @scheme[_pvar-id] is bound, but not allowed within
expressions or @scheme[syntax] templates (since it does not refer to a
particular syntax object). Only the prefixed attributes of the splice
class are usable.
}
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

@defidform[...*]{

Keyword recognized by @scheme[syntax-parse] etc as notation for
generalized sequences. It may not be used as an expression.

}

@section{Syntax Classes}
@declare-exporting[stxclass]

Syntax classes provide an abstraction mechanism for the specification
of syntax. Basic syntax classes include @scheme[identifier] and
@scheme[keyword]. From these, programmers can build larger
specifications of syntax, such as @scheme[distinct-identifier-list]
and @scheme[keyword-formals]. Macros that manipulate the same
syntactic structures can share syntax class definitions. Syntax
classes are also the basis of error reporting in
@scheme[syntax-parse].

When a syntax class accepts (matches, includes) a syntax object, it
computes and provides attributes based on the contents of the matched
syntax. While the values of the attributes depend on the matched
syntax, the set of attributes and each attribute's ellipsis nesting
depth is fixed for each syntax class.

@defform*/subs[#:literals (pattern)
               [(define-syntax-class name-id stxclass-option ...
                  stxclass-variant ...+)
                (define-syntax-class (name-id arg-id ...) stxclass-option ... 
                  stxclass-variant ...+)]
               ([stxclass-options
                 (code:line #:description description)
                 (code:line #:transparent)]
                [stxclass-variant 
                 (pattern syntax-pattern pattern-directive ...)])]{

Defines @scheme[name-id] as a syntax class. When the @scheme[arg-id]s
are present, they are bound as variables (not pattern variables) in
the body.

The body of the syntax-class definition contains one or more variants
that specify the syntax it accepts and determines the attributes it
provides. The syntax class provides only those attributes which are
present in every variant. Each such attribute must be defined with the
same ellipsis nesting depth and the same sub-attributes in each
component.

@TODO{Eliminate attribute inference, require explicit attribute
declaration?}

@specsubform[(code:line #:description description)]{

The @scheme[description] argument is an expression (with the
syntax-class's parameters in scope) that should evaluate to a
string. It is used in error messages involving the syntax class. For
example, if a term is rejected by the syntax class, an error of the
form @scheme["expected <description>"] may be generated.

If absent, the name of the syntax class is used instead.

}

@specsubform[#:transparent]{

Indicates that errors may be reported with respect to the internal
structure of the syntax class.
}

@specsubform/subs[#:literals (pattern)
                  (pattern syntax-pattern stxclass-pattern-directive ...)
                  ([stxclass-pattern-directive
                    pattern-directive
                    (code:line #:rename internal-id external-id)])]{

Accepts syntax matching the given pattern with the accompanying
pattern directives as in @scheme[syntax-parse].

Provides an attribute for every pattern variable defined in the
pattern and the @scheme[#:with] clauses. The name of the attribute is
the symbolic name of the pattern variable, except when the name is
explicitly given via a @scheme[#:rename] clause. Pattern variables
declared with a syntax class yield derived pattern variables for that
syntax class's attributes. These are propagated as nested attributes.

@specsubform[(code:line #:rename internal-id external-id)]{

Exports the pattern variable binding named by @scheme[internal-id] as
the attribute named @scheme[external-id].

}
}

Within the syntax-class body, recursive references to the enclosing
syntax class and forward references to syntax classes defined in the
same scope are allowed. For the purpose of calculating provided
attributes, recursive and forward syntax-class references generate no
nested attributes. The full set of attributes is available, however,
to @scheme[#:with] and @scheme[#:when] expressions.

This treatment of recursive and forward references prevents infinitely
nested attributes.

@TODO{Eliminate attribute inference. Explicit attribute declaration
eliminates infinitely nested attributes just as well.}

}
@;{
@defform*[[(define-syntax-splice-class (name-id arg-id ...) stxclass-body)
           (define-syntax-splice-class name-id stxclass-body)]]{

Defines @scheme[name-id] as a syntax splice class. When the
@scheme[arg-id]s are present, they are bound as variables (not pattern
variables) in the body.

The @scheme[stxclass-body] is like the body of
@scheme[define-syntax-class], except that all patterns within it must
match only proper lists:

@schemegrammar[#:literals (... ...*)
               proper-list-pattern
               ()
               (syntax-pattern . proper-list-pattern)
               (syntax-splice-pattern . proper-list-pattern)
               (syntax-pattern ... . proper-list-pattern)
               ((head ...+) ...* . proper-list-pattern)]

}
}

@defidform[pattern]{

Keyword recognized by @scheme[define-syntax-class]. It may not be
used as an expression.
}

@defform[(define-basic-syntax-class (name-id arg-id ...)
           ([attr-id attr-depth] ...)
           parser-expr)]{

Defines @scheme[name] as a syntax class with the given
attributes. Each @scheme[attr-depth] must be a literal exact
nonnegative integer (most often @scheme[0]).

The @scheme[arg-id]s are placeholders only. They are not bound in any
part of the syntax-class body.

The @scheme[parser-expr] must evaluate to a procedure. This procedure
is used to parse or reject syntax objects. The arguments to the parser
procedure consist of the syntax object to parse followed by the
syntax-class parameterization arguments. To indicate success, the
parser should return a list of attribute values, one for each
attribute listed. (For example, a parser for a syntax class that
defines no attributes returns the empty list when it succeeds.) To
indicate failure, the parser procedure should return @scheme[#f].

The parser procedure is encouraged to avoid side-effects.

@TODO{Add support for better error reporting within basic syntax
class.}

@TODO{Remove this form in favor of new stxclass-variant form.}

}
@;{
@defform[(define-basic-syntax-class* (name-id arg-id ...)
           ([attr-id attr-depth] ...)
           parser-expr)]{

Like @scheme[define-basic-syntax-class], except that on success the
parser expression produces a list with an extra element consed onto
the attribute value list. This extra element is the transformed
matched syntax. When a pattern variable annotated with the syntax
class matches, it is bound to this syntax instead of the subterm of
the original syntax object.

Transforming matched syntax is useful for implementing syntax classes
that add interpretation or constraints to expressions, such as
@scheme[expr/c], which imposes run-time contracts on expressions.

}
}

@;{
@defform[(attrs-of syntax-class-id)]{

For debugging. Returns a representation of the attributes provided by
a syntax class.

}

@defform[(parse-sc syntax-class-id stx-expr arg-expr ...)]{

For debugging. Runs the parser for the syntax class (parameterized by
the @scheme[arg-expr]s) on the syntax object produced by
@scheme[stx-expr].

}
}


@section{Library syntax classes}
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

Two of the above have short aliases:

@defstxclass[id]{ Same as @scheme[identifier]. }
@defstxclass[nat]{ Same as @scheme[exact-nonnegative-integer]. }

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


@;{


1 How to abstract over similar patterns:

(syntax-parse stx #:literals (blah bleh blaz kwA kwX)
  [(blah (bleh (kwX y z)) blaz)
   ___]
  [(blah (bleh (kwA (b c))) blaz)
   ___])

=>

(define-syntax-class common
  #:attributes (inner)
  #:literals (blah bleh blaz)
  (pattern (blah (bleh inner) blaz)))
(syntax-parse stx #:literals (kwA kwX)
  [c:common
   #:with (kwX y z) #'c.inner
   ___]
  [c:common
   #:with (kwA (b c)) #'c.inner
   ___])


OR =>

(define-syntax-class (common expected-kw)
  #:attributes (inner)
  #:literals (blah bleh blaz)
  (pattern (blah (bleh (kw . inner)) blaz)
           #:when (free-identifier=? #'kw expected-kw)))
(syntax-parse stx
  [c
   #:declare c (common #'kwX)
   #:with (y z) #'c.inner
   ___]
  [c
   #:declare c (common #'kwA)
   #:with ((b c)) #'c.inner
   ___])


}


@section{Utilities}

The @schememodname[stxclass] collection includes several utility
modules. They are documented individually below.

As a shortcut, the @schememodname[stxclass/util] module provides all
of the contents of the separate utility modules:

@defmodule[stxclass/util]

@subsection{Error reporting}

@defmodule[stxclass/util/error]

The @schememodname[scheme/base] and @schememodname[scheme] languages
provide the @scheme[raise-syntax-error] procedure for reporting syntax
errors. Using @scheme[raise-syntax-error] effectively requires passing
around either a symbol indicating the special form that signals the
error or else a ``contextual'' syntax object from which the special
form's name can be extracted. This library helps manage the contextual
syntax for reporting errors.

@defparam[current-syntax-context stx (or/c syntax? false/c)]{

The current contextual syntax object, defaulting to @scheme[#f].  It
determines the special form name that prefixes syntax errors created
by @scheme[wrong-syntax], as follows:

If it is a syntax object with a @scheme['report-error-as] syntax
property whose value is a symbol, then that symbol is used as the
special form name. Otherwise, the same rules apply as in
@scheme[raise-syntax-error].

}

@defproc[(wrong-syntax [stx syntax?] [format-string string?] [v any/c] ...)
         any]{

Raises a syntax error using the result of
@scheme[(current-syntax-context)] as the ``major'' syntax object and
the provided @scheme[stx] as the specific syntax object. (The latter,
@scheme[stx], is usually the one highlighted by DrScheme.) The error
message is constructed using the format string and arguments, and it
is prefixed with the special form name as described under
@scheme[current-syntax-context].

}

A macro using this system might set the syntax context at the very
beginning of its transformation as follows:
@SCHEMEBLOCK[
(define-syntax (my-macro stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      ___)))
]
Then any calls to @scheme[wrong-syntax] during the macro's
transformation will refer to @scheme[my-macro] (more precisely, the name that
referred to @scheme[my-macro] where the macro was used, which may be
different due to renaming, prefixing, etc).

A macro that expands into a helper macro can insert its own name into
syntax errors raised by the helper macro by installing a
@scheme['report-error-as] syntax property on the helper macro
expression. For example:
@SCHEMEBLOCK[
(define-syntax (public-macro stx)
  (syntax-case stx ()
    [(public-macro stuff)
     (syntax-property
      (syntax/loc stx (my-macro stuff other-internal-stuff))
      'report-error-as
      (syntax-e #'public-macro))]))
]

@;{
@subsection[Expand]

@defmodule[stxclass/util/expand]

TODO
}

@subsection{Miscellaneous utilities}

@defmodule[stxclass/util/misc]

@defform[(define-pattern-variable id expr)]{

Evaluates @scheme[expr] and binds it to @scheme[id] as a pattern
variable, so @scheme[id] can be used in subsequent @scheme[syntax]
patterns.

}

@defform[(with-temporaries (temp-id ...) . body)]{

Evaluates @scheme[body] with each @scheme[temp-id] bound as a pattern
variable to a freshly generated identifier.

For example, the following are equivalent:
@SCHEMEBLOCK[
(with-temporaries (x) #'(lambda (x) x))
(with-syntax ([(x) (generate-temporaries '(x))])
  #'(lambda (x) x))
]

}

@defproc[(generate-temporary) identifier?]{

Generates one fresh identifier. Singular form of
@scheme[generate-temporaries].

}

@defproc[(generate-n-temporaries [n exact-nonnegative-integer?])
         (listof identifier?)]{

Generates a list of @scheme[n] fresh identifiers.

}

@defproc[(chunk-kw-seq [stx syntax?] 
                       [table
                        (listof (cons/c keyword? 
                                        (listof (-> syntax? any))))]
                       [context (or/c syntax? false/c) #f])
         (values (listof (cons/c keyword? (cons/c (syntax/c keyword?) list?)))
                 syntax?)]{

Parses a syntax list into keyword-argument ``chunks'' and a syntax
list tail (the remainder of the syntax list). The syntax of the
keyword arguments is specified by @scheme[table], an association list
mapping keywords to lists of checker procedures. The length of the
checker list is the number of ``arguments'' expected to follow the
keyword, and each checker procedure is applied to the corresponding
argument. The result of the checker procedure is entered into the
chunk for that keyword sequence. The same keyword can appear multiple
times in the result list.

The @scheme[context] is used to report errors.

}

@defproc[(chunk-kw-seq/no-dups
                       [stx syntax?] 
                       [table
                        (listof (cons/c keyword? 
                                        (listof (-> syntax? any))))]
                       [context (or/c syntax? false/c) #f])
         (values (listof (cons/c keyword? (cons/c (syntax/c keyword?) list?)))
                 syntax?)]{

Like @scheme[chunk-kw-seq] filtered by @scheme[reject-duplicate-chunks].

The @scheme[context] is used to report errors.

}

@defproc[(reject-duplicate-chunks
           [chunks (listof (cons/c keyword? (cons/c (syntax/c keyword?) list?)))])
         void?]{

Raises a syntax error if it encounters the same keyword more than once
in the @scheme[chunks] list.

The @scheme[context] is used to report errors.

}


@subsection{Structs}

@defmodule[stxclass/util/struct]

@defform[(make struct-id v ...)]{

Constructs an instance of @scheme[struct-id], which must be defined
as a struct name. If @scheme[struct-id] has a different number of
fields than the number of @scheme[v] values provided, @scheme[make]
raises a compile-time error.

}
