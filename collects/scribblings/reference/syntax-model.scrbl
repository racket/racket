#lang scribble/doc
@(require scribble/struct
          (for-syntax mzscheme)
          "mz.ss")

@(define scheme-eval (make-base-eval))
@(interaction-eval #:eval scheme-eval (require (for-syntax racket/base)))

@;------------------------------------------------------------------------
@title[#:tag "syntax-model"]{Syntax Model}

The syntax of a Scheme program is defined by

@itemize[

 @item{a @deftech{read} phase that processes a character stream into a
       @tech{syntax object}; and}

 @item{an @deftech{expand} phase that processes a syntax object to
       produce one that is fully parsed.}

]

For details on the @tech{read} phase, see @secref["reader"]. Source
code is normally read in @scheme[read-syntax] mode, which produces a
@tech{syntax object}.

The @tech{expand} phase recursively processes a @tech{syntax object}
to produce a complete @tech{parse} of the program. @tech{Binding}
information in a @tech{syntax object} drives the @tech{expansion}
process, and when the @tech{expansion} process encounters a
@tech{binding} form, it extends syntax objects for sub-expression with
new binding information.

@;------------------------------------------------------------------------
@section[#:tag "id-model"]{Identifiers and Binding}

@guideintro["binding"]{binding}

An @deftech{identifier} is source-program entity. Parsing (i.e.,
expanding) a Scheme program reveals that some @tech{identifiers}
correspond to @tech{variables}, some refer to syntactic forms, and
some are quoted to produce a symbol or a syntax object.

An identifier @deftech{binds} another (i.e., it is a
@deftech{binding}) when the former is parsed as a @tech{variable} and
the latter is parsed as a reference to the former; the latter is
@deftech{bound}. The @deftech{scope} of a @tech{binding} is the set
of source forms to which it applies. The @deftech{environment} of a
form is the set of bindings whose @tech{scope} includes the form. A
binding for a sub-expression @deftech{shadows} any @tech{bindings}
(i.e., it is @deftech{shadowing}) in its @tech{environment}, so that
uses of an @tech{identifier} refer to the @tech{shadowing}
@tech{binding}. 

For example, as a bit of source, the text

@schemeblock[(let ([x 5]) x)]

includes two @tech{identifiers}: @scheme[let] and @scheme[x] (which
appears twice). When this source is parsed in a typical
@tech{environment}, @scheme[x] turns out to represent a
@tech{variable} (unlike @scheme[let]). In particular, the first
@scheme[x] @tech{binds} the second @scheme[x].

A @deftech{top-level binding} is a @tech{binding} from a definition at
the top-level; a @deftech{module binding} is a binding from a
definition in a module; and a @deftech{local binding} is another other
kind of binding. There is no difference between an @deftech{unbound}
identifier and one with a @tech{top-level binding}; within a module,
references to @tech{top-level bindings} are disallowed, and so such
identifiers are called @tech{unbound} in a module context.

Throughout the documentation, @tech{identifiers} are typeset to
suggest the way that they are parsed. A black, boldface
@tech{identifier} like @scheme[lambda] indicates as a reference to a
syntactic form. A plain blue @tech{identifier} like @schemeidfont{x}
is a @tech{variable} or a reference to an unspecified @tech{top-level
variable}. A hyperlinked @tech{identifier} @scheme[cons] is a
reference to a specific @tech{top-level variable}.

Every binding has a @deftech{phase level} in which it can be
referenced, where a @tech{phase level} normally corresponds to an
integer (but the special @tech{label phase level} does not
correspond to an integer).  @tech{Phase level} 0 corresponds to the
run time of the enclosing module (or the run time of top-level
expressions). Bindings in @tech{phase level} 0 constitute the
@deftech{base environment}.  @tech{Phase level} 1 corresponds to the
time during which the enclosing module (or top-level expression) is
expanded; bindings in @tech{phase level} 1 constitute the
@deftech{transformer environment}.  Phase level -1 corresponds to the
run time of a different module for which the enclosing module is
imported for use at @tech{phase level} 1 (relative to the importing
module); bindings in @tech{phase level} -1 constitute the
@deftech{template environment}. The @deftech{label phase level} does not
correspond to any execution time; it is used to track bindings (e.g.,
to identifiers within documentation) without implying an execution
dependency.

If an identifier has a @tech{local binding}, then it is the same for
all phase levels, though the reference is allowed only at a particular
phase level. Attempting to reference a @tech{local binding} in a
different @tech{phase level} from the binding's context produces a
syntax error. If an identifier has a @tech{top-level binding} or
@tech{module binding}, then it can have different such bindings in
different phase levels.

@;------------------------------------------------------------------------
@section[#:tag "stxobj-model"]{Syntax Objects}

A @deftech{syntax object} combines a simpler Scheme value, such as a
symbol or pair, with @deftech{lexical information} about bindings,
source-location information, @tech{syntax properties}, and
@tech{syntax certificates}. In particular, an @tech{identifier} is
represented as a symbol object that combines a symbol and lexical and
other information.

For example, a @schemeidfont{car} @tech{identifier} might have
@tech{lexical information} that designates it as the @scheme[car] from
the @schememodname[racket/base] language (i.e., the built-in
@scheme[car]). Similarly, a @schemeidfont{lambda} identifier's
@tech{lexical information} may indicate that it represents a procedure
form. Some other @tech{identifier}'s @tech{lexical information} may
indicate that it references a @tech{top-level variable}.

When a @tech{syntax object} represents a more complex expression than
an @tech{identifier} or simple constant, its internal components can
be extracted. Even for extracted identifier, detailed information
about binding is available mostly indirectly; two identifiers can be
compared to see if they refer to the same binding (i.e.,
@scheme[free-identifier=?]), or whether each identifier would bind the
other if one was in a binding position and the other in an expression
position (i.e., @scheme[bound-identifier=?]).

For example, the when the program written as

@schemeblock[(let ([x 5]) (+ x 6))]

is represented as a @tech{syntax object}, then two @tech{syntax
objects} can be extracted for the two @scheme[x]s. Both the
@scheme[free-identifier=?] and @scheme[bound-identifier=?] predicates
will indicate that the @scheme[x]s are the same. In contrast, the
@scheme[let] @tech{identifier} is not @scheme[free-identifier=?] or
@scheme[bound-identifier=?] to either @scheme[x].

The @tech{lexical information} in a @tech{syntax object} is
independent of the other half, and it can be copied to a new syntax
object in combination with an arbitrary other Scheme value. Thus,
identifier-@tech{binding} information in a @tech{syntax object} is
predicated on the symbolic name of the @tech{identifier} as well as
the identifier's @tech{lexical information}; the same question with
the same @tech{lexical information} but different base value can
produce a different answer.

For example, combining the lexical information from @scheme[let] in
the program above to @scheme['x] would not produce an identifier that
is @scheme[free-identifier=?] to either @scheme[x], since it does not
appear in the scope of the @scheme[x] binding. Combining the lexical
context of the @scheme[6] with @scheme['x], in contrast, would produce
an identifier that is @scheme[bound-identifier=?] to both @scheme[x]s.

The @scheme[quote-syntax] form bridges the evaluation of a program and
the representation of a program. Specifically, @scheme[(quote-syntax
_datum)] produces a syntax object that preserves all of the lexical
information that @scheme[_datum] had when it was parsed as part of the
@scheme[quote-syntax] form.

@;------------------------------------------------------------------------
@section[#:tag "expansion"]{Expansion@aux-elem{ (Parsing)}}

@deftech{Expansion} recursively processes a @tech{syntax object} in a
particular phase level, starting with @tech{phase level} 0. @tech{Bindings}
from the @tech{syntax object}'s @tech{lexical information} drive the
expansion process, and cause new bindings to be introduced for the
lexical information of sub-expressions. In some cases, a
sub-expression is expanded in a deeper phase than the enclosing
expression.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "fully-expanded"]{Fully Expanded Programs}

A complete expansion produces a @tech{syntax object} matching the
following grammar:

@margin-note{Beware that the symbolic names of identifiers in a fully
expanded program may not match the symbolic names in the grammar. Only
the binding (according to @scheme[free-identifier=?]) matters.}

@schemegrammar*[
#:literals (#%expression module #%plain-module-begin begin #%provide
            define-values define-syntaxes define-values-for-syntax
            #%require
            #%plain-lambda case-lambda if begin begin0 let-values letrec-values
            set! quote-syntax quote with-continuation-mark
            #%plain-app #%top #%variable-reference)
[top-level-form general-top-level-form
                (#%expression expr)
                (module id name-id
                  (#%plain-module-begin
                   module-level-form ...))
                (begin top-level-form ...)]
[module-level-form general-top-level-form
                   (#%provide raw-provide-spec ...)]
[general-top-level-form expr
                        (define-values (id ...) expr)
                        (define-syntaxes (id ...) expr)
                        (define-values-for-syntax (id ...) expr)
                        (#%require raw-require-spec ...)]
[expr id
      (#%plain-lambda formals expr ...+)
      (case-lambda (formals expr ...+) ...)
      (if expr expr expr)
      (begin expr ...+)
      (begin0 expr expr ...)
      (let-values (((id ...) expr) ...)
        expr ...+)
      (letrec-values (((id ...) expr) ...)
        expr ...+)
      (set! id expr)
      (@#,scheme[quote] datum)
      (quote-syntax datum)
      (with-continuation-mark expr expr expr)
      (#%plain-app expr ...+)
      (#%top . id)
      (#%variable-reference id)
      (#%variable-reference (#%top . id))
      (#%variable-reference)]
[formals (id ...)
         (id ...+ . id)
         id]]

A @deftech{fully-expanded} @tech{syntax object} corresponds to a @deftech{parse}
of a program (i.e., a @deftech{parsed} program), and @tech{lexical
information} on its @tech{identifiers} indicates the
@tech{parse}.

More specifically, the typesetting of identifiers in the above grammar
is significant. For example, the second case for @scheme[_expr] is a
@tech{syntax-object} list whose first element is an @tech{identifier},
where the @tech{identifier}'s @tech{lexical information} specifies a
binding to the @scheme[#%plain-lambda] of the
@schememodname[racket/base] language (i.e., the @tech{identifier} is
@scheme[free-identifier=?] to one whose binding is
@scheme[#%plain-lambda]). In all cases, identifiers above typeset as
syntactic-form names refer to the bindings defined in
@secref["syntax"].

Only @tech{phase levels} 0 and 1 are relevant for the parse of a
program (though the @scheme[_datum] in a @scheme[quote-syntax] form
preserves its information for all @tech{phase level}s). In particular,
the relevant @tech{phase level} is 0, except for the @scheme[_expr]s
in a @scheme[define-syntax], @scheme[define-syntaxes],
@scheme[define-for-syntax], or @scheme[define-values-for-syntax] form,
in which case the relevant @tech{phase level} is 1 (for which
comparisons are made using @scheme[free-transformer-identifier=?]
instead of @scheme[free-identifier=?]).

In addition to the grammar above, @scheme[letrec-syntaxes+values] can
appear in a fully local-expanded expression, such as the result from
@scheme[local-expand] when the stop list is empty.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "expand-steps"]{Expansion Steps}

In a recursive expansion, each single step in expanding a @tech{syntax
object} at a particular @tech{phase level} depends on the immediate shape of
the @tech{syntax object} being expanded:

@itemize[

 @item{If it is an @tech{identifier} (i.e., a syntax-object symbol),
       then a @tech{binding} is determined by the @tech{identifier}'s
       @tech{lexical information}. If the @tech{identifier} has a
       @tech{binding} other than as a @tech{top-level variable}, that
       @tech{binding} is used to continue. If the @tech{identifier}
       has no @tech{binding}, a new @tech{syntax-object} symbol
       @scheme['#%top] is created using the @tech{lexical information}
       of the @tech{identifier}; if this @schemeidfont{#%top}
       @tech{identifier} has no @tech{binding} (other than as a
       @tech{top-level variable}), then parsing fails with an
       @scheme[exn:fail:syntax] exception. Otherwise, the new
       @tech{identifier} is combined with the original
       @tech{identifier} in a new @tech{syntax-object} pair (also
       using the same @tech{lexical information} as the original
       @tech{identifier}), and the @schemeidfont{#%top} @tech{binding}
       is used to continue.}

 @item{If it is a @tech{syntax-object} pair whose first element is an
      @tech{identifier}, and if the @tech{identifier} has a
      @tech{binding} other than as a @tech{top-level variable}, then
      the @tech{identifier}'s @tech{binding} is used to continue.}

 @item{If it is a @tech{syntax-object} pair of any other form, then a
       new @tech{syntax-object} symbol @scheme['#%app] is created
       using the @tech{lexical information} of the pair. If the
       resulting @schemeidfont{#%app} @tech{identifier} has no
       binding, parsing fails with an @scheme[exn:fail:syntax]
       exception. Otherwise, the new @tech{identifier} is combined
       with the original pair to form a new @tech{syntax-object} pair
       (also using the same @tech{lexical information} as the original
       pair), and the @schemeidfont{#%app} @tech{binding} is used to
       continue.}

 @item{If it is any other syntax object, then a new
       @tech{syntax-object} symbol @scheme['#%datum] is created using
       the @tech{lexical information} of the original @tech{syntax
       object}. If the resulting @schemeidfont{#%datum}
       @tech{identifier} has no @tech{binding}, parsing fails with an
       @scheme[exn:fail:syntax] exception. Otherwise, the new
       @tech{identifier} is combined with the original @tech{syntax
       object} in a new @tech{syntax-object} pair (using the same
       @tech{lexical information} as the original pair), and the
       @schemeidfont{#%datum} @tech{binding} is used to continue.}

]

Thus, the possibilities that do not fail lead to an @tech{identifier}
with a particular @tech{binding}. This binding refers to one of three
things:

@itemize[

 @item{A @tech{transformer binding}, such as introduced by
       @scheme[define-syntax] or @scheme[let-syntax]. If the
       associated value is a procedure of one argument, the procedure
       is called as a @tech{syntax transformer} (described below), and
       parsing starts again with the @tech{syntax-object} result. If
       the @tech{transformer binding} is to any other kind of value,
       parsing fails with an @scheme[exn:fail:syntax] exception. The
       call to the @tech{syntax transformer} is @scheme[parameterize]d
       to set @scheme[current-namespace] to a @tech{namespace} that
       shares @tech{bindings} and @tech{variables} with the namespace
       being used to expand, except that its @tech{base phase} is one
       greater.}

 @item{A @tech{variable} @tech{binding}, such as introduced by a
       module-level @scheme[define] or by @scheme[let]. In this case,
       if the form being parsed is just an @tech{identifier}, then it
       is parsed as a reference to the corresponding
       @tech{variable}. If the form being parsed is a
       @tech{syntax-object} pair, then an @scheme[#%app] is added to
       the front of the @tech{syntax-object} pair in the same way as
       when the first item in the @tech{syntax-object} pair is not an
       identifier (third case in the previous enumeration), and
       parsing continues.}

 @item{A core @deftech{syntactic form}, which is parsed as described
       for each form in @secref["syntax"]. Parsing a core syntactic
       form typically involves recursive parsing of sub-forms, and may
       introduce @tech{bindings} that determine the parsing of
       sub-forms.}

]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "expand-context-model"]{Expansion Context}

Each expansion step occurs in a particular @deftech{context}, and
transformers and core syntactic forms may expand differently for
different @tech{contexts}. For example, a @scheme[module] form is
allowed only in a @tech{top-level context}, and it fails in other
contexts. The possible @tech{contexts} are as follows:

@itemize[

 @item{@deftech{top-level context} : outside of any module, definition, or
       expression, except that sub-expressions of a top-level
       @scheme[begin] form are also expanded as top-level forms.}

 @item{@deftech{module-begin context} : inside the body of a module, as the
       only form within the module.}

 @item{@deftech{module context} : in the body of a module (inside the
       module-begin layer).}

 @item{@deftech{internal-definition context} : in a nested context that allows
       both definitions and expressions.}

 @item{@deftech{expression context} : in a context where only
       expressions are allowed.}

]

Different core @tech{syntactic forms} parse sub-forms using different
@tech{contexts}. For example, a @scheme[let] form always parses the
right-hand expressions of a binding in an @tech{expression context},
but it starts parsing the body in an @tech{internal-definition
context}.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "intro-binding"]{Introducing Bindings}

@tech{Bindings} are introduced during @tech{expansion} when certain
core syntactic forms are encountered:

@itemize[

 @item{When a @scheme[require] form is encountered at the top level or
       module level, all lexical information derived from the top
       level or the specific module's level are extended with bindings
       from the specified modules. If not otherwise indicated in the
       @scheme[require] form, bindings are introduced at the
       @tech{phase level}s specified by the exporting modules:
       @tech{phase level} 0 for each normal @scheme[provide],
       @tech{phase level} 1 for each @scheme[for-syntax]
       @scheme[provide], and so on. The @scheme[for-meta]
       @scheme[provide] form allows exports at an arbitrary
       @tech{phase level} (as long as a binding exists within the
       module at the @tech{phase level}).

       A @scheme[for-syntax] sub-form within @scheme[require] imports
       similarly, but the resulting bindings have a @tech{phase level}
       that is one more than the exported @tech{phase levels}, when
       exports for the @tech{label phase level} are still imported at
       the @tech{label phase level}. More generally, a
       @scheme[for-meta] sub-form within @scheme[require] imports with
       the specified @tech{phase level} shift; if the specified shift
       is @scheme[#f], or if @scheme[for-label] is used to import,
       then all bindings are imported into the @tech{label phase
       level}.}

 @item{When a @scheme[define], @scheme[define-values],
       @scheme[define-syntax], or @scheme[define-syntaxes] form is
       encountered at the top level or module level, all lexical
       information derived from the top level or the specific module's
       level is extended with bindings for the specified identifiers
       at @tech{phase level} 0 (i.e., the @tech{base environment} is
       extended).}

 @item{When a @scheme[define-for-syntax] or
       @scheme[define-values-for-syntax] form is encountered at the
       top level or module level, bindings are introduced as for
       @scheme[define-values], but at @tech{phase level} 1 (i.e., the
       @tech{transformer environment} is extended).}

 @item{When a @scheme[let-values] form is encountered, the body of the
       @scheme[let-values] form is extended (by creating new
       @tech{syntax objects}) with bindings for the specified
       identifiers. The same bindings are added to the identifiers
       themselves, so that the identifiers in binding position are
       @scheme[bound-identifier=?] to uses in the fully expanded form,
       and so they are not @scheme[bound-identifier=?] to other
       identifiers. The bindings are available for use at the
       @tech{phase level} at which the @scheme[let-values] form is
       expanded.}

 @item{When a @scheme[letrec-values] or
       @scheme[letrec-syntaxes+values] form is encountered, bindings
       are added as for @scheme[let-values], except that the
       right-hand-side expressions are also extended with the
       bindings.}

 @item{Definitions in @tech{internal-definition contexts} introduce
       bindings as described in @secref["intdef-body"].}

]

A new binding in lexical information maps to a new variable. The
identifiers mapped to this variable are those that currently have the
same binding (i.e., that are currently @scheme[bound-identifier=?]) to
the identifier associated with the binding.

For example, in

@schemeblock[
(let-values ([(x) 10]) (+ x y))
]

the binding introduced for @scheme[x] applies to the @scheme[x] in the
body, but not the @scheme[y] n the body, because (at the point in
expansion where the @scheme[let-values] form is encountered) the
binding @scheme[x] and the body @scheme[y] are not
@scheme[bound-identifier=?].

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "transformer-model"]{Transformer Bindings}

In a @tech{top-level context} or @tech{module context}, when the
expander encounters a @scheme[define-syntaxes] form, the binding that
it introduces for the defined identifiers is a @deftech{transformer
binding}. The @tech{value} of the @tech{binding} exists at expansion
time, rather than run time (though the two times can overlap), though
the binding itself is introduced with @tech{phase level} 0 (i.e., in
the @tech{base environment}).

The @tech{value} for the binding is obtained by evaluating the
expression in the @scheme[define-syntaxes] form. This expression must
be @tech{expand}ed (i.e. parsed) before it can be evaluated, and it is
expanded at @tech{phase level} 1 (i.e., in the @tech{transformer
environment}) instead of @tech{phase level} 0.

If the resulting @scheme[value] is a procedure of one argument or
the result of @scheme[make-set!-transformer] on a procedure, then it
is used as a @deftech{syntax transformer} (a.k.a. @deftech{macro}).
The procedure is expected to accept a syntax object and return a
syntax object. A use of the binding (at @tech{phase level} 0) triggers
a call of the @tech{syntax transformer} by the expander; see
@secref["expand-steps"].

Before the expander passes a @tech{syntax object} to a transformer,
the @tech{syntax object} is extended with a @deftech{syntax mark} (that
applies to all sub-@tech{syntax objects}). The result of the
transformer is similarly extended with the same @tech{syntax
mark}. When a @tech{syntax object}'s @tech{lexical information}
includes the same mark twice in a row, the marks effectively
cancel. Otherwise, two identifiers are @scheme[bound-identifier=?]
(that is, one can bind the other) only if they have the same binding
and if they have the same marks---counting only marks that were added
after the binding.

This marking process helps keep binding in an expanded program
consistent with the lexical structure of the source program. For
example, the expanded form of the program

@schemeblock[
(define x 12)
(define-syntax m
  (syntax-rules ()
    [(_ id) (let ([x 10]) id)]))
(m x)
]

is

@schemeblock[
(define x 12)
(define-syntax m
  (syntax-rules ()
    [(_ id) (let ([x 10]) id)]))
(let-values ([(x) 10]) x)
]

However, the result of the last expression is @scheme[12], not
@scheme[10]. The reason is that the transformer bound to @scheme[m]
introduces the binding @scheme[x], but the referencing @scheme[x] is
present in the argument to the transformer. The introduced @scheme[x]
is the one left with a mark, and the reference @scheme[x] has no mark,
so the binding @scheme[x] is not @scheme[bound-identifier=?] to the
body @scheme[x].

The @scheme[set!] form works with the @scheme[make-set!-transformer]
and @scheme[prop:set!-transformer] property to support
@deftech{assignment transformers} that transform @scheme[set!]
expressions. An @tech{assignment transformer} contains a procedure
that is applied by @scheme[set!] in the same way as a normal
transformer by the expander.

The @scheme[make-rename-transformer] procedure or
@scheme[prop:rename-transformer] property creates a value that is also
handled specially by the expander and by @scheme[set!] as a
transformer binding's value. When @scheme[_id] is bound to a
@deftech{rename transformer} produced by
@scheme[make-rename-transformer], it is replaced with the target
identifier passed to @scheme[make-rename-transformer]. In addition, as
long as the target identifier does not have a true value for the
@scheme['not-free-identifier=?] @tech{syntax property}, the lexical information that
contains the binding of @scheme[_id] is also enriched so that
@scheme[_id] is @scheme[free-identifier=?] to the target identifier,
@scheme[identifier-binding] returns the same results for both
identifiers, and @scheme[provide] exports @scheme[_id] as the target
identifier. Finally, the binding is treated specially by
@scheme[syntax-local-value], and
@scheme[syntax-local-make-delta-introducer] as used by @tech{syntax
transformer}s.

In addition to using marks to track introduced identifiers, the
expander tracks the expansion history of a form through @tech{syntax
properties} such as @scheme['origin]. See @secref["stxprops"] for
more information.

Finally, the expander uses @tech{syntax certificates} to control the
way that unexported and protected @tech{module bindings} are used. See
@secref["stxcerts"] for more information on @tech{syntax
certificates}.

The expander's handling of @scheme[letrec-values+syntaxes] is similar
to its handling of @scheme[define-syntaxes]. A
@scheme[letrec-values+syntaxes] mist be expanded in an arbitrary phase
level @math{n} (not just 0), in which case the expression for the
@tech{transformer binding} is expanded at @tech{phase level} @math{n+1}.

The expression in a @scheme[define-for-syntax] or
@scheme[define-values-for-syntax] form is expanded and evaluated in
the same way as for @scheme[syntax]. However, the introduced binding
is a variable binding at @tech{phase level} 1 (not a @tech{transformer
binding} at @tech{phase level} 0).

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "partial-expansion"]{Partial Expansion}

In certain contexts, such as an @tech{internal-definition context} or
@tech{module context}, forms are partially expanded to determine
whether they represent definitions, expressions, or other declaration
forms. Partial expansion works by cutting off the normal recursion
expansion when the relevant binding is for a primitive syntactic form.

As a special case, when expansion would otherwise add an
@schemeidfont{#%app}, @schemeidfont{#%datum}, or @schemeidfont{#%top}
identifier to an expression, and when the binding turns out to be the
primitive @scheme[#%app], @scheme[#%datum], or @scheme[#%top] form,
then expansion stops without adding the identifier.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "intdef-body"]{Internal Definitions}

An @tech{internal-definition context} corresponds to a partial expansion step
(see @secref["partial-expansion"]). A form that supports internal
definitions starts by expanding its first form in an
internal-definition context, but only partially. That is, it
recursively expands only until the form becomes one of the following:

@itemize[

 @item{A @scheme[define-values] or @scheme[define-syntaxes] form, for
       any form other than the last one: The definition form is not
       expanded further. Instead, the next form is expanded partially,
       and so on. As soon as an expression form is found, the
       accumulated definition forms are converted to a
       @scheme[letrec-values] (if no @scheme[define-syntaxes] forms
       were found) or @scheme[letrec-syntaxes+values] form, moving the
       expression forms to the body to be expanded in expression
       context.

       When a @scheme[define-values] form is discovered, the lexical
       context of all syntax objects for the body sequence is
       immediately enriched with bindings for the
       @scheme[define-values] form before expansion continues. When a
       @scheme[define-syntaxes] form is discovered, the right-hand
       side is expanded and evaluated (as for a
       @scheme[letrec-values+syntaxes] form), and a transformer
       binding is installed for the body sequence before expansion
       continues.}

 @item{A primitive expression form other than @scheme[begin]: The
       expression is expanded in an expression context, along with all
       remaining body forms. If any definitions were found, this
       expansion takes place after conversion to a
       @scheme[letrec-values] or @scheme[letrec-syntaxes+values]
       form. Otherwise, the expressions are expanded immediately.}

 @item{A @scheme[begin] form: The sub-forms of the @scheme[begin] are
       spliced into the internal-definition sequence, and partial
       expansion continues with the first of the newly-spliced forms
       (or the next form, if the @scheme[begin] had no sub-forms).}

]

If the last expression form turns out to be a @scheme[define-values]
or @scheme[define-syntaxes] form, expansion fails with a syntax error.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "mod-parse"]{Module Phases and Visits}

A @scheme[require] form not only introduces @tech{bindings} at
expansion time, but also @deftech{visits} the referenced module when
it is encountered by the expander. That is, the expander
instantiates any @scheme[define-for-syntax]ed variables defined
in the module, and also evaluates all expressions for
@scheme[define-syntaxes] @tech{transformer bindings}.

Module @tech{visits} propagate through @scheme[require]s in the same
way as module @tech{instantiation}. Moreover, when a module is
@tech{visit}ed at @tech{phase} 0, any module that it @scheme[require]s
@scheme[for-syntax] is @tech{instantiate}d at @tech{phase} 1, while
further @scheme[require]s @scheme[for-template] leading back
to @tech{phase} 0 causes the required module to be visited at
@tech{phase} 0 (i.e., not @tech{instantiate}d).

During compilation, the top-level of module context is itself
implicitly @tech{visit}ed. Thus, when the expander encounters
@scheme[(require (for-syntax ....))], it immediately
@tech{instantiate}s the required module at @tech{phase} 1, in addition
to adding bindings at @tech{phase level} 1 (i.e., the
@tech{transformer environment}). Similarly, the expander immediately
evaluates any @scheme[define-values-for-syntax] form that it
encounters.

@tech{Phases} beyond 0 are @tech{visit}ed on demand. For example,
when the right-hand side of a @tech{phase}-0 @scheme[let-syntax] is to
be expanded, then modules that are @tech{available} at @tech{phase} 1
are visited. More generally, initiating expansion at @tech{phase}
@math{n} @tech{visit}s modules at @tech{phase} @math{n}, which in turn
@tech{instantiates} modules at @tech{phase} @math{n+1}. These
@tech{visits} and @tech{instantiations} apply to @tech{available}
modules in the enclosing @tech{namespace}'s @tech{module registry};
a per-registry lock prevents multiple threads from concurrently
instantiating and visiting available modules.

When the expander encounters @scheme[require] and @scheme[(require
(for-syntax ....))] within a @tech{module context}, the resulting
@tech{visits} and @tech{instantiations} are specific to the expansion
of the enclosing module, and are kept separate from @tech{visits} and
@tech{instantiations} triggered from a @tech{top-level context} or
from the expansion of a different module. Along the same lines, when a
module is attached to a namespace through
@scheme[namespace-attach-module], modules that it @scheme[require]s
are transitively attached, but instances are attached only at
phases at or below the namespace's @tech{base phase}.

@;------------------------------------------------------------------------
@subsection[#:tag "macro-introduced-bindings"]{Macro-Introduced Bindings}

When a top-level definition binds an identifier that originates from a
 macro expansion, the definition captures only uses of the identifier
 that are generated by the same expansion. This behavior is consistent
 with expansion in @tech{internal-definition contexts}, where the
 defined identifier turns into a fresh lexical binding.

@examples[
(define-syntax def-and-use-of-x
  (syntax-rules ()
    [(def-and-use-of-x val)
     (code:comment @#,t{@scheme[x] below originates from this macro:})
     (begin (define x val) x)]))
(define x 1)
x
(def-and-use-of-x 2)
x

(define-syntax def-and-use
  (syntax-rules ()
    [(def-and-use x val)
     (code:comment @#,t{@scheme{x} below was provided by the macro use:})
     (begin (define x val) x)]))
(def-and-use x 3)
x
]

For a top-level definition (outside of a module), the order of
 evaluation affects the binding of a generated definition for a
 generated identifier use. If the use precedes the definition, then
 the use refers to a non-generated binding, just as if the generated
 definition were not present. (No such dependency on order occurs
 within a module, since a module binding covers the entire module
 body.) To support the declaration of an identifier before its use,
 the @scheme[define-syntaxes] form avoids binding an identifier if the
 body of the @scheme[define-syntaxes] declaration produces zero
 results.

@examples[
#:eval scheme-eval
(define bucket-1 0)
(define bucket-2 0)
(define-syntax def-and-set!-use-of-x
  (syntax-rules ()
    [(def-and-set!-use-of-x val)
     (begin (set! bucket-1 x) (define x val) (set! bucket-2 x))]))
(define x 1)
(def-and-set!-use-of-x 2)
x
bucket-1
bucket-2

(define-syntax defs-and-uses/fail
  (syntax-rules ()
    [(def-and-use)
     (begin
      (code:comment @#,t{Initial reference to @scheme[even] precedes definition:})
      (define (odd x) (if (zero? x) #f (even (sub1 x))))
      (define (even x) (if (zero? x) #t (odd (sub1 x))))
      (odd 17))]))
(defs-and-uses/fail)
     
(define-syntax defs-and-uses
  (syntax-rules ()
    [(def-and-use)
     (begin
      (code:comment @#,t{Declare before definition via no-values @scheme[define-syntaxes]:})
      (define-syntaxes (odd even) (values))
      (define (odd x) (if (zero? x) #f (even (sub1 x))))
      (define (even x) (if (zero? x) #t (odd (sub1 x))))
      (odd 17))]))
(defs-and-uses)
]

Macro-generated @scheme{require} and @scheme{provide}
 clauses also introduce and reference generation-specific bindings:

@itemize[

 @item{In @scheme[require], for a @scheme[_require-spec] of the form
 @scheme[(rename-in [_orig-id _bind-id])] or @scheme[(only-in
 .... [_orig-id _bind-id])], the @scheme[_bind-id] is bound only for
 uses of the identifier generated by the same macro expansion as
 @scheme[_bind-id]. In @scheme[require] for other
 @scheme[_require-spec]s, the generator of the @scheme[_require-spec]
 determines the scope of the bindings.}

 @item{In @scheme[provide], for a @scheme[_provide-spec] of the form
 @scheme[_id], the exported identifier is the one that binds
 @scheme[_id] within the module in a generator-specific way, but the
 external name is the plain @scheme[_id]. The exceptions for
 @scheme[all-except-out] are similarly determined in a
 generator-specific way, as is the @scheme[_orig-id] binding of a
 @scheme[rename-out] form, but plain identifiers are used for the
 external names. For @scheme[all-defined-out], only identifiers with
 definitions having the same generator as the
 @scheme[(all-defined-out)] form are exported; the external name is
 the plain identifier from the definition.}

]

@;------------------------------------------------------------------------
@section[#:tag "compilation-model"]{Compilation}

Before expanded code is evaluated, it is first @deftech{compiled}. A
compiled form has essentially the same information as the
corresponding expanded form, though the internal representation
naturally dispenses with identifiers for syntactic forms and local
bindings. One significant difference is that a compiled form is almost
entirely opaque, so the information that it contains cannot be
accessed directly (which is why some identifiers can be dropped). At
the same time, a compiled form can be marshaled to and from a byte
string, so it is suitable for saving and re-loading code.

Although individual read, expand, compile, and evaluate operations are
available, the operations are often combined automatically. For
example, the @scheme[eval] procedure takes a syntax object and expands
it, compiles it, and evaluates it.

@;------------------------------------------------------------------------
@section[#:tag "namespace-model"]{Namespaces}

@margin-note/ref{See @secref["Namespaces"] for functions that
manipulate namespaces.}

A @deftech{namespace} is a top-level mapping from symbols to binding
information. It is the starting point for expanding an expression; a
@tech{syntax object} produced by @scheme[read-syntax] has no initial
lexical context; the @tech{syntax object} can be expanded after
initializing it with the mappings of a particular namespace. A
namespace is also the starting point evaluating expanded code, where
the first step in evaluation is linking the code to specific module
instances and top-level variables.

For expansion purposes, a namespace maps each symbol in each
@tech{phase level} to one of three possible bindings:

@itemize[

 @item{a particular @tech{module binding} from a particular module}

 @item{a top-level transformer binding named by the symbol}

 @item{a top-level variable named by the symbol}

]

An ``empty'' namespace maps all symbols to top-level variables.
Certain evaluations extend a namespace for future expansions;
importing a module into the top-level adjusts the namespace bindings
for all of the imported named, and evaluating a top-level
@scheme[define] form updates the namespace's mapping to refer to a
variable (in addition to installing a value into the variable).

A namespace also has a @deftech{module registry} that maps module
names to module declarations (see @secref["module-eval-model"]).
This registry is shared by all @tech{phase level}s.

For evaluation, each namespace encapsulates a distinct set of
top-level variables at various @tech{phases}, as well as a potentially
distinct set of module instances in each @tech{phase}. That is, even
though module declarations are shared for all @tech{phase levels},
module instances are distinct for each @tech{phase}. Each namespace
has a @deftech{base phase}, which corresponds to the phase used by
reflective operations such as @scheme[eval] and
@scheme[dynamic-require]. In particular, using @scheme[eval] on a
@scheme[require] form @tech{instantiates} a module in the namespace's
@tech{base phase}.

After a namespace is created, module instances from existing
namespaces can be attached to the new namespace.  In terms of the
evaluation model, top-level variables from different namespaces
essentially correspond to definitions with different prefixes, but
attaching a module uses the same prefix for the module's definitions
in namespaces where it is attached.  The first step in evaluating any
compiled expression is to link its top-level variable and module-level
variable references to specific variables in the namespace.

At all times during evaluation, some namespace is designated as the
@deftech{current namespace}. The current namespace has no particular
relationship, however, with the namespace that was used to expand the
code that is executing, or with the namespace that was used to link
the compiled form of the currently evaluating code. In particular,
changing the current namespace during evaluation does not change the
variables to which executing expressions refer. The current namespace
only determines the behavior of reflective operations to expand code
and to start evaluating expanded/compiled code.

@examples[
(code:line
 (define x 'orig) (code:comment @#,t{define in the original namespace}))
(code:comment @#,t{The following @scheme[let] expression is compiled in the original})
(code:comment @#,t{namespace, so direct references to @scheme[x] see @scheme['orig].})
(code:line
 (let ([n (make-base-namespace)]) (code:comment @#,t{make new namespace})
   (parameterize ([current-namespace n]) 
     (eval '(define x 'new)) (code:comment @#,t{evals in the new namespace})
     (display x) (code:comment @#,t{displays @scheme['orig]})
     (display (eval 'x)))) (code:comment @#,t{displays @scheme['new]}))
]

A @tech{namespace} is purely a top-level entity, not to be confused
with an @tech{environment}. In particular, a @tech{namespace} does not
encapsulate the full @tech{environment} of an expression inside
local-binding forms.

If an @tech{identifier} is bound to syntax or to an import, then
defining the @tech{identifier} as a @tech{variable} shadows the syntax
or import in future uses of the environment. Similarly, if an
@tech{identifier} is bound to a @tech{top-level variable}, then
binding the identifier to syntax or an import shadows the variable;
the variable's value remains unchanged, however, and may be accessible
through previously evaluated expressions.

@examples[
(define x 5)
(define (f) x)
x
(f)
(define-syntax x (syntax-id-rules () [_ 10]))
x
(f)
(define x 7)
x
(f)
(module m mzscheme (define x 8) (provide x))
(require 'm)
(eval:alts x (eval 'x))
(f)
]

@;------------------------------------------------------------------------
@section[#:tag "infernames"]{Inferred Value Names}

To improve error reporting, names are inferred at compile-time for
certain kinds of values, such as procedures. For example, evaluating
the following expression:

@schemeblock[
(let ([f (lambda () 0)]) (f 1 2 3))
]

produces an error message because too many arguments are provided to
the procedure. The error message is able to report @schemeidfont{f} as
the name of the procedure. In this case, Scheme decides, at
compile-time, to name as @scheme['f] all procedures created by the
@scheme[let]-bound @scheme[lambda].

Names are inferred whenever possible for procedures. Names closer to
an expression take precedence. For example, in

@schemeblock[
(define my-f
  (let ([f (lambda () 0)]) f))
]

the procedure bound to @scheme[my-f] will have the inferred name
@scheme['f].

When an @indexed-scheme['inferred-name] property is attached to a
syntax object for an expression (see @secref["stxprops"]), the
property value is used for naming the expression, and it overrides any
name that was inferred from the expression's context. Normally, the
property value should be a symbol or an identifier.

When an inferred name is not available, but a source location is
available, a name is constructed using the source location
information. Inferred and property-assigned names are also available
to syntax transformers, via @scheme[syntax-local-name].

@;----------------------------------------

@close-eval[scheme-eval]
