#lang scribble/doc
@(require scribble/struct "mz.rkt" (for-syntax mzscheme))

@(define racket-eval (make-base-eval))
@(interaction-eval #:eval racket-eval (require (for-syntax racket/base)))

@;------------------------------------------------------------------------
@title[#:tag "syntax-model"]{Syntax Model}

The syntax of a Racket program is defined by

@itemize[

 @item{a @deftech{read} pass that processes a character stream into a
       @tech{syntax object}; and}

 @item{an @deftech{expand} pass that processes a syntax object to
       produce one that is fully parsed.}

]

For details on the @tech{read} pass, see @secref["reader"]. Source
code is normally read in @racket[read-syntax] mode, which produces a
@tech{syntax object}.

The @tech{expand} pass recursively processes a @tech{syntax object}
to produce a complete @tech{parse} of the program. @tech{Binding}
information in a @tech{syntax object} drives the @tech{expansion}
process, and when the @tech{expansion} process encounters a
@tech{binding} form, it extends syntax objects for sub-expression with
new binding information.

@;------------------------------------------------------------------------
@section[#:tag "id-model"]{Identifiers, Binding, and Scopes}

@guideintro["binding"]{binding}

An @deftech{identifier} is a source-program entity. Parsing (i.e.,
expanding) a Racket program reveals that some @tech{identifiers}
correspond to @tech{variables}, some refer to @tech{syntactic forms}
(such as @racket[lambda], which is the @tech{syntactic form} for
functions), some refer to @tech{transformers} for macro expansion, and
some are quoted to produce @tech{symbols} or @tech{syntax objects}. An
identifier @deftech{binds} another (i.e., it is a @deftech{binding})
when the former is parsed as a @tech{variable} or syntactic form and
the latter is parsed as a @deftech{reference} to the former; the
latter is @deftech{bound}.

For example, as a fragment of source, the text

@racketblock[(let ([x 5]) x)]

includes two @tech{identifiers}: @racket[let] and @racket[x] (which
appears twice). When this source is parsed in a context where
@racket[let] has its usual meaning, the first @racket[x] @tech{binds}
the second @racket[x].

Bindings and references are determined through @tech{scope sets}. A
@deftech{scope} corresponds to a region of the program that is either
in part of the source or synthesized through elaboration of the
source. Nested binding contexts (such as nested functions) create
nested @tech{scopes}, while macro expansion creates scopes that
overlap in more complex ways. Conceptually, each @tech{scope} is
represented by a unique token, but the token is not directly
accessible. Instead, each @tech{scope} is represented by a value that
is internal to the representation of a program.

A @deftech{form} is a fragment of a program, such as an identifier or
a function call. A @tech{form} is represented as a @tech{syntax
object}, and each syntax object has an associated set of @tech{scopes}
(i.e., a @deftech{scope set}). In the above example,
the representations of the @racket[x]s include the @tech{scope} that
corresponds to the @racket[let] form.

When a @tech{form} parses as the binding of a particular identifier,
parsing updates a global table that maps a combination of an
identifier's @tech{symbol} and @tech{scope set} to its meaning: a
@tech{variable}, a @tech{syntactic form}, or a @tech{transformer}. An
identifier refers to a particular binding when the reference's symbol
and the identifier's symbol are the same, and when the reference's
@tech{scope set} is a subset of the binding's
@tech{scope set}. For a given identifier, multiple bindings may have
@tech{scope sets} that are subsets of the identifier's; in that case,
the identifier refers to the binding whose set is a superset of all
others; if no such binding exists, the reference is ambiguous (and triggers a syntax
error if it is parsed as an expression). A binding @deftech{shadows}
any @tech{binding} (i.e., it is @deftech{shadowing} any @tech{binding}) 
that the same symbol but a subset of scopes.

For example, in

@racketblock[(let ([x 5]) x)]

in a context where @racket[let] corresponds to the usual
@tech{syntactic form}, the parsing of @racket[let] introduces a new
scope for the binding of @racket[x]. Since the second @racket[x]
receives that scope as part of the @racket[let] body, the first
@racket[x] @tech{binds} the second @racket[x]. In the more complex
case

@racketblock[(let ([x 5])
               (let ([x 6])
                 x))]

the inner @racket[let] creates a second scope for the second
@racket[x]s, so its @tech{scope set} is a superset of the first
@racket[x]'s @tech{scope set}---which means that the binding for the
second @racket[x] @tech{shadows} the one for the first @racket[x], and
the third @racket[x] refers to the binding created by the second one.

A @deftech{top-level binding} is a @tech{binding} from a definition at
the top-level; a @deftech{module binding} is a binding from a
definition in a module; all other bindings are @deftech{local
bindings}. Within a module, references to @tech{top-level bindings}
are disallowed. An identifier without a binding is @deftech{unbound}.

Throughout the documentation, @tech{identifiers} are typeset to
suggest the way that they are parsed. A hyperlinked identifier
like @racket[lambda] indicates a reference to a syntactic form or
variable. A plain identifier like @racketidfont{x} is a
@tech{variable} or a reference to an unspecified @tech{top-level
variable}.

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

An identifier can have different bindings in different @tech{phase
levels}. More precisely, the @tech{scope set} associated with a
@tech{form} can be different at different phase levels; a top-level or
module context implies a distinct scope at every phase level, while
scopes from macro expansion or other syntactic forms are added to a
form's @tech{scope sets} at all phases. The context of each binding
and reference determines the @tech{phase level} whose @tech{scope set} is
relevant.

@history[#:changed "6.3" @elem{Changed local bindings to have a
                               specific phase level, like top-level
                               and module bindings.}]

@;------------------------------------------------------------------------
@section[#:tag "stxobj-model"]{Syntax Objects}

A @deftech{syntax object} combines a simpler Racket value, such as a
symbol or pair, with a @tech{scope set} at each @tech{phase level},
source-location information, @tech{syntax properties}, and
@tech{tamper status}. In particular, an @tech{identifier} is
represented as a syntax object that combines a @tech{symbol} with scope sets
and other information. The @deftech{lexical information} of a
@tech{syntax object} is its @tech{scope set} combined with the portion
of the global table of bindings that is relevant to the syntax
object's set of scopes.

For example, a @racketidfont{car} @tech{identifier} might have
@tech{lexical information} that designates it as the @racket[car] from
the @racketmodname[racket/base] language (i.e., the built-in
@racket[car]). Similarly, a @racketidfont{lambda} identifier's
@tech{lexical information} may indicate that it represents a procedure
form. Some other @tech{identifier}'s @tech{lexical information} may
indicate that it references a @tech{top-level variable}.

When a @tech{syntax object} represents a more complex expression than
an @tech{identifier} or simple constant, its internal components can
be extracted. Even for extracted identifiers, detailed information
about binding is available mostly indirectly; two identifiers can be
compared to determine whether they refer to the same binding (i.e.,
@racket[free-identifier=?]), or whether the identifiers have the same
@tech{scope set} so that each identifier would bind the
other if one were in a binding position and the other in an expression
position (i.e., @racket[bound-identifier=?]).

For example, when the program written as

@racketblock[(let ([x 5]) (+ x 6))]

is represented as a @tech{syntax object}, then two @tech{syntax
objects} can be extracted for the two @racket[x]s. Both the
@racket[free-identifier=?] and @racket[bound-identifier=?] predicates
will indicate that the @racket[x]s are the same. In contrast, the
@racket[let] @tech{identifier} is not @racket[free-identifier=?] or
@racket[bound-identifier=?] to either @racket[x].

The @tech{lexical information} in a @tech{syntax object} is
independent of the rest of the @tech{syntax object}, and it can be copied to a new syntax
object in combination with an arbitrary other Racket value. Thus,
identifier-@tech{binding} information in a @tech{syntax object} is
predicated on the symbolic name of the @tech{identifier} as well as
the identifier's @tech{lexical information}; the same question with
the same @tech{lexical information} but different base value can
produce a different answer.

For example, combining the lexical information from @racket[let] in
the program above to @racket['x] would not produce an identifier that
is @racket[free-identifier=?] to either @racket[x], since it does not
appear in the scope of the @racket[x] binding. Combining the lexical
context of the @racket[6] with @racket['x], in contrast, would produce
an identifier that is @racket[bound-identifier=?] to both @racket[x]s.

The @racket[quote-syntax] form bridges the evaluation of a program and
the representation of a program. Specifically, @racket[(quote-syntax
_datum #:local)] produces a syntax object that preserves all of the
lexical information that @racket[_datum] had when it was parsed as
part of the @racket[quote-syntax] form. Note that
@racket[(quote-syntax _datum)] form is similar, but it removes certain
@tech{scopes} from the @racket[_datum]'s @tech{scope sets};
see @racket[quote-syntax] for more information.

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
the binding (according to @racket[free-identifier=?]) matters.}

@racketgrammar*[
#:literals (#%expression module module* #%plain-module-begin begin #%provide
            define-values define-syntaxes begin-for-syntax
            #%require #%declare
            #%plain-lambda case-lambda if begin begin0 let-values letrec-values
            set! quote-syntax quote with-continuation-mark
            #%plain-app #%top #%variable-reference)
[top-level-form general-top-level-form
                (#%expression expr)
                (module id module-path
                  (#%plain-module-begin
                   module-level-form ...))
                (begin top-level-form ...)
                (begin-for-syntax top-level-form ...)]
[module-level-form general-top-level-form
                   (#%provide raw-provide-spec ...)
                   (begin-for-syntax module-level-form ...)
                   submodule-form
                   (#%declare declaration-keyword ...)]
[submodule-form    (module id module-path
                     (#%plain-module-begin
                      module-level-form ...))
                   (module* id module-path 
                     (#%plain-module-begin
                      module-level-form ...))
                   (module* id #f 
                     (#%plain-module-begin
                      module-level-form ...))]
[general-top-level-form expr
                        (define-values (id ...) expr)
                        (define-syntaxes (id ...) expr)
                        (#%require raw-require-spec ...)]
[expr id
      (#%plain-lambda formals expr ...+)
      (case-lambda (formals expr ...+) ...)
      (if expr expr expr)
      (begin expr ...+)
      (begin0 expr expr ...)
      (let-values ([(id ...) expr] ...)
        expr ...+)
      (letrec-values ([(id ...) expr] ...)
        expr ...+)
      (set! id expr)
      (@#,racket[quote] datum)
      (quote-syntax datum)
      (quote-syntax datum #:local)
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
is significant. For example, the second case for @racket[_expr] is a
@tech{syntax-object} list whose first element is an @tech{identifier},
where the @tech{identifier}'s @tech{lexical information} specifies a
binding to the @racket[#%plain-lambda] of the
@racketmodname[racket/base] language (i.e., the @tech{identifier} is
@racket[free-identifier=?] to one whose binding is
@racket[#%plain-lambda]). In all cases, identifiers above typeset as
syntactic-form names refer to the bindings defined in
@secref["syntax"].

In a fully expanded program for a namespace whose @tech{base phase} is
0, the relevant @tech{phase level} for a binding in the program is
@math{N} if the bindings has @math{N} surrounding
@racket[begin-for-syntax] and @racket[define-syntaxes] forms---not
counting any @racket[begin-for-syntax] forms that wrap a
@racket[module] or @racket[module*] form for the body of the @racket[module]
or @racket[module*], unless a @racket[module*] form as @racket[#f] in place
of a @racket[_module-path] after the @racket[_id]. The
@racket[_datum] in a @racket[quote-syntax] form
preserves its information for all @tech{phase level}s.

A reference to a @tech{local binding} in a fully expanded program has
a @tech{scope set} that matches its binding identifier exactly.
Additional @tech{scopes}, if any, are removed. As a result,
@racket[bound-identifier=?] can be used to correlate local binding
identifiers with reference identifiers, while
@racket[free-identifier=?] must be used to relate references to
@tech{module bindings} or @tech{top-level bindings}.

In addition to the grammar above, @racket[#%expression] can appear in
a fully local-expanded expression position. For example,
@racket[#%expression] can appear in the result from
@racket[local-expand] when the stop list is empty.
Reference-identifier @tech{scope sets} are reduced in local-expanded
expressions only when the @racket[local-expand] stop list is empty.

@history[#:changed "6.3" @elem{Added the @racket[#:local] variant of
                               @racket[quote-syntax]; removed
                               @racket[letrec-syntaxes+values] from
                               possibly appearing in a fully
                               local-expanded form.}]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "expand-steps"]{Expansion Steps}

In a recursive expansion, each single step in expanding a @tech{syntax
object} at a particular @tech{phase level} depends on the immediate shape of
the @tech{syntax object} being expanded:

@itemize[

 @item{If it is an @tech{identifier} (i.e., a syntax-object symbol),
       then a @tech{binding} is determined by the @tech{identifier}'s
       @tech{lexical information}. If the @tech{identifier} has a
       @tech{binding}, that @tech{binding} is used to continue. If the @tech{identifier}
       is @tech{unbound}, a new @tech{syntax-object} symbol
       @racket['#%top] is created using the @tech{lexical information}
       of the @tech{identifier}; if this @racketidfont{#%top}
       @tech{identifier} has no @tech{binding}, then parsing fails with an
       @racket[exn:fail:syntax] exception. Otherwise, the new
       @tech{identifier} is combined with the original
       @tech{identifier} in a new @tech{syntax-object} pair (also
       using the same @tech{lexical information} as the original
       @tech{identifier}), and the @racketidfont{#%top} @tech{binding}
       is used to continue.

       @history[#:changed "6.3" @elem{Changed the introduction of
                                      @racket[#%top] in a top-level context
                                      to @tech{unbound} identifiers only.}]}

 @item{If it is a @tech{syntax-object} pair whose first element is an
      @tech{identifier}, and if the @tech{identifier} has a
      @tech{binding} other than as a @tech{top-level variable}, then
      the @tech{identifier}'s @tech{binding} is used to continue.}

 @item{If it is a @tech{syntax-object} pair of any other form, then a
       new @tech{syntax-object} symbol @racket['#%app] is created
       using the @tech{lexical information} of the pair. If the
       resulting @racketidfont{#%app} @tech{identifier} has no
       binding, parsing fails with an @racket[exn:fail:syntax]
       exception. Otherwise, the new @tech{identifier} is combined
       with the original pair to form a new @tech{syntax-object} pair
       (also using the same @tech{lexical information} as the original
       pair), and the @racketidfont{#%app} @tech{binding} is used to
       continue.}

 @item{If it is any other syntax object, then a new
       @tech{syntax-object} symbol @racket['#%datum] is created using
       the @tech{lexical information} of the original @tech{syntax
       object}. If the resulting @racketidfont{#%datum}
       @tech{identifier} has no @tech{binding}, parsing fails with an
       @racket[exn:fail:syntax] exception. Otherwise, the new
       @tech{identifier} is combined with the original @tech{syntax
       object} in a new @tech{syntax-object} pair (using the same
       @tech{lexical information} as the original pair), and the
       @racketidfont{#%datum} @tech{binding} is used to continue.}

]

Thus, the possibilities that do not fail lead to an @tech{identifier}
with a particular @tech{binding}. This binding refers to one of three
things:

@itemize[

 @item{A @deftech{transformer}, such as introduced by
       @racket[define-syntax] or @racket[let-syntax]. If the
       associated value is a procedure of one argument, the procedure
       is called as a @tech{syntax transformer} (described below), and
       parsing starts again with the @tech{syntax-object} result. If
       the @tech{transformer} binding is to any other kind of value,
       parsing fails with an @racket[exn:fail:syntax] exception. The
       call to the @tech{syntax transformer} is @racket[parameterize]d
       to set @racket[current-namespace] to a @tech{namespace} that
       shares @tech{bindings} and @tech{variables} with the namespace
       being used to expand, except that its @tech{base phase} is one
       greater.}

 @item{A @tech{variable} @tech{binding}, such as introduced by a
       module-level @racket[define] or by @racket[let]. In this case,
       if the form being parsed is just an @tech{identifier}, then it
       is parsed as a reference to the corresponding
       @tech{variable}. If the form being parsed is a
       @tech{syntax-object} pair, then an @racket[#%app] is added to
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
different @tech{contexts}. For example, a @racket[module] form is
allowed only in a @tech{top-level context}, and it fails in other
contexts. The possible @tech{contexts} are as follows:

@itemize[

 @item{@deftech{top-level context} : outside of any module, definition, or
       expression, except that sub-expressions of a top-level
       @racket[begin] form are also expanded as top-level forms.}

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
@tech{contexts}. For example, a @racket[let] form always parses the
right-hand expressions of a binding in an @tech{expression context},
but it starts parsing the body in an @tech{internal-definition
context}.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "intro-binding"]{Introducing Bindings}

@tech{Bindings} are introduced during @tech{expansion} when certain
core syntactic forms are encountered:

@itemize[

 @item{When a @racket[require] form is encountered at the top level or
       module level, each symbol specified by the form is paired with the
       @tech{scope set} of the specification to introduce new bindings.
       If not otherwise indicated in the
       @racket[require] form, bindings are introduced at the
       @tech{phase level}s specified by the exporting modules:
       @tech{phase level} 0 for each normal @racket[provide],
       @tech{phase level} 1 for each @racket[for-syntax]
       @racket[provide], and so on. The @racket[for-meta]
       @racket[provide] form allows exports at an arbitrary
       @tech{phase level} (as long as a binding exists within the
       module at the @tech{phase level}).

       A @racket[for-syntax] sub-form within @racket[require] imports
       similarly, but the resulting bindings have a @tech{phase level}
       that is one more than the exported @tech{phase levels}, when
       exports for the @tech{label phase level} are still imported at
       the @tech{label phase level}. More generally, a
       @racket[for-meta] sub-form within @racket[require] imports with
       the specified @tech{phase level} shift; if the specified shift
       is @racket[#f], or if @racket[for-label] is used to import,
       then all bindings are imported into the @tech{label phase
       level}.}

 @item{When a @racket[define], @racket[define-values],
       @racket[define-syntax], or @racket[define-syntaxes] form is
       encountered at the top level or module level, a binding is
       added @tech{phase level} 0 (i.e., the @tech{base environment}
       is extended) for each defined identifier.}

 @item{When a @racket[begin-for-syntax] form is encountered at the top
       level or module level, bindings are introduced as for
       @racket[define-values] and @racket[define-syntaxes], but at
       @tech{phase level} 1 (i.e., the @tech{transformer environment}
       is extended). More generally, @racket[begin-for-syntax] forms
       can be nested, an each @racket[begin-for-syntax] shifts its
       body definition by one @tech{phase level}.}

 @item{When a @racket[let-values] form is encountered, the body of the
       @racket[let-values] form is extended (by creating new
       @tech{syntax objects}) with a fresh @tech{scope}. The @tech{scope} is added to the identifiers
       themselves, so that the identifiers in binding position are
       @racket[bound-identifier=?] to uses in the fully expanded form,
       and so they are not @racket[bound-identifier=?] to other
       identifiers. The new bindings are at the
       @tech{phase level} at which the @racket[let-values] form is
       expanded.}

 @item{When a @racket[letrec-values] or
       @racket[letrec-syntaxes+values] form is encountered, bindings
       are added as for @racket[let-values], except that the
       right-hand-side expressions are also extended with the
       new @tech{scope}.}

 @item{Definitions in @tech{internal-definition contexts} introduce
       new scopes and bindings as described in @secref["intdef-body"].}

]

For example, in

@racketblock[
(let-values ([(x) 10]) (+ x y))
]

the binding introduced for @racket[x] applies to the @racket[x] in the
body, because a fresh @tech{scope} is created and added to both the binding
@racket[x] and reference @racket[x]. The same scope is added to the
@racket[y], but since it has a different symbol than the binding
@racket[x], it does not refer to the new binding. Any @racket[x]
outside of this @racket[let-values] form does not receive the fresh
@tech{scope} and therefore does not refer to the new binding.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "transformer-model"]{Transformer Bindings}

In a @tech{top-level context} or @tech{module context}, when the
expander encounters a @racket[define-syntaxes] form, the binding that
it introduces for the defined identifiers is a @tech{transformer}
binding. The @tech{value} of the @tech{binding} exists at expansion
time, rather than run time (though the two times can overlap), though
the binding itself is introduced with @tech{phase level} 0 (i.e., in
the @tech{base environment}).

The @tech{value} for the binding is obtained by evaluating the
expression in the @racket[define-syntaxes] form. This expression must
be @tech{expand}ed (i.e., parsed) before it can be evaluated, and it is
expanded at @tech{phase level} 1 (i.e., in the @tech{transformer
environment}) instead of @tech{phase level} 0.

If the resulting @tech{value} is a procedure of one argument or
the result of @racket[make-set!-transformer] on a procedure, then it
is used as a @deftech{syntax transformer} (a.k.a. @deftech{macro}).
The procedure is expected to accept a syntax object and return a
syntax object. A use of the binding (at @tech{phase level} 0) triggers
a call of the @tech{syntax transformer} by the expander; see
@secref["expand-steps"].

Before the expander passes a @tech{syntax object} to a transformer,
the @tech{syntax object} is extended with a fresh @tech{scope} (that
applies to all sub-@tech{syntax objects}) to distinguish @tech{syntax objects}
at the macro's use site from @tech{syntax objects} that are introduced by the macro;
in the result of the transformer the presence of the @tech{scope} is
flipped, so that introduced @tech{syntax objects} retain the @tech{scope},
and use-site @tech{syntax objects} do not have it. In addition, if
the use of a transformer is in the same definition context as its binding,
the use-site @tech{syntax object} is extended with an additional fresh
@deftech{use-site scope} that is not flipped in the transformer's result,
so that only use-site @tech{syntax objects} have the @tech{use-site scope}.

The @tech{scope}-introduction process for macro expansion helps keep
binding in an expanded program consistent with the lexical structure
of the source program. For example, the expanded form of the program

@racketblock[
(define x 12)
(define-syntax m
  (syntax-rules ()
    [(_ id) (let ([x 10]) id)]))
(m x)
]

is

@racketblock[
(define x 12)
(define-syntax m ....)
(let-values ([(x) 10]) x)
]

However, the result of the last expression is @racket[12], not
@racket[10]. The reason is that the transformer bound to @racket[m]
introduces the binding @racket[x], but the referencing @racket[x] is
present in the argument to the transformer. The introduced @racket[x]
is left with one fresh @tech{scope}, while the reference @racket[x] has a different fresh @tech{scope},
so the binding @racket[x] is not @racket[bound-identifier=?] to the
body @racket[x].

A @tech{use-site scope} on a binding identifier is ignored when the
definition is in the same context where the @tech{use-site scope} was
introduced. This special treatment of @tech{use-site scopes} allows a
macro to expand to a visible definition. For example, the expanded
form of the program

@racketblock[
(define-syntax m
  (syntax-rules ()
    [(_ id) (define id 5)]))
(m x)
x
]

is

@racketblock[
(define-syntax m ....)
(define x 5)
x
]

where the @racket[x] in the @racket[define] form has a @tech{use-site
scope} that is not present on the final @racket[x]. The final
@racket[x] nevertheless refers to the definition, because the
@tech{use-site scope} is effectively removed before installing the
definition's binding. In contrast, the expansion of

@racketblock[
(define-syntax m
  (syntax-rules ()
    [(_ id) (let ([x 4])
              (let ([id 5])
                x))]))
(m x)
]

is

@racketblock[
(define-syntax m ....)
(let ([x 4])
  (let ([x 5])
    x))
]

where the second @racket[x] has a @tech{use-site scope} that prevents
it from binding the final @racket[x]. The @tech{use-site scope} is not
ignored in this case, because the binding is not part of the definition
context where @racket[(m x)] was expanded.

The @racket[set!] form works with the @racket[make-set!-transformer]
and @racket[prop:set!-transformer] property to support
@deftech{assignment transformers} that transform @racket[set!]
expressions. An @tech{assignment transformer} contains a procedure
that is applied by @racket[set!] in the same way as a normal
transformer by the expander.

The @racket[make-rename-transformer] procedure or
@racket[prop:rename-transformer] property creates a value that is also
handled specially by the expander and by @racket[set!] as a
transformer binding's value. When @racket[_id] is bound to a
@deftech{rename transformer} produced by
@racket[make-rename-transformer], it is replaced with the target
identifier passed to @racket[make-rename-transformer]. In addition, as
long as the target identifier does not have a true value for the
@racket['not-free-identifier=?] @tech{syntax property}, the
binding table is extended to indicate that @racket[_id] is an alias
for the identifier in the @tech{rename transformer}. The
@racket[free-identifier=?] function follows aliasing chains to determine
equality of bindings, the @racket[identifier-binding] function
similarly follows aliasing chains, and the @racket[provide] form
exports @racket[_id] as the target identifier. Finally, the
@racket[syntax-local-value] function follows @tech{rename transformer}
chains even when binding aliases are not installed.

In addition to using scopes to track introduced identifiers, the
expander tracks the expansion history of a form through @tech{syntax
properties} such as @racket['origin]. See @secref["stxprops"] for
more information.

Finally, the expander uses a @tech{tamper status} to control the way
that unexported and protected @tech{module bindings} are used. See
@secref["stxcerts"] for more information on a @tech{tamper status}.

The expander's handling of @racket[letrec-syntaxes+values] is similar
to its handling of @racket[define-syntaxes]. A
@racket[letrec-syntaxes+values] can be expanded in an arbitrary phase
level @math{n} (not just 0), in which case the expression for the
@tech{transformer} binding is expanded at @tech{phase level} @math{n+1}.

The expressions in a @racket[begin-for-syntax] form are expanded and
evaluated in the same way as for @racket[define-syntaxes]. However,
any introduced bindings from definition within
@racket[begin-for-syntax] are at @tech{phase level} 1 (not a
@tech{transformer} binding at @tech{phase level} 0).

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "partial-expansion"]{Partial Expansion}

In certain contexts, such as an @tech{internal-definition context} or
@tech{module context}, @deftech{partial expansion} is used to determine
whether forms represent definitions, expressions, or other declaration
forms. Partial expansion works by cutting off the normal recursive
expansion when the relevant binding is for a primitive syntactic form.

As a special case, when expansion would otherwise add an
@racketidfont{#%app}, @racketidfont{#%datum}, or @racketidfont{#%top}
identifier to an expression, and when the binding turns out to be the
primitive @racket[#%app], @racket[#%datum], or @racket[#%top] form,
then expansion stops without adding the identifier.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "intdef-body"]{Internal Definitions}

An @tech{internal-definition context} supports local definitions mixed
with expressions. Forms that allow internal definitions document such
positions using the @racket[_body] meta-variable. Definitions in an
internal-definition context are equivalent to local binding via
@racket[letrec-syntaxes+values]; macro expansion converts internal
definitions to a @racket[letrec-syntaxes+values] form.

Expansion of an internal-definition context begins with the
introduction of a fresh @tech{scope} for the context. Thereafter,
expansion relies on @tech{partial expansion} of each @racket[_body] in
an internal-definition sequence. Partial expansion of each
@racket[_body] produces a form matching one of the following cases:

@itemize[

 @item{A @racket[define-values] form: The binding table is immediately enriched
       with bindings for the @racket[define-values] form.  Further
       expansion of the definition is deferred, and partial expansion
       continues with the rest of the body.}

 @item{A @racket[define-syntaxes] form: The right-hand side is
       expanded and evaluated (as for a
       @racket[letrec-syntaxes+values] form), and a transformer
       binding is installed for the body sequence before partial
       expansion continues with the rest of the body.}

 @item{A primitive expression form other than @racket[begin]: Further
       expansion of the expression is deferred, and partial expansion
       continues with the rest of the body.}

 @item{A @racket[begin] form: The sub-forms of the @racket[begin] are
       spliced into the internal-definition sequence, and partial
       expansion continues with the first of the newly-spliced forms
       (or the next form, if the @racket[begin] had no sub-forms).}

]

After all body forms are partially expanded, if no definitions were
encountered, then the expressions are collected into a @racket[begin]
form as the internal-definition context's expansion.  Otherwise, at
least one expression must appear after the last definition, and any
@racket[_expr] that appears between definitions is converted to
@racket[(define-values () (begin _expr (values)))]; the definitions
are then converted to bindings in a @racket[letrec-syntaxes+values]
form, and all expressions after the last definition become the body of
the @racket[letrec-syntaxes+values] form.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "mod-parse"]{Module Phases and Visits}

A @racket[require] form not only introduces @tech{bindings} at
expansion time, but also @deftech{visits} the referenced module when
it is encountered by the expander. That is, the expander instantiates
any variables defined in the module within @racket[begin-for-syntax],
and it also evaluates all expressions for @racket[define-syntaxes]
@tech{transformer} bindings.

Module @tech{visits} propagate through @racket[require]s in the same
way as module @tech{instantiation}. Moreover, when a module is
@tech{visit}ed at @tech{phase} 0, any module that it @racket[require]s
@racket[for-syntax] is @tech{instantiate}d at @tech{phase} 1, while
further @racket[require]s @racket[for-template] leading back
to @tech{phase} 0 causes the required module to be visited at
@tech{phase} 0 (i.e., not @tech{instantiate}d).

During compilation, the top-level of module context is itself
implicitly @tech{visit}ed. Thus, when the expander encounters
@racket[(require (for-syntax ....))], it immediately
@tech{instantiate}s the required module at @tech{phase} 1, in addition
to adding bindings at @tech{phase level} 1 (i.e., the
@tech{transformer environment}). Similarly, the expander immediately
evaluates any form that it encounters within
@racket[begin-for-syntax].

@tech{Phases} beyond 0 are @tech{visit}ed on demand. For example,
when the right-hand side of a @tech{phase}-0 @racket[let-syntax] is to
be expanded, then modules that are @tech{available} at @tech{phase} 1
are visited. More generally, initiating expansion at @tech{phase}
@math{n} @tech{visit}s modules at @tech{phase} @math{n}, which in turn
@tech{instantiates} modules at @tech{phase} @math{n+1}. These
@tech{visits} and @tech{instantiations} apply to @tech{available}
modules in the enclosing @tech{namespace}'s @tech{module registry};
a per-registry lock prevents multiple threads from concurrently
instantiating and visiting available modules.

When the expander encounters @racket[require] and @racket[(require
(for-syntax ....))] within a @tech{module context}, the resulting
@tech{visits} and @tech{instantiations} are specific to the expansion
of the enclosing module, and are kept separate from @tech{visits} and
@tech{instantiations} triggered from a @tech{top-level context} or
from the expansion of a different module. Along the same lines, when a
module is attached to a namespace through
@racket[namespace-attach-module], modules that it @racket[require]s
are transitively attached, but instances are attached only at
phases at or below the namespace's @tech{base phase}.

@;------------------------------------------------------------------------
@subsection[#:tag "macro-introduced-bindings"]{Macro-Introduced Bindings}

When a top-level definition binds an identifier that originates from a
 macro expansion, the definition captures only uses of the identifier
 that are generated by the same expansion due to the fresh @tech{scope}
 that is generated for the expansion.

@examples[
(define-syntax def-and-use-of-x
  (syntax-rules ()
    [(def-and-use-of-x val)
     (code:comment @#,t{@racket[x] below originates from this macro:})
     (begin (define x val) x)]))
(define x 1)
x
(def-and-use-of-x 2)
x

(define-syntax def-and-use
  (syntax-rules ()
    [(def-and-use x val)
     (code:comment @#,t{@racket{x} below was provided by the macro use:})
     (begin (define x val) x)]))
(def-and-use x 3)
x
]

For a top-level definition (outside of a module), the order of
 evaluation affects the binding of a generated definition for a
 generated identifier use. If the use precedes the definition, then
 the use is resolved with the bindings that are in place that at
 point, which will not be a macro-generated binding.
 (No such dependency on order occurs
 within a module, since a module binding covers the entire module
 body.) To support the declaration of an identifier before its use,
 the @racket[define-syntaxes] form avoids binding an identifier if the
 body of the @racket[define-syntaxes] declaration produces zero
 results.

@examples[
#:eval racket-eval
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
       (code:comment @#,t{Initial reference to @racket[even] precedes definition:})
       (define (odd x) (if (zero? x) #f (even (sub1 x))))
       (define (even x) (if (zero? x) #t (odd (sub1 x))))
       (odd 17))]))
(defs-and-uses/fail)

(define-syntax defs-and-uses
  (syntax-rules ()
    [(def-and-use)
     (begin
       (code:comment @#,t{Declare before definition via no-values @racket[define-syntaxes]:})
       (define-syntaxes (odd even) (values))
       (define (odd x) (if (zero? x) #f (even (sub1 x))))
       (define (even x) (if (zero? x) #t (odd (sub1 x))))
       (odd 17))]))
(defs-and-uses)
]

Macro-generated @racket[require] and @racket[provide]
 clauses also introduce and reference generation-specific bindings
 (due to the added @tech{scope}) with the same ordering effects as
 for definitions. The bindings depend on the @tech{scope set} attached
 to specific parts of the form:

@itemize[

 @item{In @racket[require], for a @racket[_require-spec] of the form
 @racket[(rename-in [_orig-id _bind-id])] or @racket[(only-in
 .... [_orig-id _bind-id])], the @racket[_bind-id] supplies the
 @tech{scope set} for the binding. In @racket[require] for other
 @racket[_require-spec]s, the generator of the @racket[_require-spec]
 determines the @tech{scope set}.}

 @item{In @racket[provide], for a @racket[_provide-spec] of the form
 @racket[_id], the exported identifier is the one that binds
 @racket[_id], but the
 external name is the plain, symbolic part of @racket[_id]. The exceptions for
 @racket[all-except-out] are similarly determined, as is the @racket[_orig-id] binding of a
 @racket[rename-out] form, and plain symbols are used for the
 external names. For @racket[all-defined-out], only identifiers with
 definitions having only the scopes of
 @racket[(all-defined-out)] form are exported; the external name is
 the plain symbol from the definition.}

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
example, the @racket[eval] procedure takes a syntax object and expands
it, compiles it, and evaluates it.

@;------------------------------------------------------------------------
@section[#:tag "namespace-model"]{Namespaces}

@margin-note/ref{See @secref["Namespaces"] for functions that
manipulate namespaces.}

A @deftech{namespace} is both a starting point for parsing and a
starting point for running @tech{compiled} code. A @tech{namespace}
also has a @deftech{module registry} that maps module names to module
declarations (see @secref["module-eval-model"]). This registry is
shared by all @tech{phase level}s, and it applies both to parsing and
to running @tech{compiled} code.

As a starting point for parsing, a @tech{namespace} provides scopes
(one per @tech{phase level}, plus one that spans all @tech{phase
levels}). Operations such as @racket[namespace-require] create initial
@tech{bindings} using the namespace's @tech{scopes}, and the further
expansion and evaluation in the namespace can create additional
@tech{bindings}. Evaluation of a form with a namespace always adds the
namespace's phase-specific @tech{scopes} to the form and to any result
of expanding the top-level form; as a result, every binding identifier
has at least one @tech{scope}. The namespace's additional scope, which
is added at all @tech{phase levels}, is added only on request (e.g.,
by using @racket[eval] as opposed to @racket[eval-syntax]). Except for
namespaces generated by a module (see @racket[module->namespace]),
every namespace uses the same @tech{scope} as the one added to all
@tech{phase levels}, while the @tech{scopes} specific to a @tech{phase
level} are always distinct.

As a starting point evaluating @tech{compiled} code, each namespace
encapsulates a distinct set of top-level variables at various
@tech{phases}, as well as a potentially distinct set of module
instances in each @tech{phase}. That is, even though module
declarations are shared for all @tech{phase levels}, module instances
are distinct for each @tech{phase}. Each namespace has a @deftech{base
phase}, which corresponds to the phase used by reflective operations
such as @racket[eval] and @racket[dynamic-require]. In particular,
using @racket[eval] on a @racket[require] form @tech{instantiates} a
module in the namespace's @tech{base phase}.

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
(code:comment @#,t{The following @racket[let] expression is compiled in the original})
(code:comment @#,t{namespace, so direct references to @racket[x] see @racket['orig].})
(code:line
 (let ([n (make-base-namespace)]) (code:comment @#,t{make new namespace})
   (parameterize ([current-namespace n]) 
     (eval '(define x 'new)) (code:comment @#,t{evals in the new namespace})
     (display x) (code:comment @#,t{displays @racket['orig]})
     (display (eval 'x)))) (code:comment @#,t{displays @racket['new]}))
]

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
(module m racket (define x 8) (provide x))
(require 'm)
(eval:alts x (eval 'x))
(f)
]

Like a top-level @tech{namespace}, each @racket[module] form has an
associated @tech{scope} to span all @tech{phase levels} of the
module's content, plus a @tech{scope} at each @tech{phase level}. The
latter is added to every form, original or appearing through partial
macro expansion, within the module's immediate body. Those same scopes
are propagated to a namespace created by @racket[module->namespace]
for the module. Meanwhile, parsing of a @racket[module] form begins by
removing the all scopes that correspond to the enclosing top-level or
(in the case of @tech{submodules}) @racket[module] and
@racket[module*] forms.

@;------------------------------------------------------------------------
@section[#:tag "infernames"]{Inferred Value Names}

To improve error reporting, names are inferred at compile-time for
certain kinds of values, such as procedures. For example, evaluating
the following expression:

@racketblock[
(let ([f (lambda () 0)]) (f 1 2 3))
]

produces an error message because too many arguments are provided to
the procedure. The error message is able to report @racketidfont{f} as
the name of the procedure. In this case, Racket decides, at
compile-time, to name as @racket['f] all procedures created by the
@racket[let]-bound @racket[lambda].


@margin-note/ref{See @racket[procedure-rename] to override a procedure's
inferred name at runtime.}
Names are inferred whenever possible for procedures. Names closer to
an expression take precedence. For example, in

@racketblock[
(define my-f
  (let ([f (lambda () 0)]) f))
]

the procedure bound to @racket[my-f] will have the inferred name
@racket['f].

When an @indexed-racket['inferred-name] property is attached to a
syntax object for an expression (see @secref["stxprops"]), the
property value is used for naming the expression, and it overrides any
name that was inferred from the expression's context. Normally, the
property value should be a symbol. A @racket['inferred-name] 
property value of @|void-const| hides a name that would otherwise be
inferred from context (perhaps because a binding identifier's was 
automatically generated and should not be exposed).

To support the propagation and merging of consistent properties during
expansions, the value of the @racket['inferred-name] property can be a
tree formed with @racket[cons] where all of the leaves are the same.
For example, @racket[(cons 'name 'name)] is equivalent to
@racket['name], and @racket[(cons (void) (void))] is equivalent to
@|void-const|.

When an inferred name is not available, but a source location is
available, a name is constructed using the source location
information. Inferred and property-assigned names are also available
to syntax transformers, via @racket[syntax-local-name].

@;----------------------------------------
@section[#:tag "cross-phase persistent-grammar"]{Cross-Phase Persistent Module Declarations}

A module is @tech{cross-phase persistent} only if it fits the following grammar,
which uses non-terminals from @secref["fully-expanded"], only if
it includes @racket[(#%declare #:cross-phase-persistent)], only
it includes no uses of @racket[quote-syntax] or @racket[#%variable-reference], 
and only if no module-level binding is @racket[set!]ed.

@racketgrammar*[
#:literals (module module* #%plain-module-begin begin #%provide
            define-values #%require #%declare
            #%plain-lambda case-lambda begin
            set! quote-syntax quote with-continuation-mark
            #%plain-app
            cons list make-struct-type make-struct-type-property
            gensym string->uninterned-symbol)
[cross-module (module id module-path
                (#%plain-module-begin
                  cross-form ...))]
[cross-form     (#%declare #:cross-phase-persistent)
                (begin cross-form ...)
                (#%provide raw-provide-spec ...)
                submodule-form
                (define-values (id ...) cross-expr)
                (#%require raw-require-spec ...)]
[cross-expr id
                (@#,racket[quote] cross-datum)
                (#%plain-lambda formals expr ...+)
                (case-lambda (formals expr ...+) ...)
                (#%plain-app cons cross-expr ...+)
                (#%plain-app list cross-expr ...+)
                (#%plain-app make-struct-type cross-expr ...+)
                (#%plain-app make-struct-type-property
                             cross-expr ...+)
                (#%plain-app gensym)
                (#%plain-app gensym string)
                (#%plain-app string->uninterned-symbol string)]
[cross-datum     number
                 boolean
                 identifier
                 string
                 bytes]
]

This grammar applies after @tech{expansion}, but because a @tech{cross-phase persistent}
module imports only from other cross-phase persistent modules, the only relevant
expansion steps are the implicit introduction of
@racket[#%plain-module-begin], implicit introduction of @racket[#%plain-app],
and implicit introduction and/or expansion of @racket[#%datum].

@;----------------------------------------

@close-eval[racket-eval]
