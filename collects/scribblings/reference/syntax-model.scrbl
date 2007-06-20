#reader(lib "docreader.ss" "scribble")
@require[(lib "struct.ss" "scribble")]
@require-for-syntax[mzscheme]
@require["mz.ss"]

@;------------------------------------------------------------------------
@title{Syntax Model}

The syntax of a Scheme program is defined by

@itemize{

 @item{a @deftech{read} phase that processes a character stream into a
       Scheme value, especially one composed of pairs and symbols, and
       possibly with source-location information to form a
       @tech{syntax object}; and}

 @item{an @deftech{expand} phase that processes a syntax object to
       produce one that is fully parsed.}

}

For details on the @tech{read} phase, see @secref["mz:reader"]. Source
code is normally read in @scheme[read-syntax] mode. Otherwise, it must
be converted to syntax using @scheme[datum->syntax], because the
@tech{expand} phase is defined in terms of @tech{syntax objects}.

Expansion recursively processes a @tech{syntax object}. Binding
information on a syntax object drives the expansion process, and the
expansion process generates new syntax objects that are like old ones,
but enriched with new binding information.

@;------------------------------------------------------------------------
@section{Identifiers and Binding}

@guideintro["guide:binding"]{binding}

An @deftech{identifier} is source-program entity. Parsing a Scheme
program reveals that some @tech{identifiers} correspond to
@tech{variables}, some refer to syntactic forms, and some are quoted
to produce a symbol or a syntax object. An identifier @deftech{binds}
another (i.e., it is a @deftech{binding}) when the former is parsed as
a @tech{variable} and the latter is parsed as a reference to the
former. An @tech{identifier} is @deftech{bound} in a sub-expression if
it @tech{binds} any uses of the @tech{identifier} in the
sub-expression that are not otherwise @tech{bound} within the
sub-expression; conversely, a binding for a sub-expression
@deftech{shadows} any @tech{bindings} (i.e., it is
@deftech{shadowing}) in its context, so that uses of an
@tech{identifier} refer to the @tech{shadowing} @tech{binding}.  The
@deftech{environment} of a form is the set of bindings whose scope
includes the form.

For example, as a bit of source, the text

@schemeblock[(let ([x 5]) x)]

includes two @tech{identifiers}: @scheme[let] and @scheme[x] (which
appears twice). When this source is parsed in a typical
@tech{environment}, @scheme[x] turns out to represent a
@tech{variable} (unlike @scheme[let]). In particular, the first
@scheme[x] @tech{binds} the second @scheme[x].

Throughout the documentation, @tech{identifiers} are typeset to
suggest the way that they are parsed. A black, boldface
@tech{identifier} like @scheme[lambda] indicates as a reference to a
syntactic form. A plain blue @tech{identifier} like @schemeidfont{x}
is a @tech{variable} or a reference to an unspecified @tech{top-level
variable}. A hyperlinked @tech{identifier} @scheme[cons] is a
reference to a specific @tech{top-level variable}.

@;------------------------------------------------------------------------
@section{Syntax Objects}

A @deftech{syntax object} combines a simpler Scheme value, such as a
symbol or pair, which information about bindings and (optionally)
source-location information. In particular, an @tech{identifier} is
represented as a symbol object that combines a symbol and
lexical/source information.

For example, a @schemeidfont{car} @tech{identifier} might have lexical
information that designates it as the @scheme[car] from the
@schememodname[big] language (i.e., the built-in
@scheme[car]). Similarly, a @schemeidfont{lambda} identifier's lexical
information may indicate that it represents a procedure form. Some
other @tech{identifier}'s lexical information may indicate that it
references a @tech{top-level variable}.

When a @tech{syntax object} represents a more complex expression than
am identifier or simple constant, its internal pieces can be
extracted. Detailed information about binding is available mostly
indirectly. For example, two identifiers, perhaps extracted from a
larger expression, can be compared to see if they refer to the same
binding (i.e., @scheme[free-identifier=?]). A slightly different test
is whether each identifier would bind the other if one was in a
binding position and the other in an expression position (i.e.,
@scheme[bound-identifier=?]).

For example, the when the program written as

@schemeblock[(let ([x 5]) (+ x 6))]

is represented as a syntax object, then two @tech{syntax objects} can
be extracted for the two @scheme[x]s. Both the
@scheme[free-identifier=?] and @scheme[bound-identifier=?] predicates
will indicate that the @scheme[x]s are the same. In contrast, the
@scheme[let] identifier is not @scheme[free-identifier=?] or
@scheme[bound-identifier=?] to either @scheme[x].

The lexical information in a syntax object is independent of the other
half, and it can be transferred to a new syntax object, combined with
an arbitrary other Scheme value. Thus, identifier-binding information
in a syntax object is predicated on the symbolic name of the
identifier; the same question with the same lexical information but
different base value can produce a different answer.

For example, combining the lexical information from @scheme[let] in
the program above to @scheme['x] would not produce an identifier that
is @scheme[free-identifier=?] to either @scheme[x], since it does not
appear in the scope of the @scheme[x] binding. Combining the lexical
context of the @scheme[6] with @scheme['x], in contrast, would produce
an identifier that is @scheme[bound-identifier=?] to both @scheme[x]s.

The @scheme[quote-syntax] form bridges the evaluation of a program and
the representation of a program. Specifically, @scheme[(quote-syntax
_datum)] produces a syntax object that preserves all of the lexical
information that @scheme[_datum] had when it was parsed as part of a
@scheme[quote-syntax] form.

@;------------------------------------------------------------------------
@section[#:tag "mz:expansion"]{Expansion@aux-elem{ (Parsing)}}

Expansion recursively processes a @tech{syntax object}. Binding
information on a syntax object drives the expansion process, and the
expansion process generates new syntax objects that are like old ones,
but enriched with new binding information.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "mz:fully-expanded"]{Fully Expanded Programs}

A complete expansion produces a @tech{syntax object} matching the
following grammar:

@schemegrammar*[
#:literals (#%expression module #%plain-module-begin begin provide
            define-values define-syntaxes define-values-for-syntax
            require require-for-syntax require-for-template
            #%plain-lambda case-lambda if begin begin0 let-values letrec-values
            set! quote-syntax quote with-continuation-mark
            #%plain-app #%datum #%top #%variable-reference)
[top-level-form general-top-level-form
                (#%expression expr)
                (module id name-id 
                  (#%plain-module-begin 
                   module-level-form ...))
                (begin top-level-form ...)]
[module-level-form general-top-level-form
                   (provide provide-spec ...)]
[general-top-level-form expr
                        (define-values (id ...) expr)
                        (define-syntaxes (id ...) expr)
                        (define-values-for-syntax (id ...) expr)
                        (require require-spec ...)
                        (require-for-syntax require-spec ...)
                        (require-for-template require-spec ...)]
[expr id
      (#%plain-lambda formals expr ...+)
      (case-lambda (formals expr ...+) ...)
      (if expr expr)
      (if expr expr expr)
      (begin expr ...+)
      (begin0 expr expr ...)
      (let-values (((id ...) expr) ...) 
        expr ...+)
      (letrec-values (((id ...) expr) ...) 
        expr ...+)
      (set! id expr)
      (#, @scheme[quote] datum)
      (quote-syntax datum)
      (with-continuation-mark expr expr expr)
      (#%plain-app expr ...+)
      (#%top . id)
      (#%variable-reference id)
      (#%variable-reference (#%top . id))]
[formals (id ...)
         (id ...+ . id)
         id]]

This fully-expanded @tech{syntax object} corresponds to a
@deftech{parse} of the expression (i.e., a @deftech{parsed}
expression), and lexical information on its @tech{identifiers}
indicates the @tech{parse}.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection{Expansion Steps}

A step in parsing a form represented as a syntax object depends on its
outermost shape:

@itemize{

 @item{If it is an @tech{identifier} (i.e., a syntax-object symbol), then a
       binding is determined using symbol along with the lexical
       information in the identifier. If the identifier has a binding
       other than as a top-level variable, it is used to continue. If
       the identifier has no binding, a new syntax-object symbol
       @scheme['#%top] is created using the lexical context of the
       identifier; if this @schemeidfont{#%top} identifier has no
       binding (other than as a top-level variable), then parsing
       fails with an @scheme[exn:fail:syntax] exception. Otherwise,
       the new identifier is combined with the original identifier in
       a new syntax-object pair (using the same context as the
       original identifier), and the @schemeidfont{#%top} binding is
       used to continue.}

 @item{If it is a syntax-object pair whose first element is an
      identifier, and if the identifier has a binding other than as a
      top-level variable, then the identifier's binding is used to
      continue.}

 @item{If it is a syntax-object pair of any other form, then a new
       syntax-object symbol @scheme['#%app] is created using the
       lexical context of the pair. If the resulting
       @schemeidfont{#%app} identifier has no binding, parsing fails
       with an @scheme[exn:fail:syntax] exception. Otherwise, the new
       identifier is combined with the original pair to form a new
       syntax-object pair (using the same context as the original
       pair), and the @schemeidfont{#%app} binding is used to
       continue.}

 @item{If it is any other syntax object, then a new syntax-object
       symbol @scheme['#%datum] is created using the lexical context
       of the original syntax object. If the resulting
       @schemeidfont{#%datum} identifier has no binding, parsing fails
       with an @scheme[exn:fail:syntax] exception. Otherwise, the new
       identifier is combined with the original syntax object in a new
       syntax-object pair (using the same context as the original
       pair), and the @schemeidfont{#%datum} binding is used to
       continue.}

}

Thus, the possibilities that do not fail lead to an identifier with a
particular binding. This binding refers to one of three things:

@itemize{

 @item{A transformer binding, such as introduced by
       @scheme[define-syntax] or @scheme[let-syntax]. If the
       associated value is to a procedure of one argument, the
       procedure is called as a syntax transformer (see
       @secref["transformers"]), and parsing starts again with the
       syntax-object result. If the transformer binding is to any
       other kind of value, parsing fails with an
       @scheme[exn:fail:syntax] exception.}

 @item{A variable binding, such as introduced by a module-level
       @scheme[define] or by @scheme[let]. In this case, if the form
       being parsed is just an identifier, then it is parsed as a
       run-time reference to the location of the corresponding
       variable. If the form being parsed is a syntax-object list,
       then an @scheme[#%app] is added to the front of the
       syntax-object list in the same way as when the first item in
       the syntax-object list is not an identifier (third case in the
       previous enumeration), and parsing continues.}

 @item{Core syntax, which is parsed as described in
       @secref["mz:syntax"]. Parsing core syntactic forms typically
       involves recursive parsing of sub-forms, and may introduce
       bindings that control the parsing of sub-forms.}

}

Each expansion step occurs in a particular context, and transformers
and core-syntax parsing can depend on the context. For example, a
@scheme[module] form is allowed only in a top-level context. The
possible contexts are as follows:

@itemize{

 @item{@defterm{top level} : outside of any module, definition, or
       expression, except that sub-expressions of a top-level
       @scheme[begin] form are also expanded as top-level forms.}

 @item{@defterm{module begin} : inside the body of a module, as the
       only form within the module.}

 @item{@defterm{module body} : in the body of a module (inside the
       moudule-begin layer).}

 @item{@defterm{internal definition} : in a nested context that allows
       both definitions and expressions.}

 @item{@defterm{expression} : in a context where only expressions are
       allowed.}

}

Different core syntax forms parse sub-forms in different contexts. For
example, a @scheme[let] form always parses the right-hand expressions
of a binding in an expression context, but it starts parsing the body
in an internal-definition context.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "mz:intdef-body"]{Internal Definitions}

An internal-definition context corresponds to a partial expansion
step. A form that supports internal definitions starts by expanding
its first form in an internal-definition context, but only
partially. That is, it recursively expands only until the form becomes
one of the following:

@itemize{

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
       side is executed and a transformer binding is installed for the
       body sequence before expansion continues.}

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

}

If the last expression form turns out to be a @scheme[define-values]
or @scheme[define-syntaxes] form, expansion fails with a syntax error.


@;------------------------------------------------------------------------
@section{Compilation}

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
@section{Namespaces}

A @deftech{namespace} is a top-level mapping from symbols to binding
information. It is the starting point for expanding an expression; a
@tech{syntax object} produced by @scheme[read-syntax] has no initial
lexical context; the @tech{syntax object} can be expanded after
initializing it with the mappings of a particular namespace. A
namespace is also the starting point evaluating expanded code, where
the first step in evaluation is linking the code to specific module
instances and top-level variables.

For expansion purposes, a namespace maps each symbol to one of three
possible bindings:

@itemize{

 @item{a particular module-level binding from a particular module}

 @item{a top-level transformer binding named by the symbol}

 @item{a top-level variable named by the symbol}

}

An ``empty'' namespace maps all symbols to top-level variables.
Certain evaluations extend a namespace for future expansions;
importing a module into the top-level adjusts the namespace bindings
for all of the imported named, and evaluating a top-level
@scheme[define] form updates the namespace's mapping to refer to a
variable (in addition to installing a value into the variable).

For evaluation, each namespace encapsulates a distinct set of
top-level variables, as well as a potentially distinct set of module
instances. After a namespace is created, module instances from
existing namespaces can be attached to the new namespace.  In terms of
the evaluation model, top-level variables from different namespaces
essentially correspond to definitions with different prefixes.
Furthermore, the first step in evaluating any compiled expression is
to link its top-level variable and module-level variable references to
specific variables in the namespace.

At all times during evaluation, some namespace is designated as the
@deftech{current namespace}. The current namespace has no particular
relationship, however, with the namespace that was used to expand the
code that is executing, or with the namespace that was used to link
the compiled form of the currently evaluating code. In particular,
changing the current namespace during evaluation does not change the
variables to which executing expressions refer. The current namespace
only determines the behavior of (essentially reflective) operations to
expand code and to start evaluating expanded/compiled code.

A namespace is purely a top-level entity, not to be confused with an
environment. In particular, a namespace does not encapsulate the full
environment of an expression inside local-binding forms.
