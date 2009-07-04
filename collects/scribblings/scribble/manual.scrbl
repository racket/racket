#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-syntax scheme/base)
          (for-label scribble/manual-struct))

@(define lit-ellipses (scheme ...))
@(define lit-ellipses+ (scheme ...+))

@title[#:tag "manual" #:style 'toc]{Manual Forms}

@defmodule[scribble/manual]{The @schememodname[scribble/manual]
library provides all of @schememodname[scribble/basic] plus additional
functions, including many that are relatively specific to writing PLT
Scheme documentation.

The @schememodname[scribble/manual] name can also be used as a
language with @hash-lang[]. It acts like the
@schememodname[scribble/doc] language, except that the
@schememodname[scribble/manual] library is also required into
the module.}

@local-table-of-contents[]

@; ------------------------------------------------------------------------
@section[#:tag "scribble:manual:code"]{Typesetting Code}

@defform[(schemeblock datum ...)]{

Typesets the @scheme[datum] sequence as a table of Scheme code inset
by two spaces. The source locations of the @scheme[datum]s determine
the generated layout. For example,

@schemeblock[
(schemeblock
 (define (loop x)
   (loop (not x))))
]

produces the output

@schemeblock[
(define (loop x)
  (loop (not x)))
]

with the @scheme[(loop (not x))] indented under @scheme[define],
because that's the way it is idented the use of @scheme[schemeblock].

Furthermore, @scheme[define] is typeset as a keyword (bold and black)
and as a hyperlink to @scheme[define]'s definition in the reference
manual, because this document was built using a for-label binding of
@scheme[define] (in the source) that matches a definition in the
reference manual. Similarly, @scheme[not] is a hyperlink to the its
definition in the reference manual.

Use @scheme[unsyntax] to escape back to an expression that produces an
@scheme[element]. For example,

@let[([unsyntax #f])
(schemeblock
 (schemeblock
   (+ 1 (unsyntax (elem (scheme x) (subscript "2"))))))
]

produces

@schemeblock[
(+ 1 (unsyntax (elem (scheme x) (subscript "2"))))
]

The @scheme[unsyntax] form is regonized via
@scheme[free-identifier=?], so if you want to typeset code that
includes @scheme[unsyntax], you can simply hide the usual binding:

@SCHEMEBLOCK[
(schemeblock
  (let ([(UNSYNTAX (scheme unsyntax)) #f])
    (schemeblock
      (syntax (+ 1 (unsyntax x))))))
]

Or use @scheme[SCHEMEBLOCK], whose escape form is @scheme[UNSYNTAX]
instead of @scheme[unsyntax].

A few other escapes are recognized symbolically:

@itemize[

 @item{@scheme[(#,(scheme code:line) _datum ...)] typesets as the
       sequence of @scheme[_datum]s (i.e., without the
       @scheme[code:line] wrapper).}

 @item{@scheme[(#,(scheme code:comment) _datum)] typesets like
       @scheme[_datum], but colored as a comment and prefixed with a
       semi-colon. A typical @scheme[_datum] escapes from
       Scheme-typesetting mode using @scheme[unsyntax] and
       produces a paragraph using @scheme[t]: 

       @verbatim[#:indent 2]|{
         (code:comment @#,t{this is a comment})
       }|

       (Note that @litchar|{@#,foo{...}}| reads as
       @SCHEME[(unsyntax (foo "..."))].)
       }

 @item{@schemeidfont{code:blank} typesets as a blank space.}

 @item{@schemeidfont{_}@scheme[_id] typesets as @scheme[id], but
       colored as a variable (like @scheme[schemevarfont]); this
       escape applies only if @schemeidfont{_}@scheme[_id] has no
       for-label binding and is not specifically colored as a subform
       non-terminal via @scheme[defform], a variable via
       @scheme[defproc], etc.}

]

See also @schememodname[scribble/comment-reader].
}

@defform[(SCHEMEBLOCK datum ...)]{Like @scheme[schemeblock], but with
the expression escape @scheme[UNSYNTAX] instead of @scheme[unsyntax].}

@defform[(schemeblock0 datum ...)]{Like @scheme[schemeblock], but
without insetting the code.}

@defform[(SCHEMEBLOCK0 datum ...)]{Like @scheme[SCHEMEBLOCK], but
without insetting the code.}

@defform[(schemeinput datum ...)]{Like @scheme[schemeblock], but the
@scheme[datum] are typeset after a prompt representing a REPL.}

@defform[(schememod lang datum ...)]{Like @scheme[schemeblock], but
the @scheme[datum] are typeset inside a @schememodfont{#lang}-form
module whose language is @scheme[lang]. The source location of
@scheme[lang] (relative to the body @scheme[datum]s) determines the
relative positioning of the @schememodfont{#lang} line in the typeset
output.}

@defform[(scheme datum ...)]{Like @scheme[schemeblock], but typeset on
a single line and wrapped with its enclosing paragraph, independent of
the formatting of @scheme[datum].}

@defform[(SCHEME datum ...)]{Like @scheme[scheme], but with the
@scheme[UNSYNTAX] escape like @scheme[schemeblock].}

@defform[(schemeresult datum ...)]{Like @scheme[scheme], but typeset
as a REPL value (i.e., a single color with no hyperlinks).}

@defform[(schemeid datum ...)]{Like @scheme[scheme], but typeset
as an unbound identifier (i.e., no coloring or hyperlinks).}

@defform*[((schememodname datum)
           (schememodname ((unsyntax (scheme unsyntax)) expr)))]{
Like @scheme[scheme], but typeset as a module path. If @scheme[datum]
is an identifier or @scheme[expr] produces a symbol, then it is
hyperlinked to the module path's definition as created by
@scheme[defmodule].}

@defproc[(litchar [str string?]) element?]{Typesets @scheme[str] as a
representation of literal text. Use this when you have to talk about
the individual characters in a stream of text, as as when documenting
a reader extension.}

@defproc[(verbatim [#:indent indent integer? 0] [str string?] ...)
         flow-element?]{Typesets @scheme[str]
as a table/paragraph in typewriter font with the linebreaks specified
by newline characters in @scheme[str]. ``Here strings'' are often
useful with @scheme[verbatim].}

@defproc[(schemefont [pre-content any/c] ...) element?]{Typesets
@tech{decode}d @scheme[pre-content] as uncolored, unhyperlinked
Scheme. This procedure is useful for typesetting things like
@schemefont{#lang}, which are not @scheme[read]able by themselves.}

@defproc[(schemevalfont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as a value.}

@defproc[(schemeresultfont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as a REPL result.}

@defproc[(schemeidfont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as an identifier.}

@defproc[(schemevarfont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as a variable (i.e., an argument or
sub-form in a procedure being documented).}

@defproc[(schemekeywordfont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as a syntactic form name.}

@defproc[(schemeparenfont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored like parentheses.}

@defproc[(schememetafont [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as meta-syntax, such as backquote or
unquote.}

@defproc[(schemeerror [pre-content any/c] ...) element?]{Like
@scheme[schemefont], but colored as error-message text.}

@defproc[(procedure [pre-content any/c] ...) element?]{Typesets
@tech{decode}d @scheme[pre-content] as a procedure name in a REPL
result (e.g., in typewriter font with a @litchar{#<procedure:} prefix
and @litchar{>} suffix.).}

@defform[(var datum)]{Typesets @scheme[datum] as an identifier that is
an argument or sub-form in a procedure being documented. Normally, the
@scheme[defproc] and @scheme[defform] arrange for @scheme[scheme] to
format such identifiers automatically in the description of the
procedure, but use @scheme[var] if that cannot work for some reason.}

@defform[(svar datum)]{Like @scheme[var], but for subform non-terminals
in a form definition.}

@; ------------------------------------------------------------------------

@subsection{Typesetting Comments}

@defmodulereader[scribble/comment-reader]

As a reader module, @schememodname[scribble/comment-reader] reads a
single S-expression that contains @litchar{;}-based comment lines, and
it wraps the comments with @scheme[code:comment] for use with forms
like @scheme[schemeblock]. More precisely,
@schememodname[scribble/comment-reader] extends the current reader to
adjust the parsing of @litchar{;}.

For example, within a Scribble document that imports
@schememodname[scribble/manual],

@verbatim[#:indent 2]|{
  @#reader scribble/comment-reader
   (schemeblock
    ;; This is not a pipe
    (make-pipe)
   )
}|

generates

@#reader scribble/comment-reader
 (schemeblock
  ;; This is not a pipe
  (make-pipe)
 )

The initial @litchar["@"] is needed above to shift into S-expression
mode, so that @schememetafont{#reader} is recognized as a reader
declaration instead of literal text. Also, the example uses
@scheme[(schemeblock ....)]  instead of
@schememetafont["@"]@scheme[schemeblock]@schememetafont["["]@scheme[....]@schememetafont["]"]
because the @"@"-reader would drop comments within the
@scheme[schemeblock] before giving
@schememodname[scribble/comment-reader] a chance to convert them.

@; ------------------------------------------------------------------------
@section[#:tag "doc-modules"]{Documenting Modules}

@defform/subs[(defmodule maybe-req id maybe-sources pre-flow ...)
              ([maybe-req code:blank
                          (code:line #:require-form expr)]
               [maybe-sources code:blank
                              (code:line #:use-sources (mod-path ...))])]{

Produces a sequence of flow elements (encaptured in a @scheme[splice])
to start the documentation for a module that can be @scheme[require]d
using the path @scheme[id]. The @tech{decode}d @scheme[pre-flow]s
introduce the module, but need not include all of the module content.

Besides generating text, this form expands to a use of
@scheme[declare-exporting] with @scheme[id]; the
@scheme[#:use-sources] clause, if provided, is propagated to
@scheme[declare-exporting]. Consequently, @scheme[defmodule] should be
used at most once in a section, though it can be shadowed with
@scheme[defmodule]s in sub-sections.

If a @scheme[#:require-form] clause is provided, the given expression
produces an element to use instead of @scheme[(scheme require)] for
the declaration of the module. This is useful to suggest a different
way of accessing the module instead of through @scheme[require].

Hyperlinks created by @scheme[schememodname] are associated with the
enclosing section, rather than the local @scheme[id] text.}


@defform[(defmodulelang id maybe-sources pre-flow ...)]{

Like @scheme[defmodule], but documents @scheme[id] as a module path
suitable for use by either @scheme[require] or @schememodfont{#lang}.}


@defform[(defmodulereader id maybe-sources pre-flow ...)]{

Like @scheme[defmodule], but documents @scheme[id] as a module path
suitable for use with @schememetafont{#reader}.}


@deftogether[(
@defform[(defmodule* maybe-req  (id ...+) maybe-sources pre-flow ...)]
@defform[(defmodulelang* (id ...+) maybe-sources pre-flow ...)]
@defform[(defmodulereader* (id ...+) maybe-sources pre-flow ...)]
)]{

Like @scheme[defmodule], etc., but introduces multiple module paths instead
of just one.}

@deftogether[(
@defform[(defmodule*/no-declare maybe-req (id ...) pre-flow ...)]
@defform[(defmodulelang*/no-declare (id ...) pre-flow ...)]
@defform[(defmodulereader*/no-declare (id ...) pre-flow ...)]
)]{

Like @scheme[defmodule*], etc., but without expanding to
@scheme[declare-exporting]. Use this form when you want to provide a
more specific list of modules (e.g., to name both a specific module
and one that combines several modules) via your own
@scheme[declare-exporting] declaration.}

@defform/subs[(declare-exporting mod-path ... maybe-sources)
              ([maybe-sources code:blank
                              (code:line #:use-sources (mod-path ...))])]{
                                 
Associates the @scheme[mod-path]s to all bindings defined within the
enclosing section, except as overridden by other
@scheme[declare-exporting] declarations in nested sub-sections.  The
list of @scheme[mod-path]s is shown, for example, when the user hovers
the mouse over one of the bindings defined within the section.

More significantly, the first @scheme[mod-path] plus the
@scheme[#:use-sources] @scheme[mod-path]s determine the binding that
is documented by each @scheme[defform], @scheme[defproc], or similar
form within the section that contains the @scheme[declare-exporting]
declaration:

@itemize[

 @item{If no @scheme[#:use-sources] clause is supplied, then the
       documentation applies to the given name as exported by the first
       @scheme[mod-path].}

 @item{If @scheme[#:use-sources] @scheme[mod-path]s are supplied, then
       they are tried in order. The first one to provide an export
       with the same symbolic name and
       @scheme[free-label-identifier=?] to the given name is used as
       the documented binding. This binding is assumed to be the same
       as the identifier as exported by the first @scheme[mod-path] in
       the @scheme[declare-exporting] declaration.}

]

The initial @scheme[mod-path]s sequence can be empty if
@scheme[mod-path]s are given with @scheme[#:use-sources]. In that
case, the rendered documentation never reports an exporting module for
identifiers that are documented within the section, but the
@scheme[mod-path]s in @scheme[#:use-sources] provide a binding context
for connecting (via hyperlinks) definitions and uses of identifiers.

The @scheme[declare-exporting] form should be used no more than once
per section, since the declaration applies to the entire section,
although overriding @scheme[declare-exporting] forms can appear in
sub-sections.}

@; ------------------------------------------------------------------------
@section[#:tag "doc-forms"]{Documenting Forms, Functions, Structure Types, and Values}

@defform/subs[(defproc prototype
                       result-contract-expr-datum
                       pre-flow ...)
              ([prototype (id arg-spec ...)
                          (prototype arg-spec ...)]
               [arg-spec (arg-id contract-expr-datum)
                         (arg-id contract-expr-datum default-expr)
                         (keyword arg-id contract-expr-datum)
                         (keyword arg-id contract-expr-datum default-expr)
                         ellipses
                         ellipses+]
               [ellipses @#,lit-ellipses]
               [ellipses+ @#,lit-ellipses+])]{

Produces a sequence of flow elements (encapsulated in a
@scheme[splice]) to document a procedure named @scheme[id]. Nesting
@scheme[prototype]s corresponds to a curried function, as in
@scheme[define]. The @scheme[id] is indexed, and it also registered so
that @scheme[scheme]-typeset uses of the identifier (with the same
for-label binding) are hyperlinked to this documentation.

A @scheme[defmodule] or @scheme[declare-exporting] form (or one of the
variants) in an enclosing section determines the @scheme[id] binding
that is being defined. The @scheme[id] should also have a for-label
binding (as introduced by @scheme[(require (for-label ....))]) that
matches the definition binding; otherwise, the defined @scheme[id]
will not typeset correctly within the definition.

Each @scheme[arg-spec] must have one of the following forms:

@specsubform[(arg-id contract-expr-datum)]{
       An argument whose contract is specified by
       @scheme[contract-expr-datum] which is typeset via
       @scheme[schemeblock0].}

@specsubform[(arg-id contract-expr-datum default-expr)]{
       Like the previous case, but with a default value. All arguments
       with a default value must be grouped together, but they can be
       in the middle of required arguments.}

@specsubform[(keyword arg-id contract-expr-datum)]{
       Like the first case, but for a keyword-based argument.}

@specsubform[(keyword arg-id contract-expr-datum default-expr)]{
       Like the previous case, but with a default
       value.}

@specsubform[@#,lit-ellipses]{Any number of the preceding argument.  This
       form is normally used at the end, but keyword-based arguments
       can sensibly appear afterward. See also the documentation for
       @scheme[append] for a use of @lit-ellipses before the last
       argument.}

@specsubform[@#,lit-ellipses+]{One or more of the preceding argument
       (normally at the end, like @lit-ellipses).}

The @scheme[result-contract-expr-datum] is typeset via
@scheme[schemeblock0], and it represents a contract on the procedure's
result.

The @tech{decode}d @scheme[pre-flow] documents the procedure. In this
description, references to @svar[arg-id]s using @scheme[scheme],
@scheme[schemeblock], @|etc| are typeset as procedure arguments.

The typesetting of all information before the @scheme[pre-flow]s
ignores the source layout, except that the local formatting is
preserved for contracts and default-values expressions.}


@defform[(defproc* ([prototype
                     result-contract-expr-datum] ...)
                   pre-flow ...)]{

Like @scheme[defproc], but for multiple cases with the same
@scheme[id]. 

When an @scheme[id] has multiple calling cases, they must be defined
with a single @scheme[defproc*], so that a single definition point
exists for the @scheme[id]. However, multiple distinct @scheme[id]s
can also be defined by a single @scheme[defproc*], for the case that
it's best to document a related group of procedures at once.}


@defform/subs[(defform maybe-id maybe-literals form-datum maybe-contracts
                pre-flow ...)
              ([maybe-id code:blank
                         (code:line #:id id)]
               [maybe-literals code:blank
                               (code:line #:literals (literal-id ...))]
               [maybe-contracts code:blank
                                (code:line #:contracts ([subform-datum contract-expr-datum]
                                                        ...))])]{

Produces a sequence of flow elements (encapsulated in a
@scheme[splice]) to document a syntatic form named by @scheme[id]
whose syntax is described by @scheme[form-datum]. If no @scheme[#:id] is used
to specify @scheme[id], then @scheme[form-datum] must have the form
@scheme[(id . _datum)].

The @scheme[id] is indexed, and it is also registered so that
@scheme[scheme]-typeset uses of the identifier (with the same
for-label binding) are hyperlinked to this documentation.

The @scheme[defmodule] or @scheme[declare-exporting] requirements, as
well as the binding requirements for @scheme[id], are the same as for
@scheme[defproc].

The @tech{decode}d @scheme[pre-flow] documents the form. In this
description, a reference to any identifier in @scheme[form-datum] via
@scheme[scheme], @scheme[schemeblock], @|etc| is typeset as a sub-form
non-terminal. If @scheme[#:literals] clause is provided, however,
instances of the @scheme[literal-id]s are typeset normally (i.e., as
determined by the enclosing context).

If a @scheme[#:contracts] clause is provided, each
@scheme[subform-datum] (typically an identifier that serves as a
meta-variable in @scheme[form-datum]) is shown as producing a value
that must satisfy the contract described by @scheme[contract-expr-datum].

The typesetting of @scheme[form-datum], @scheme[subform-datum], and
@scheme[contract-expr-datum] preserves the source layout, like
@scheme[schemeblock].}

@defform[(defform* maybe-id maybe-literals [form-datum ...+] maybe-contracts
           pre-flow ...)]{

Like @scheme[defform], but for multiple forms using the same
@scheme[_id].}

@defform[(defform/subs maybe-id maybe-literals form-datum
           ([nonterm-id clause-datum ...+] ...)
           maybe-contracts
           pre-flow ...)]{

Like @scheme[defform], but including an auxiliary grammar of
non-terminals shown with the @scheme[_id] form. Each
@scheme[nonterm-id] is specified as being any of the corresponding
@scheme[clause-datum]s, where the formatting of each
@scheme[clause-datum] is preserved.}


@defform[(defform*/subs maybe-id maybe-literals [form-datum ...]
           maybe-contracts
           pre-flow ...)]{

Like @scheme[defform/subs], but for multiple forms for @scheme[_id].}


@defform[(defform/none maybe-literal form-datum maybe-contracts
           pre-flow ...)]{

Like @scheme[defform], but without registering a definition.}


@defform[(defidform id pre-flow ...)]{

Like @scheme[defform], but with a plain @scheme[id] as the form.}


@defform[(specform maybe-literals datum maybe-contracts
           pre-flow ...)]{

Like @scheme[defform], but without indexing or registering a
definition, and with indenting on the left for both the specification
and the @scheme[pre-flow]s.}


@defform[(specsubform maybe-literals datum maybe-contracts
           pre-flow ...)]{

Similar to @scheme[defform], but without any specific identifier being
defined, and the table and flow are typeset indented. This form is
intended for use when refining the syntax of a non-terminal used in a
@scheme[defform] or other @scheme[specsubform]. For example, it is
used in the documentation for @scheme[defproc] in the itemization of
possible shapes for @svar[arg-spec].

The @scheme[pre-flow]s list is parsed as a flow that documents the
procedure. In this description, a reference to any identifier in
@scheme[datum] is typeset as a sub-form non-terminal.}


@defform[(specsubform/subs maybe-literals datum
           ([nonterm-id clause-datum ...+] ...)
           maybe-contracts
           pre-flow ...)]{

Like @scheme[specsubform], but with a grammar like
@scheme[defform/subs].}


@defform[(specspecsubform maybe-literals datum maybe-contracts
           pre-flow ...)]{

Like @scheme[specsubform], but indented an extra level. Since using
@scheme[specsubform] within the body of @scheme[specsubform] already
nests indentation, @scheme[specspecsubform] is for extra indentation
without nesting a description.}


@defform[(specspecsubform/subs maybe-literals datum
          ([nonterm-id clause-datum ...+] ...)
          maybe-contracts
          pre-flow ...)]{

Like @scheme[specspecsubform], but with a grammar like
@scheme[defform/subs].}


@defform[(defparam id arg-id contract-expr-datum pre-flow ...)]{

Like @scheme[defproc], but for a parameter. The
@scheme[contract-expr-datum] serves as both the result contract on the
parameter and the contract on values supplied for the parameter. The
@scheme[arg-id] refers to the parameter argument in the latter case.}

@defform[(defboolparam id arg-id pre-flow ...)]{

Like @scheme[defparam], but the contract on a parameter argument is
@scheme[any/c], and the contract on the parameter result is
@scheme[boolean?].}


@defform[(defthing id contract-expr-datum pre-flow ...)]{

Like @scheme[defproc], but for a non-procedure binding.}


@defform/subs[(defstruct struct-name ([field-name contract-expr-datum] ...)
                flag-keywords
                pre-flow ...)
              ([struct-name id
                            (id super-id)]
               [flag-keywords code:blank
                              #:mutable
                              (code:line #:inspector #f)
                              (code:line #:mutable #:inspector #f)])]{

Similar to @scheme[defform] or @scheme[defproc], but for a structure
definition.}


@defform[(deftogether [def-expr ...] pre-flow ...)]{

Combines the definitions created by the @scheme[def-expr]s into a
single definition box. Each @scheme[def-expr] should produce a
definition point via @scheme[defproc], @scheme[defform], etc. Each
@scheme[def-expr] should have an empty @scheme[pre-flow]; the
@tech{decode}d @scheme[pre-flow] sequence for the @scheme[deftogether]
form documents the collected bindings.}


@defform/subs[(schemegrammar maybe-literals id clause-datum ...+)
              ([maybe-literals code:blank
                               (code:line #:literals (literal-id ...))])]{
 
Creates a table to define the grammar of @scheme[id]. Each identifier
mentioned in a @scheme[clause-datum] is typeset as a non-terminal,
except for the identifiers listed as @scheme[literal-id]s, which are
typeset as with @scheme[scheme].}


@defform[(schemegrammar* maybe-literals [id clause-datum ...+] ...)]{

Like @scheme[schemegrammar], but for typesetting multiple productions
at once, aligned around the @litchar{=} and @litchar{|}.}

@defproc[(defidentifier [id identifier?]
                        [#:form? form? any/c #f]
                        [#:index? index? any/c #t]
                        [#:show-libs? show-libs? any/c #t])
         element?]{

Typesets @scheme[id] as a Scheme identifier, and also establishes the
identifier as the definition of a binding in the same way as
@scheme[defproc], @scheme[defform], etc. As always, the library that
provides the identifier must be declared via @scheme[defmodule] or
@scheme[declare-exporting] for an enclosing section.

If @scheme[form?] is a true value, then the identifier is documented
as a syntactic form, so that uses of the identifier (normally
including @scheme[id] itself) are typeset as a syntactic form.

If @scheme[index?] is a true value, then the identifier is registered
in the index.

If @scheme[show-libs?] is a true value, then the identifier's defining
module may be exposed in the typeset form (e.g., when viewing HTML and
the mouse hovers over the identifier).}

@; ------------------------------------------------------------------------
@section[#:tag "doc-classes"]{Documenting Classes and Interfaces}

@defform/subs[(defclass id super (intf-id ...) pre-flow ...)
              ([super super-id
                      (mixin-id super)])]{

Creates documentation for a class @scheme[id] that is a subclass of
@scheme[super] and implements each interface @scheme[intf-id]. Each
identifier in @scheme[super] (except @scheme[object%]) and
@scheme[intf-id] must be documented somewhere via @scheme[defclass] or
@scheme[definterface].

The decoding of the @scheme[pre-flow] sequence should start with
general documentation about the class, followed by constructor
definition (see @scheme[defconstructor]), and then field and method
definitions (see @scheme[defmethod]). In rendered form, the
constructor and method specification are indented to visually group
them under the class definition.}

@defform[(defclass/title id super (intf-id ...) pre-flow ...)]{

Like @scheme[defclass], also includes a @scheme[title] declaration
with the style @scheme['hidden]. In addition, the constructor and
methods are not left-indented.

This form is normally used to create a section to be rendered on its
own HTML. The @scheme['hidden] style is used because the definition
box serves as a title.}

@defform[(definterface id (intf-id ...) pre-flow ...)]{

Like @scheme[defclass], but for an interfaces. Naturally,
@scheme[pre-flow] should not generate a constructor declaration.}

@defform[(definterface/title id (intf-id ...) pre-flow ...)]{

Like @scheme[definterface], but for single-page rendering as in
@scheme[defclass/title].}

@defform[(defmixin id (domain-id ...) (range-id ...) pre-flow ...)]{

Like @scheme[defclass], but for a mixin. Any number of
@scheme[domain-id] classes and interfaces are specified for the
mixin's input requires, and any number of result classes and (more
likely) interfaces are specified for the @scheme[range-id]. The
@scheme[domain-id]s supply inherited methods.}

@defform[(defmixin/title id (domain-id ...) (range-id ...) pre-flow ...)]{

Like @scheme[defmixin], but for single-page rendering as in
@scheme[defclass/title].}

@defform/subs[(defconstructor (arg-spec ...) pre-flow ...)
              ([arg-spec (arg-id contract-expr-datum)
                         (arg-id contract-expr-datum default-expr)])]{

Like @scheme[defproc], but for a constructor declaration in the body
of @scheme[defclass], so no return contract is specified. Also, the
@scheme[new]-style keyword for each @scheme[arg-spec] is implicit from
the @scheme[arg-id].}

@defform[(defconstructor/make (arg-spec ...) pre-flow ...)]{

Like @scheme[defconstructor], but specifying by-position
initialization arguments (for use with @scheme[make-object]) instead
of by-name arguments (for use with @scheme[new]).}

@defform[(defconstructor*/make [(arg-spec ...) ...] pre-flow ...)]{

Like @scheme[defconstructor/make], but with multiple constructor
patterns analogous @scheme[defproc*].}

@defform[(defconstructor/auto-super [(arg-spec ...) ...] pre-flow ...)]{

Like @scheme[defconstructor], but the constructor is
annotated to indicate that additional initialization arguments are
accepted and propagated to the superclass.}

@defform/subs[#:literals (override override-final public-final 
                          augment augment-final pubment extend extend-final)
              (defmethod maybe-mode (id arg-spec ...)
                         result-contract-expr-datum
                         pre-flow ...)
              ([maybe-mode code:blank
                           (code:line #:mode override)
                           (code:line #:mode override-final)
                           (code:line #:mode public-final)
                           (code:line #:mode augment)
                           (code:line #:mode augment-final)
                           (code:line #:mode pubment)
                           (code:line #:mode extend)
                           (code:line #:mode extend-final)])]{

Like @scheme[defproc], but for a method within a @scheme[defclass] or
@scheme[definterface] body.

The @scheme[maybe-mode] specifies whether the method overrides a
method from a superclass, and so on. (For these purposes, use
@scheme[#:mode override] when refining a method of an implemented
interface.) The @scheme[extend] mode is like @scheme[override], but
the description of the method should describe only extensions to the
superclass implementation.}

@defform[(defmethod* maybe-mode
                     ([(id arg-spec ...)
                       result-contract-expr-datum] ...)
                     pre-flow ...)]{

Like @scheme[defproc*], but for a method within a @scheme[defclass] or
@scheme[definterface] body. The @scheme[maybe-mode] specification is as in
@scheme[defmethod].}


@defform[(method class/intf-id method-id)]{

Creates a hyperlink to the method named by @scheme[method-id] in the
class or interface named by @scheme[class/intf-id]. The hyperlink
names the method, only; see also @scheme[xmethod].

For-label binding information is used with @scheme[class/intf-id], but
not @scheme[method-id].}

@defform[(xmethod class/intf-id method-id)]{

Like @scheme[method], but the hyperlink shows both the method name and
the containing class/interface.}

@; ------------------------------------------------------------------------
@section[#:tag "doc-signatures"]{Documenting Signatures}

@defform[(defsignature id (super-id ...) pre-flow ...)]{

Defines a signature @scheme[id] that extends the @scheme[super-id]
signatures. Any elements defined in @tech{decode}d
@scheme[pre-flow]s---including forms, procedures, structure types,
classes, interfaces, and mixins---are defined as members of the
signature instead of direct bindings. These definitions can be
referenced through @scheme[sigelem] instead of @scheme[scheme].

The @tech{decode}d @scheme[pre-flow]s inset under the signature
declaration in the typeset output, so no new sections, @|etc| can be
started.}

@defform[(defsignature/splice id (super-id ...) pre-flow ...)]{

Like @scheme[defsignature], but the @tech{decode}d @scheme[pre-flow]s
are not typeset under the signature declaration, and new sections,
@|etc| can be started in the @scheme[pre-flow]s.}

@defproc[(signature-desc [pre-flow any/c] ...) any/c]{

Produces an opaque value that @scheme[defsignature] recognizes to
outdent in the typeset form. This is useful for text describing the
signature as a whole to appear right after the signature declaration.}

@defform[(sigelem sig-id id)]{

Typesets the identifier @scheme[id] with a hyperlink to its definition
as a member of the signature named by @scheme[sig-id].}

@; ------------------------------------------------------------------------
@section[#:tag "doc-strings"]{Various String Forms}

@defproc[(emph [pre-content any/c] ...) element?]{Typesets the
@tech{decode}d @scheme[pre-content] with emphasis (e.g., in italic).}

@defproc[(defterm [pre-content any/c] ...) element?]{Typesets the
@tech{decode}d @scheme[pre-content] as a defined term (e.g., in
italic). Consider using @scheme[deftech] instead, though, so that uses
of @scheme[tech] can hyper-link to the definition.}

@defproc[(onscreen [pre-content any/c] ...) element?]{ Typesets the
@tech{decode}d @scheme[pre-content] as a string that appears in a GUI,
such as the name of a button.}

@defproc[(menuitem [menu-name string?] [item-name string?]) element?]{
Typesets the given combination of a GUI's menu and item name.}

@defproc[(filepath [pre-content any/c] ...) element?]{Typesets the
@tech{decode}d @scheme[pre-content] as a file name (e.g., in
typewriter font and in in quotes).}

@defproc[(exec [pre-content any/c] ...) element?]{Typesets the
@tech{decode}d @scheme[pre-content] as a command line (e.g., in
typewriter font).}

@defproc[(envvar [pre-content any/c] ...) element?]{Typesets the given
@tech{decode}d @scheme[pre-content] as an environment variable (e.g.,
in typewriter font).}

@defproc[(Flag [pre-content any/c] ...) element?]{Typesets the given
@tech{decode}d @scheme[pre-content] as a flag (e.g., in typewriter
font with a leading @litchar{-}).}

@defproc[(DFlag [pre-content any/c] ...) element?]{Typesets the given
@tech{decode}d @scheme[pre-content] a long flag (e.g., in typewriter
font with two leading @litchar{-}s).}

@defproc[(PFlag [pre-content any/c] ...) element?]{Typesets the given
@tech{decode}d @scheme[pre-content] as a @litchar{+} flag (e.g., in typewriter
font with a leading @litchar{+}).}

@defproc[(DPFlag [pre-content any/c] ...) element?]{Typesets the given
@tech{decode}d @scheme[pre-content] a long @litchar{+} flag (e.g., in
typewriter font with two leading @litchar{+}s).}

@defproc[(math [pre-content any/c] ...) element?]{The @tech{decode}d
@scheme[pre-content] is further transformed:

 @itemize[

  @item{Any immediate @scheme['rsquo] is converted to @scheme['prime].}

  @item{Parentheses and sequences of decimal digits in immediate
        strings are left as-is, but any other immediate string is
        italicized.}
 ]

Extensions to @scheme[math] are likely, such as recognizing @litchar{_}
and @litchar{^} for subscripts and superscripts.}

@; ------------------------------------------------------------------------
@section[#:tag "section-links"]{Links}

@defproc[(secref [tag string?]
                 [#:doc module-path (or/c module-path? false/c) #f]
                 [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f]
                 [#:underline? underline? any/c #t])
         element?]{

Inserts the hyperlinked title of the section tagged @scheme[tag], but
@schemeidfont{aux-element} items in the title content are omitted in the
hyperlink label.

If @scheme[#:doc module-path] is provided, the @scheme[tag] refers to
a tag with a prefix determined by @scheme[module-path]. When
@exec{setup-plt} renders documentation, it automatically adds a tag
prefix to the document based on the source module. Thus, for example,
to refer to a section of the PLT Scheme reference,
@scheme[module-path] would be @scheme['(lib
"scribblings/reference/reference.scrbl")].

The @scheme[#:tag-prefixes prefixes] argument similarly supports
selecting a particular section as determined by a path of tag
prefixes. When a @scheme[#:doc] argument is provided, then
@scheme[prefixes] should trace a path of tag-prefixed subsections to
reach the @scheme[tag] section. When @scheme[#:doc] is not provided,
the @scheme[prefixes] path is relative to any enclosing section (i.e.,
the youngest ancestor that produces a match).

If @scheme[underline?] is @scheme[#f], then the hyperlink is rendered
in HTML without an underline.}


@defproc[(seclink [tag string?] 
                  [#:doc module-path (or/c module-path? false/c) #f]
                  [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f]
                  [#:underline? underline? any/c #t]
                  [pre-content any/c] ...) element?]{

Like @scheme[secref], but the link label is the @tech{decode}d
@scheme[pre-content] instead of the target section's name.}

@defproc[(other-manual [module-path module-path?]
                       [#:underline? underline? any/c #t])
         element?]{

Like @scheme[secref] for the document's implicit @scheme["top"]
tag. Use this function to refer to a whole manual instead of
@scheme[secref], in case a special style in the future is used for
manual titles.}


@defproc[(schemelink [id symbol?] [pre-content any/c] ...) element?]{

The @tech{decode}d @scheme[pre-content] is hyperlinked to the definition
of @scheme[id].}


@defproc[(link [url string?] [pre-content any/c] ...
               [#:underline? underline? any/c #t]
               [#:style style any/c (if underline? #f "plainlink")]) 
         element?]{

The @tech{decode}d @scheme[pre-content] is hyperlinked to
@scheme[url].  If @scheme[style] is not supplied, then
@scheme[underline?] determines how the link is rendered.}


@defproc[(elemtag [t tag?] [pre-content any/c] ...) element?]{

The tag @scheme[t] refers to the content form of
@scheme[pre-content].}


@defproc[(elemref [t tag?] [pre-content any/c] ... 
                  [#:underline? underline? any/c #t]) element?]{

The @tech{decode}d @scheme[pre-content] is hyperlinked to @scheme[t],
which is normally defined using @scheme[elemtag].}


@defproc[(deftech [pre-content any/c] ...
                  [#:style? style? any/c #t]) element?]{

Produces an element for the @tech{decode}d @scheme[pre-content], and
also defines a term that can be referenced elsewhere using
@scheme[tech].

The @scheme[content->string] result of the @tech{decode}d
@scheme[pre-content] is used as a key for references, but normalized
as follows:

@itemize[

 @item{A trailing ``ies'' is replaced by ``y''.}

 @item{A trailing ``s'' is removed.}

 @item{Consecutive hyphens and whitespaces are all replaced by a
       single space.}

]

These normalization steps help support natural-language references
that differ slightly from a defined form. For example, a definition of
``bananas'' can be referenced with a use of ``banana''.

If @scheme[style?] is true, then @scheme[defterm] is used on
@scheme[pre-content].}

@defproc[(tech [pre-content any/c] ...
               [#:doc module-path (or/c module-path? false/c) #f]
               [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f])
         element?]{

Produces an element for the @tech{decode}d @scheme[pre-content], and
hyperlinks it to the definition of the content as established by
@scheme[deftech]. The content's string form is normalized in the same
way as for @scheme[deftech]. The @scheme[#:doc] and
@scheme[#:tag-prefixes] arguments support cross-document and
section-specific references, like in @scheme[secref].

With the default style files, the hyperlink created by @scheme[tech]
is somewhat quieter than most hyperlinks: the underline in HTML output
is gray, instead of blue, and the term and underline turn blue only
when the mouse is moved over the term.

In some cases, combining both natural-language uses of a term and
proper linking can require some creativity, even with the
normalization performed on the term. For example, if ``bind'' is
defined, but a sentence uses the term ``binding,'' the latter can be
linked to the former using @schemefont["@tech{bind}ing"].}

@defproc[(techlink [pre-content any/c] ...
                   [#:doc module-path (or/c module-path? false/c) #f]
                   [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f]) 
         element?]{

Like @scheme[tech], but the link is not a quiet. For example, in HTML
output, a hyperlink underline appears even when the mouse is not over
the link.}

@; ------------------------------------------------------------------------
@section[#:tag "manual-indexing"]{Indexing}

@defform[(indexed-scheme datum ...)]{

A combination of @scheme[scheme] and @scheme[as-index], with the
following special cases when a single @scheme[datum] is provided:

 @itemize[

 @item{If @scheme[datum] is a @scheme[quote] form, then the quote is
       removed from the key (so that it's sorted using its unquoted
       form).}

 @item{If @scheme[datum] is a string, then quotes are removed from the
       key (so that it's sorted using the string content).}

]}

@defproc[(idefterm [pre-content any/c] ...) element?]{Combines
@scheme[as-index] and @scheme[defterm]. The content normally should be
plural, rather than singular. Consider using @scheme[deftech],
instead, which always indexes.}

@defproc[(pidefterm [pre-content any/c] ...) element?]{Like
@scheme[idefterm], but plural: adds an ``s'' on the end of the content
for the index entry. Consider using @scheme[deftech], instead.}

@defproc[(indexed-file [pre-content any/c] ...) element?]{A
combination of @scheme[file] and @scheme[as-index], but where the sort
key for the index iterm does not include quotes.}

@defproc[(indexed-envvar [pre-content any/c] ...) element?]{A
combination of @scheme[envvar] and @scheme[as-index].}

@; ------------------------------------------------------------------------
@section{Images}

@defproc[(image [filename-relative-to-source string?]
                [pre-element any/c] ...)
         flow-element?]{
 Creates a centered image from the given relative source path. The
 @tech{decode}d @scheme[pre-content] serves as the alternate text for
 contexts where the image cannot be displayed.

 The path is relative to the current directory, which is set by
 @exec{setup-plt} and @exec{scribble} to the directory of the main
 document file.

 When generating Latex output, if the filename has a @filepath{.gif}
 suffix, then the suffix is changed to @filepath{.png} (so a PNG file
 must exist in addition to the GIF file).}

@defproc[(image/plain [filename-relative-to-source string?]
                      [pre-element any/c] ...)
         element?]{
 Like @scheme[image], but the result is an element to appear inline in
 a paragraph.}

@; ------------------------------------------------------------------------
@section{Bibliography}

@margin-note{See also @schememodname[scriblib/autobib].}

@defproc[(cite [key string?]) element?]{

Links to a bibliography entry, using @scheme[key] both to indicate the
bibliography entry and, in square brackets, as the link text.}

@defproc[(bibliography [#:tag tag string? "doc-bibliography"]
                       [entry bib-entry?] ...)
         part?]{

Creates a bibliography part containing the given entries, each of
which is created with @scheme[bib-entry]. The entries are typeset in
order as given.}

@defproc[(bib-entry [#:key key string?]
                    [#:title title any/c]
                    [#:is-book? is-book? any/c #f]
                    [#:author author any/c #f]
                    [#:location location any/c #f]
                    [#:date date any/c #f] 
                    [#:url url any/c #f])
         bib-entry?]{

Creates a bibliography entry. The @scheme[key] is used to refer to the
entry via @scheme[cite]. The other arguments are used as elements in
the entry:

@itemize[

 @item{@scheme[title] is the title of the cited work. It will be
       surrounded by quotes in typeset form if @scheme[is-book?] is
       @scheme[#f], otherwise it is typeset via @scheme[italic].}

 @item{@scheme[author] lists the authors. Use names in their usual
       order (as opposed to ``last, first''), and separate multiple
       names with commas using ``and'' before the last name (where
       there are multiple names). The @scheme[author] is typeset in
       the bibliography as given, or it is omitted if given as
       @scheme[#f].}

 @item{@scheme[location] names the publication venue, such as a
       conference name or a journal with volume, number, and
       pages. The @scheme[location] is typeset in the bibliography as
       given, or it is omitted if given as @scheme[#f].}

 @item{@scheme[date] is a date, usually just a year (as a string). It
       is typeset in the bibliography as given, or it is omitted if
       given as @scheme[#f].}

 @item{@scheme[url] is an optional URL. It is typeset in the
       bibliography using @scheme[tt] and hyperlinked, or it is
       omitted if given as @scheme[#f].}

]}


@defproc[(bib-entry? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a bibliography entry created by
@scheme[bib-entry], @scheme[#f] otherwise.}


@; ------------------------------------------------------------------------
@section{Miscellaneous}

@defproc[(t [pre-content any/c] ...) paragraph?]{Wraps the
@tech{decode}d @scheme[pre-content] as a paragraph.}

@defthing[PLaneT string?]{@scheme["PLaneT"] (to help make sure you get
the letters in the right case).}

@defproc[(hash-lang) element?]{Returns an element for @hash-lang[]
that is hyperlinked to an explanation.}

@defthing[void-const element?]{Returns an element for @|void-const|.}

@defthing[undefined-const element?]{Returns an element for @|undefined-const|.}

@defproc[(centerline [pre-flow any/c] ...) table?]{Produces a
centered table with the @scheme[pre-flow] parsed by
@scheme[decode-flow].}

@defproc[(commandline [pre-content any/c] ...) paragraph?]{Produces
an inset command-line example (e.g., in typewriter font).}

@defproc[(margin-note [pre-content any/c] ...) blockquote?]{Produces
a @tech{blockquote} to be typeset in the margin instead of inlined.}

@; ------------------------------------------------------------------------
@section[#:tag "index-entries"]{Index-Entry Descriptions}

@defmodule[scribble/manual-struct]{The
@schememodname[scribble/manual-struct] library provides types used to
describe index entries created by @schememodname[scribble/manual]
functions. These structure types are provided separate from
@schememodname[scribble/manual] so that
@schememodname[scribble/manual] need not be loaded when deserializing
cross-reference information that was generated by a previously
rendered document.}

@defstruct[module-path-index-desc ()]{

Indicates that the index entry corresponds to a module definition via
@scheme[defmodule] and company.}

@defstruct[exported-index-desc ([name symbol?]
                               [from-libs (listof module-path?)])]{

Indicates that the index entry corresponds to the definition of an
exported binding. The @scheme[name] field and @scheme[from-libs] list
correspond to the documented name of the binding and the primary
modules that export the documented name (but this list is not
exhaustive, because new modules can re-export the binding).}

@defstruct[(form-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
syntactic form via @scheme[defform] and company.}

@defstruct[(procedure-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
procedure binding via @scheme[defproc] and company.}

@defstruct[(thing-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
binding via @scheme[defthing] and company.}

@defstruct[(struct-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
structure type via @scheme[defstruct] and company.}

@defstruct[(class-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
class via @scheme[defclass] and company.}

@defstruct[(interface-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of an
interface via @scheme[definterface] and company.}

@defstruct[(mixin-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
mixin via @scheme[defmixin] and company.}

@defstruct[(method-index-desc exported-index-desc) ([method-name symbol?]
                                                    [class-tag tag?])]{

Indicates that the index entry corresponds to the definition of an
method via @scheme[defmethod] and company. The @scheme[_name] field
from @scheme[exported-index-desc] names the class or interface that
contains the method. The @scheme[method-name] field names the method.
The @scheme[class-tag] field provides a pointer to the start of the
documentation for the method's class or interface.}
