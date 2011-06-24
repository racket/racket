#lang scribble/doc
@(require (except-in "mz.rkt" import export)
          (for-syntax racket/base)
          (for-label racket/require-transform
                     racket/require-syntax
                     racket/provide-transform
                     racket/provide-syntax))

@(define stx-eval (make-base-eval))
@(interaction-eval #:eval stx-eval (require (for-syntax racket/base)))

@(define (transform-time) @t{This procedure must be called during the
dynamic extent of a @tech{syntax transformer} application by the
expander, otherwise the @exnraise[exn:fail:contract].})


@title[#:tag "stxtrans"]{Syntax Transformers}

@defproc[(set!-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value created by
@racket[make-set!-transformer] or an instance of a structure type with
the @racket[prop:set!-transformer] property, @racket[#f] otherwise.}


@defproc[(make-set!-transformer [proc (syntax? . -> . syntax?)])
         set!-transformer?]{

Creates an @tech{assignment transformer} that cooperates with
@racket[set!]. If the result of @racket[make-set!-transformer] is
bound to @racket[_id] as a @tech{transformer binding}, then
@racket[proc] is applied as a transformer when @racket[_id] is
used in an expression position, or when it is used as the target of a
@racket[set!] assignment as @racket[(set! _id _expr)]. When the
identifier appears as a @racket[set!] target, the entire @racket[set!]
expression is provided to the transformer.

@examples[
#:eval stx-eval
(let ([x 1]
      [y 2])
  (let-syntax ([x (make-set!-transformer
                    (lambda (stx)
                      (syntax-case stx (set!)
                        (code:comment @#,t{Redirect mutation of x to y})
                        [(set! id v) (syntax (set! y v))]
                        (code:comment @#,t{Normal use of @racket[x] really gets @racket[x]})
                        [id (identifier? (syntax id)) (syntax x)])))])
    (begin
      (set! x 3)
      (list x y))))
]}


@defproc[(set!-transformer-procedure [transformer set!-transformer?])
         (syntax? . -> . syntax?)]{

Returns the procedure that was passed to
@racket[make-set!-transformer] to create @racket[transformer] or that
is identified by the @racket[prop:set!-transformer] property of
@racket[transformer].}


@defthing[prop:set!-transformer struct-type-property?]{

A @tech{structure type property} to identify structure types that act
as @tech{assignment transformers} like the ones created by
@racket[make-set!-transformer].

The property value must be an exact integer or procedure of one or two
arguments. In the former case, the integer designates a field within
the structure that should contain a procedure; the integer must be
between @racket[0] (inclusive) and the number of non-automatic fields
in the structure type (exclusive, not counting supertype fields), and
the designated field must also be specified as immutable.

If the property value is a procedure of one argument, then the
procedure serves as a @tech{syntax transformer} and for @racket[set!]
transformations. If the property value is a procedure of two
arguments, then the first argument is the structure whose type has
@racket[prop:set!-transformer] property, and the second argument is a
syntax object as for a @tech{syntax transformer} and for @racket[set!]
transformations; @racket[set!-transformer-procedure] applied to the
structure produces a new function that accepts just the syntax object
and calls the procedure associated through the property. Finally, if the
property value is an integer, the target identifier is extracted from
the structure instance; if the field value is not a procedure of one
argument, then a procedure that always calls
@racket[raise-syntax-error] is used, instead.

If a value has both the @racket[prop:set!-transformer] and
@racket[prop:rename-transformer] properties, then the latter takes
precedence. If a structure type has the @racket[prop:set!-transformer]
and @racket[prop:procedure] properties, then the former takes
precedence for the purposes of macro expansion.}


@defproc[(rename-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value created by
@racket[make-rename-transformer] or an instance of a structure type
with the @racket[prop:rename-transformer] property, @racket[#f]
otherwise.}


@defproc[(make-rename-transformer [id-stx syntax?]
                                  [delta-introduce (identifier? . -> . identifier?)
                                                   (lambda (id) id)])
         rename-transformer?]{

Creates a @tech{rename transformer} that, when used as a
@tech{transformer binding}, acts as a transformer that inserts the
identifier @racket[id-stx] in place of whatever identifier binds the
transformer, including in non-application positions, in @racket[set!]
expressions.

Such a transformer could be written manually, but the one created by
@racket[make-rename-transformer] triggers special cooperation with the
parser and other syntactic forms when @racket[_id] is bound to the
rename transformer:

@itemlist[

 @item{The parser to installs a @racket[free-identifier=?] and
       @racket[identifier-binding] equivalence between @racket[_id]
       and @racket[_id-stx], as long as @racket[id-stx] does not have
       a true value for the @indexed-racket['not-free-identifier=?]
       @tech{syntax property}.}

 @item{A @racket[provide] of @racket[_id] provides the binding
       indicated by @racket[id-stx] instead of @racket[_id], as long
       as @racket[id-stx] does not have a true value for the
       @indexed-racket['not-free-identifier=?] @tech{syntax property}
       and as long as @racket[id-stx] has a binding.}

 @item{If @racket[provide] exports @racket[_id], it uses a
       symbol-valued @indexed-racket['nominal-id] property of
       @racket[id-stx] to specify the ``nominal source identifier'' of
       the binding as reported by @racket[identifier-binding].}

 @item{If @racket[id-stx] has a true value for the
       @indexed-racket['not-provide-all-defined] @tech{syntax
       property}, then @racket[_id] (or its target) is not exported by
       @racket[all-defined-out].}

 @item{The @racket[syntax-local-value] and
       @racket[syntax-local-make-delta-introducer] functions recognize
       rename-transformer bindings and consult their targets.}

]}


@defproc[(rename-transformer-target [transformer rename-transformer?])
         identifier?]{

Returns the identifier passed to @racket[make-rename-transformer] to
create @racket[transformer] or as indicated by a
@racket[prop:rename-transformer] property on @racket[transformer].}


@defthing[prop:rename-transformer struct-type-property?]{

A @tech{structure type property} to identify structure types that act
as @tech{rename transformers} like the ones created by
@racket[make-rename-transformer].

The property value must be an exact integer or an identifier
@tech{syntax object}. In the former case, the integer designates a
field within the structure that should contain an identifier; the
integer must be between @racket[0] (inclusive) and the number of
non-automatic fields in the structure type (exclusive, not counting
supertype fields), and the designated field must also be specified as
immutable.

If the property value is an identifier, the identifier serves as the
target for renaming, just like the first argument to
@racket[make-rename-transformer]. If the property value is an integer,
the target identifier is extracted from the structure instance; if the
field value is not an identifier, then an identifier @racketidfont{?}
with an empty context is used, instead.}


@defproc[(local-expand [stx syntax?]
                       [context-v (or/c 'expression 'top-level 'module 'module-begin list?)]
                       [stop-ids (or/c (listof identifier?) #f)]
                       [intdef-ctx (or/c internal-definition-context? 
                                         (and/c pair? 
                                                (listof internal-definition-context?))
                                         #f)
                                   #f])
         syntax?]{

Expands @racket[stx] in the lexical context of the expression
currently being expanded. The @racket[context-v] argument is used as
the result of @racket[syntax-local-context] for immediate expansions;
for a particular @tech{internal-definition context}, generate a unique
value and @racket[cons] it onto the current result of
@racket[syntax-local-context] if it is a list.

When an identifier in @racket[stop-ids] is encountered by the expander
in a sub-expression, expansions stops for the sub-expression. If
@racket[stop-ids] is a non-empty list, then
@racket[begin], @racket[quote], @racket[set!], @racket[lambda],
@racket[case-lambda], @racket[let-values], @racket[letrec-values],
@racket[if], @racket[begin0], @racket[with-continuation-mark],
@racket[letrec-syntaxes+values], @racket[#%app],
@racket[#%expression], @racket[#%top], and
@racket[#%variable-reference] are added to @racket[stop-ids].  If
@racket[#%app], @racket[#%top], or @racket[#%datum] appears in
@racket[stop-ids], then application, top-level variable reference, and
literal data expressions without the respective explicit form are not
wrapped with the explicit form. If @racket[stop-ids] is @racket[#f]
instead of a list, then @racket[stx] is expanded only as long as the
outermost form of @racket[stx] is a macro (i.e., expansion does not
proceed to sub-expressions). A fully expanded form can include the
bindings listed in @secref["fully-expanded"] plus the
@racket[letrec-syntaxes+values] form.

The optional @racket[intdef-ctx] argument must be either @racket[#f],
the result of @racket[syntax-local-make-definition-context], or a list
of such results. In the latter two cases, lexical information for
internal definitions is added to @racket[stx] before it is expanded
(in reverse order relative to the list). The lexical information is
also added to the expansion result (because the expansion might
introduce bindings or references to internal-definition bindings).

Expansion of @racket[stx] can use certificates for the expression
already being expanded (see @secref["stxcerts"]), and @tech{inactive
certificates} associated with @racket[stx] are activated for
@racket[stx] (see @secref["stxcerts"]). Furthermore, if the
transformer is defined within a module (i.e., the current expansion
was triggered by a use of a module-defined identifier with a
@tech{transformer binding}) or if the current expression is being
expanded for the body of a module, then the expansion of @racket[stx]
can use any identifier defined by the module.

@transform-time[]

@examples[#:eval stx-eval
(define-syntax do-print
  (syntax-rules ()
    [(_ x ...) (printf x ...)]))

(define-syntax hello
  (syntax-rules ()
    [(_ x) (do-print "hello ~a" x)]))

(define-syntax (show stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([partly-expanded (local-expand #'(hello x)
						  'expression
						  (list #'do-print))]
		   [expanded (local-expand #'(hello x)
					   'expression
					   #f)])
       (printf "partly expanded syntax is ~a\n" (syntax->datum #'partly-expanded))
       (printf "expanded syntax is ~a\n" (syntax->datum #'expanded))
       #'expanded)]))

(show 1)
]}


@defproc[(syntax-local-expand-expression [stx syntax?])
         (values syntax? syntax?)]{

Like @racket[local-expand] given @racket['expression] and an empty
stop list, but with two results: a syntax object for the fully
expanded expression, and a syntax object whose content is opaque. The
latter can be used in place of the former (perhaps in a larger
expression produced by a macro transformer), and when the macro
expander encounters the opaque object, it substitutes the fully
expanded expression without re-expanding it; the
@exnraise[exn:fail:syntax] if the expansion context includes bindings
or marks that were not present for the original expansion, in which
case re-expansion might produce different results. Consistent use of
@racket[syntax-local-expand-expression] and the opaque object thus
avoids quadratic expansion times when local expansions are nested.

@transform-time[]}


@defproc[(local-transformer-expand [stx syntax?]
                       [context-v (or/c 'expression 'top-level 'module 'module-begin list?)]
                       [stop-ids (or/c (listof identifier?) #f)]
                       [intdef-ctx (or/c internal-definition-context? #f) #f])
         syntax?]{

Like @racket[local-expand], but @racket[stx] is expanded as a
transformer expression instead of a run-time expression, and any
lifted expressions---from calls to
@racket[syntax-local-lift-expression] during the expansion of
@racket[stx]---are captured into a @racket[let-values] form in the
result.}


@defproc[(local-expand/capture-lifts [stx syntax?]
                       [context-v (or/c 'expression 'top-level 'module 'module-begin list?)]
                       [stop-ids (or/c (listof identifier?) #f)]
                       [intdef-ctx (or/c internal-definition-context? #f) #f]
                       [lift-ctx any/c (gensym 'lifts)])
         syntax?]{

Like @racket[local-expand], but the result is a syntax object that
represents a @racket[begin] expression. Lifted expressions---from
calls to @racket[syntax-local-lift-expression] during the expansion of
@racket[stx]---appear with their identifiers in @racket[define-values]
forms, and the expansion of @racket[stx] is the last expression in the
@racket[begin]. The @racket[lift-ctx] value is reported by
@racket[syntax-local-lift-context] during local expansion. The lifted
expressions are not expanded, but instead left as provided in the
@racket[begin] form.}


@defproc[(local-transformer-expand/capture-lifts [stx syntax?]
                       [context-v (or/c 'expression 'top-level 'module 'module-begin list?)]
                       [stop-ids (or/c (listof identifier?) #f)]
                       [intdef-ctx (or/c internal-definition-context? #f) #f])
         syntax?]{

Like @racket[local-expand/capture-lifts], but @racket[stx] is expanded
as a transformer expression instead of a run-time expression. Lifted
expressions are reported as @racket[define-values] forms (in the
transformer environment).}


@defproc[(internal-definition-context? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{internal-definition
context}, @racket[#f] otherwise.}


@defproc[(syntax-local-make-definition-context
          [intdef-ctx (or/c internal-definition-context? #f) #f])
         internal-definition-context?]{

Creates an opaque @tech{internal-definition context} value to be used
with @racket[local-expand] and other functions. A transformer should
create one context for each set of internal definitions to be
expanded, and use it when expanding any form whose lexical context
should include the definitions. After discovering an internal
@racket[define-values] or @racket[define-syntaxes] form, use
@racket[syntax-local-bind-syntaxes] to add bindings to the context.
Finally, the transformer must call
@racket[internal-definition-context-seal] after all bindings have been
added; if an unsealed @tech{internal-definition context} is detected
in a fully expanded expression, the @exnraise[exn:fail:contract].

If @racket[intdef-ctx] is not @racket[#f], then the new
internal-definition context extends the given one. That is, expanding
in the new internal-definition context can use bindings previously
introduced into @racket[intdef-ctx].

@transform-time[]}


@defproc[(syntax-local-bind-syntaxes [id-list (listof identifier?)]
                                     [expr (or/c syntax? #f)]
                                     [intdef-ctx internal-definition-context?])
         void?]{

Binds each identifier in @racket[id-list] within the
@tech{internal-definition context} represented by @racket[intdef-ctx], where
@racket[intdef-ctx] is the result of
@racket[syntax-local-make-definition-context]. Supply @racket[#f] for
@racket[expr] when the identifiers correspond to
@racket[define-values] bindings, and supply a compile-time expression
when the identifiers correspond to @racket[define-syntaxes] bindings;
in the latter case, the number of values produced by the expression should
match the number of identifiers, otherwise the
@exnraise[exn:fail:contract:arity].

@transform-time[]}


@defproc[(internal-definition-context-seal [intdef-ctx internal-definition-context?])
         void?]{

Indicates that no further bindings will be added to
@racket[intdef-ctx], which must not be sealed already. See also
@racket[syntax-local-make-definition-context].}


@defproc[(identifier-remove-from-definition-context [id-stx identifier?]
                                                    [intdef-ctx (or/c internal-definition-context?
                                                                      (listof internal-definition-context?))])
         identifier?]{

Removes @racket[intdef-ctx] (or each identifier in the list) from the
@tech{lexical information} of @racket[id-stx]. This operation is
useful for correlating an identifier that is bound in an
internal-definition context with its binding before the
internal-definition context was created.

If simply removing the contexts produces a different binding than
completely ignoring the contexts (due to nested internal definition
contexts, for example), then the resulting identifier is given a
@tech{syntax mark} to simulate a non-existent lexical context. The
@racket[intdef-ctx] argument can be a list because removing
internal-definition contexts one at a time can produce a different
intermediate binding than removing them all at once.}


@defproc[(syntax-local-value [id-stx syntax?]
                             [failure-thunk (or/c (-> any) #f)
                                            #f]
                             [intdef-ctx (or/c internal-definition-context?
                                               #f)
                                         #f])
         any]{

Returns the @tech{transformer binding} value of @racket[id-stx] in
either the context associated with @racket[intdef-ctx] (if not
@racket[#f]) or the context of the expression being expanded (if
@racket[intdef-ctx] is @racket[#f]).  If @racket[intdef-ctx] is
provided, it must be an extension of the context of the expression
being expanded.

If @racket[id-stx] is bound to a @tech{rename transformer} created
with @racket[make-rename-transformer], @racket[syntax-local-value]
effectively calls itself with the target of the rename and returns
that result, instead of the @tech{rename transformer}.

If @racket[id-stx] has no @tech{transformer binding} (via
@racket[define-syntax], @racket[let-syntax], etc.) in that
environment, the result is obtained by applying @racket[failure-thunk]
if not @racket[#f]. If @racket[failure-thunk] is @racket[false], the
@exnraise[exn:fail:contract].

Resolving @racket[id-stx] can use certificates for the expression
being transformed (see @secref["stxcerts"]) as well as @tech{inactive
certificates} associated with @racket[id-stx] (see
@secref["stxcerts"]). Furthermore, if the transformer is defined
within a module (i.e., the current transformation was triggered by a
use of a module-defined identifier) or if the current expression is
being expanded for the body of a module, then resolving
@racket[id-stx] can access any identifier defined by the module.

@transform-time[]}


@defproc[(syntax-local-value/immediate [id-stx syntax?]
                                       [failure-thunk (or/c (-> any) #f)
                                                      #f]
                                       [intdef-ctx (or/c internal-definition-context?
                                                         #f)
                                                   #f])
         any]{

Like @racket[syntax-local-value], but the result is normally two
values. If @racket[id-stx] is bound to a @tech{rename transformer},
the results are the rename transformer and the identifier in the
transformer augmented with certificates from
@racket[id-stx]. @margin-note*{Beware that @racket[provide] on an
@racket[_id] bound to a @tech{rename transformer} may export the
target of the rename instead of @racket[_id]. See
@racket[make-rename-transformer] for more information.} If
@racket[id-stx] is not bound to a @tech{rename transformer}, then the
results are the value that @racket[syntax-local-value] would produce
and @racket[#f].

If @racket[id-stx] has no transformer binding, then
@racket[failure-thunk] is called (and it can return any number of
values), or an exception is raised if @racket[failure-thunk] is
@racket[#f].}


@defproc[(syntax-local-lift-expression [stx syntax?])
         identifier?]{

Returns a fresh identifier, and cooperates with the @racket[module],
@racket[letrec-syntaxes+values], @racket[define-syntaxes],
@racket[begin-for-syntax], and top-level expanders to bind the
generated identifier to the expression @racket[stx].

A run-time expression within a module is lifted to the module's top
level, just before the expression whose expansion requests the
lift. Similarly, a run-time expression outside of a module is lifted
to a top-level definition. A compile-time expression in a
@racket[letrec-syntaxes+values] or @racket[define-syntaxes] binding is
lifted to a @racket[let] wrapper around the corresponding right-hand
side of the binding. A compile-time expression within
@racket[begin-for-syntax] is lifted to a @racket[define-for-syntax]
declaration just before the requesting expression.

Other syntactic forms can capture lifts by using
@racket[local-expand/capture-lifts] or
@racket[local-transformer-expand/capture-lifts].

@transform-time[]}

@defproc[(syntax-local-lift-values-expression [n exact-nonnegative-integer?] [stx syntax?])
         (listof identifier?)]{

Like @racket[syntax-local-lift-expression], but binds the result to
@racket[n] identifiers, and returns a list of the @racket[n]
identifiers.

@transform-time[]}


@defproc[(syntax-local-lift-context)
         any/c]{

Returns a value that represents the target for expressions lifted via
@racket[syntax-local-lift-expression]. That is, for different
transformer calls for which this procedure returns the same value (as
determined by @racket[eq?]), lifted expressions for the two
transformer are moved to the same place. Thus, the result is useful
for caching lift information to avoid redundant lifts.

@transform-time[]}


@defproc[(syntax-local-lift-module-end-declaration [stx syntax?])
         void?]{

Cooperates with the @racket[module] form to insert @racket[stx] as
a top-level declaration at the end of the module currently being
expanded. If the current expression being
transformed is not in the module top-level, then @racket[stx] is
eventually expanded in an expression context.

@transform-time[] If the current expression being transformed is not
within a @racket[module] form, or if it is not a run-time expression,
then the @exnraise[exn:fail:contract].}


@defproc[(syntax-local-lift-require [raw-require-spec any/c] [stx syntax?])
         syntax?]{

Lifts a @racket[#%require] form corresponding to
@racket[raw-require-spec] (either as a @tech{syntax object} or datum)
to the top-level or to the top of the module currently being expanded,
wrapping it with @racket[for-meta] if the current expansion context is
not @tech{phase level} 0.

The resulting syntax object is the same as @racket[stx], except that a
fresh @tech{syntax mark} is added. The same @tech{syntax mark} is
added to the lifted @racket[#%require] form, so that the
@racket[#%require] form can bind uses of imported identifiers in the
resulting syntax object (assuming that the lexical information of
@racket[stx] includes the binding environment into which the
@racket[#%require] is lifted).

If @racket[raw-require-spec] and @racket[stx] are part of the input to
a transformer, then typically @racket[syntax-local-introduce] should be
applied to each before passing them to
@racket[syntax-local-lift-require], and then
@racket[syntax-local-introduce] should be applied to the result of
@racket[syntax-local-lift-require]. Otherwise, marks added
by the macro expander can prevent access to the new imports.

@transform-time[]}

@defproc[(syntax-local-lift-provide [raw-provide-spec-stx syntax?])
         void?]{

Lifts a @racket[#%provide] form corresponding to
@racket[raw-provide-spec-stx] to the top of the module currently being
expanded.

@transform-time[] If the current expression being transformed is not
within a @racket[module] form, or if it is not a run-time expression,
then the @exnraise[exn:fail:contract]. }

@defproc[(syntax-local-name) any/c]{

Returns an inferred name for the expression position being
transformed, or @racket[#f] if no such name is available. A name is
normally a symbol or an identifier. See also @secref["infernames"].

@transform-time[]}


@defproc[(syntax-local-context)
         (or/c 'expression 'top-level 'module 'module-begin list?)]{

Returns an indication of the context for expansion that triggered a
@tech{syntax transformer} call. See @secref["expand-context-model"]
for more information on contexts.

The symbol results indicate that the expression is being expanded for
an @tech{expression context}, a @tech{top-level context}, a
@tech{module context}, or a @tech{module-begin context}.

A list result indicates expansion in an @tech{internal-definition
context}. The identity of the list's first element (i.e., its
@racket[eq?]ness) reflects the identity of the internal-definition
context; in particular two transformer expansions receive the same
first value if and only if they are invoked for the same
@tech{internal-definition context}. Later values in the list similarly
identify @tech{internal-definition contexts} that are still being expanded,
and that required the expansion of nested internal-definition
contexts.

@transform-time[]}


@defproc[(syntax-local-phase-level) (or/c exact-integer? #f)]{

During the dynamic extent of a @tech{syntax transformer} application
by the expander, the result is the @tech{phase level} of the form
being expanded. Otherwise, the result is @racket[0].}


@defproc[(syntax-local-module-exports [mod-path module-path?]) 
         (values (listof symbol?) (listof symbol?) (listof symbol?))]{

Returns three lists of symbols that represent the @racket[provide]d
bindings of the module named by @racket[mod-path]. The first list
corresponds to the @tech{phase level} 0 exports of the module, the
second list corresponds to the @tech{phase level} -1 exports of the
module, and the last list corresponds to the @tech{label phase level}
exports of the module.

@transform-time[]}


@defproc[(syntax-local-get-shadower [id-stx identifier?]) identifier?]{

Returns @racket[id-stx] if no binding in the current expansion context
shadows @racket[id-stx] (ignoring unsealed @tech{internal-definition
contexts} and identifiers that had the @indexed-racket['unshadowable]
@tech{syntax property}), if @racket[id-stx] has no module bindings in
its lexical information, and if the current expansion context is not a
@tech{module context}.

If a binding of @racket[inner-identifier] shadows @racket[id-stx], the
result is the same as @racket[(syntax-local-get-shadower
inner-identifier)], except that it has the location and properties of
@racket[id-stx]. When searching for a shadowing binding, bindings from
unsealed @tech{internal-definition contexts} are ignored.

Otherwise, the result is the same as @racket[id-stx] with its module
bindings (if any) removed from its lexical information, and the
lexical information of the current @tech{module context} (if any)
added.

Thus, the result is an identifier corresponding to the innermost
shadowing of @racket[id-stx] in the current context if it is shadowed,
and a module-contextless version of @racket[id-stx] otherwise.

@transform-time[]}


@defproc[(syntax-local-certifier [active? boolean? #f])
         ((syntax?) (any/c (or/c procedure? #f)) 
          . ->* . syntax?)]{

Returns a procedure that captures any certificates currently available
for @racket[syntax-local-value] or @racket[local-expand]. The
procedure accepts one to three arguments: @racket[_stx] (required),
@racket[_key] (optional), and @racket[_intro] (optional). The
procedure's result is a syntax object like @racket[stx], except that
it includes the captured certificates as inactive (see
@secref["stxcerts"]) if @racket[active?] is @racket[#f] (the default)
or active otherwise. If @racket[key] is supplied and not @racket[#f],
it is associated with each captured certificate for later use through
@racket[syntax-recertify]. If @racket[_intro] is supplied, and if it
is not @racket[#f] (the default), then it must be a procedure created
by @racket[make-syntax-introducer], in which case the certificate
applies only to parts of @racket[stx] that are marked as introduced by
@racket[_intro].

Supply @racket[#t] for @racket[active?] when the syntax to be
certified can be safely used in any context by any party, and where
access to the syntax object should not confer any additional
access. Supply @racket[#f] for @racket[active?] when the syntax to be
certified is not accessible to parties that might abuse the access
that the certificate provides, and when the certified syntax
eventually appears (via macro expansion) within a larger expression
from which it cannot be safely extracted by other parties.

@transform-time[]}

@defproc[(syntax-transforming?) boolean?]{

Returns @racket[#t] during the dynamic extent of a @tech{syntax
transformer} application by the expander, @racket[#f] otherwise.}


@defproc[(syntax-local-introduce [stx syntax?]) syntax?]{

Produces a syntax object that is like @racket[stx], except that a
@tech{syntax mark} for the current expansion is added (possibly
canceling an existing mark in parts of @racket[stx]). See
@secref["transformer-model"] for information on @tech{syntax
marks}.

@transform-time[]}


@defproc[(make-syntax-introducer) (syntax? . -> . syntax?)]{

Produces a procedure that behaves like
@racket[syntax-local-introduce], but using a fresh @tech{syntax
mark}. Multiple applications of the same
@racket[make-syntax-introducer] result procedure use the same mark,
and different result procedures use distinct marks.}

@defproc[(make-syntax-delta-introducer [ext-stx syntax?] 
                                       [base-stx (or/c syntax? #f)]
                                       [phase-level (or/c #f exact-integer?)
                                                    (syntax-local-phase-level)])
         (syntax? . -> . syntax?)]{

Produces a procedure that behaves like
@racket[syntax-local-introduce], but using the @tech{syntax marks} of
@racket[ext-stx] that are not shared with @racket[base-stx].  If
@racket[ext-stx] does not extend the set of marks in @racket[base-stx]
or if @racket[base-stx] is @racket[#f], and if @racket[ext-stx] has a
module binding in the @tech{phase level} indicated by
@racket[phase-level], then any marks of @racket[ext-stx] that would be
needed to preserve its binding are not transferred in an introduction.

This procedure is potentially useful when @racket[_m-id] has a
transformer binding that records some @racket[_orig-id], and a use of
@racket[_m-id] introduces a binding of @racket[_orig-id]. In that
case, the @tech{syntax marks} in the use of @racket[_m-id] since the
binding of @racket[_m-id] should be transferred to the binding
instance of @racket[_orig-id], so that it captures uses with the same
lexical context as the use of @racket[_m-id].

More typically, however, @racket[syntax-local-make-delta-introducer]
should be used, since it cooperates with @tech{rename transformers}.}

@defproc[(syntax-local-make-delta-introducer [id identifier?])
         (identifier? . -> . identifier?)]{

Determines the binding of @racket[id]. If the binding is not a
@tech{rename transformer}, the result is an introducer as created by
@racket[make-syntax-delta-introducer] using @racket[id] and the
binding of @racket[id] in the environment of expansion. If the binding
is a @tech{rename transformer}, then the introducer is one composed
with the target of the @tech{rename transformer} and its
binding. Furthermore, the @racket[_delta-introduce] functions
associated with the @tech{rename transformers} (supplied as the second
argument to @racket[make-rename-transformer]) are composed (in
first-to-last order) before the introducers created with
@racket[make-syntax-delta-introducer] (which are composed
last-to-first).

The @exnraise[exn:fail:contract] if @racket[id] or any identifier in
its rename-transformer chain has no binding.

@transform-time[]}


@defproc[(syntax-local-transforming-module-provides?) boolean?]{

Returns @racket[#t] while a @tech{provide transformer} is running (see
@racket[make-provide-transformer]) or while an @racketidfont{expand} sub-form of
@racket[#%provide] is expanded, @racket[#f] otherwise.}


@defproc[(syntax-local-module-defined-identifiers) 
         (values (listof identifier?) (listof identifier?))]{

Can be called only while
@racket[syntax-local-transforming-module-provides?] returns
@racket[#t].

It returns two lists of identifiers corresponding to all definitions
within the module being expanded. This information is used for
implementing @racket[provide] sub-forms like @racket[all-defined-out].

The first result list corresponds to @tech{phase} 0 (i.e., normal)
definitions, and the second corresponds to @tech{phase} -1 (i.e.,
for-syntax) definitions.}


@defproc[(syntax-local-module-required-identifiers
          [mod-path (or/c module-path? #f)]
          [phase-level (or/c exact-integer? #f #t)])
         (listof (cons/c (or/c exact-integer? #f)
                         (listof identifier?)))]{

Can be called only while
@racket[syntax-local-transforming-module-provides?] returns
@racket[#t].

It returns an association list mapping phase levels to lists of
identifiers.  Each list of identifiers includes all bindings imported
(into the module being expanded) using the module path
@racket[mod-path], or all modules if @racket[mod-path] is
@racket[#f]. The association list includes all identifiers imported
with a @racket[phase-level] shift, of all shifts if
@racket[phase-level] is @racket[#t].

When an identifier is renamed on import, the result association list
includes the identifier by its internal name. Use
@racket[identifier-binding] to obtain more information about the
identifier.}

@; ----------------------------------------------------------------------

@section[#:tag "require-trans"]{@racket[require] Transformers}

@note-lib-only[racket/require-transform]

A @tech{transformer binding} whose value is a structure with the
@racket[prop:require-transformer] property implements a derived
@racket[_require-spec] for @racket[require].

The transformer is called with the syntax object representing its use
as a @racket[_require-spec] within a @racket[require] form, and the
result must be two lists: a list of @racket[import]s and a list of
@racket[import-source]s.

If the derived form contains a sub-form that is a
@racket[_require-spec], then it can call @racket[expand-import] to
transform the sub-@racket[_require-spec] to lists of imports and
import sources.

See also @racket[define-require-syntax], which supports macro-style
@racket[require] transformers.

@defproc[(expand-import [stx syntax?])
         (values (listof import?)
                 (listof import-source?))]{

Expands the given @racket[_require-spec] to lists of imports and
import sources.  The latter specifies modules to be
@tech{instantiate}d or @tech{visit}ed, so the modules that it
represents should be a superset of the modules represented in the
former list (so that a module will be @tech{instantiate}d or
@tech{visit}ed even if all of imports are eventually filtered from the
former list).}


@defproc[(make-require-transformer [proc (syntax? . -> . (values
                                                          (listof import?)
                                                          (listof import-source?)))])
         require-transformer?]{

Creates a @deftech{require transformer} (i.e., a structure with the
@racket[prop:require-transformer] property) using the given procedure
as the transformer.}


@defthing[prop:require-transformer struct-type-property?]{

A property to identify @racket[require] transformers. The property
value must be a procedure that takes a syntax object and returns
import and import-source lists.}


@defproc[(require-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the
@racket[prop:require-transformer] property, @racket[#f] otherwise.}


@defstruct[import ([local-id identifier?]
                   [src-sym symbol?]
                   [src-mod-path module-path?]
                   [mode (or/c exact-integer? #f)]
                   [req-mode (or/c exact-integer? #f)]
                   [orig-mode (or/c exact-integer? #f)]
                   [orig-stx syntax?])]{

A structure representing a single imported identifier:

@itemize[

 @item{@racket[local-id] --- the identifier to be bound within the
       importing module.}

 @item{@racket[src-sym] --- the external name of the binding as
       exported from its source module.}

 @item{@racket[src-mod-path] --- a @tech{module path} (relative to the
       importing module) for the source of the imported binding.}

 @item{@racket[orig-stx] --- a @tech{syntax object} for the source of
       the import, used for error reporting.}

 @item{@racket[mode] --- the @tech{phase level} of the binding in the
       importing module.}

 @item{@racket[req-mode] --- the @tech{phase level} shift of the
       import relative to the exporting module.}

 @item{@racket[orig-mode] --- the @tech{phase level} of the
       binding as exported by the exporting module.}

]}


@defstruct[import-source ([mod-path-stx (and/c syntax?
                                               (lambda (x)
                                                 (module-path? (syntax->datum x))))]
                          [mode (or/c exact-integer? #f)])]{

A structure representing an imported module, which must be
@tech{instantiate}d or @tech{visit}ed even if no binding is imported
into a module.

@itemize[

 @item{@racket[mod-path-stx] --- a @tech{module path} (relative
       to the importing module) for the source of the imported binding.}

 @item{@racket[mode] --- the @tech{phase level} shift of the import.}

]}


@defproc[(syntax-local-require-certifier)
         ((syntax?) (or/c #f (syntax? . -> . syntax?)) 
          . ->* . syntax?)]{

Like @racket[syntax-local-certifier], but to certify @tech{syntax
objects} that correspond to @racket[require] sub-forms, so that
@racket[expand-import] can deconstruct the @tech{syntax object} as
necessary to expand it.}


@; ----------------------------------------------------------------------

@section[#:tag "provide-trans"]{@racket[provide] Transformers}

@note-lib-only[racket/provide-transform]

A @tech{transformer binding} whose value is a structure with the
@racket[prop:provide-transformer] property implements a derived
@racket[_provide-spec] for @racket[provide].

The transformer is called with the syntax object representing its use
as a @racket[_provide-spec] within a @racket[provide] form and a list
of symbols representing the export modes specified by enclosing
@racket[_provide-spec]s. The result must be a list of
@racket[export]s.

If the derived form contains a sub-form that is a
@racket[_provide-spec], then it can call @racket[expand-export] to
transform the sub-@racket[_provide-spec] to a list of exports.

See also @racket[define-provide-syntax], which supports macro-style
@racket[provide] transformers.


@defproc[(expand-export [stx syntax?] [modes (listof (or/c exact-integer? #f))])
         (listof export?)]{

Expands the given @racket[_provide-spec] to a list of exports. The
@racket[modes] list controls the expansion of
sub-@racket[_provide-specs]; for example, an identifier refers to a
@tech{phase level} 0 binding unless the @racket[modes] list specifies
otherwise. Normally, @racket[modes] is either empty or contains a
single element.}


@defproc[(make-provide-transformer [proc (syntax? (listof (or/c exact-integer? #f))
                                          . -> . (listof export?))])
         provide-transformer?]{

Creates a @deftech{provide transformer} (i.e., a structure with the
@racket[prop:provide-transformer] property) using the given procedure
as the transformer.}


@defthing[prop:provide-transformer struct-type-property?]{

A property to identify @racket[provide] transformers. The property
value must be a procedure that takes a syntax object and mode list and
returns an export list.}


@defproc[(provide-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the
@racket[prop:provide-transformer] property, @racket[#f] otherwise.}


@defstruct[export ([local-id identifier?]
                   [out-sym symbol?]
                   [mode (or/c exact-integer? #f)]
                   [protect? any/c]
                   [orig-stx syntax?])]{

A structure representing a single imported identifier:

@itemize[

 @item{@racket[local-id] --- the identifier that is bound within the
       exporting module.}

 @item{@racket[out-sym] --- the external name of the binding.}

 @item{@racket[orig-stx] --- a @tech{syntax object} for the source of
       the export, used for error reporting.}

 @item{@racket[protect?] --- indicates whether the identifier should
       be protected (see @secref["modprotect"]).}

 @item{@racket[mode] --- the @tech{phase level} of the binding in the
       exporting module.}

]}


@defproc[(syntax-local-provide-certifier)
         ((syntax?) (or/c #f (syntax? . -> . syntax?)) 
          . ->* . syntax?)]{

Like @racket[syntax-local-certifier], but to certify @tech{syntax
objects} that correspond to @racket[provide] sub-forms, so that
@racket[expand-export] can deconstruct the @tech{syntax object} as
necessary to expand it.}

@; ----------------------------------------------------------------------

@close-eval[stx-eval]
