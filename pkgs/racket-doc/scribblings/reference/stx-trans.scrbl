#lang scribble/doc
@(require (except-in "mz.rkt" import export)
          (for-syntax racket/base)
          (for-label racket/require-transform
                     racket/require-syntax
                     racket/provide-transform
                     racket/provide-syntax
                     racket/keyword-transform
                     syntax/intdef))

@(define stx-eval (make-base-eval))
@examples[#:hidden #:eval stx-eval (require (for-syntax racket/base))]

@(define (transform-time) @t{This procedure must be called during the
dynamic extent of a @tech{syntax transformer} application by the
expander or while a module is @tech{visit}ed (see 
@racket[syntax-transforming?]), otherwise the
@exnraise[exn:fail:contract].})


@title[#:tag "stxtrans"]{Syntax Transformers}

@defproc[(set!-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value created by
@racket[make-set!-transformer] or an instance of a structure type with
the @racket[prop:set!-transformer] property, @racket[#f] otherwise.}


@defproc[(make-set!-transformer [proc (syntax? . -> . syntax?)])
         set!-transformer?]{

Creates an @tech{assignment transformer} that cooperates with
@racket[set!]. If the result of @racket[make-set!-transformer] is
bound to @racket[_id] as a @tech{transformer} binding, then
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
otherwise.

@examples[#:eval stx-eval
  (rename-transformer? (make-rename-transformer #'values))
  (rename-transformer? 'not-a-rename-transformer)
]}


@defproc[(make-rename-transformer [id-stx syntax?])
         rename-transformer?]{

Creates a @tech{rename transformer} that, when used as a
@tech{transformer} binding, acts as a transformer that inserts the
identifier @racket[id-stx] in place of whatever identifier binds the
transformer, including in non-application positions, in @racket[set!]
expressions.

Such a transformer could be written manually, but the one created by
@racket[make-rename-transformer] triggers special cooperation with the
parser and other syntactic forms when @racket[_id] is bound to the
rename transformer:

@itemlist[

 @item{The parser installs a @racket[free-identifier=?] and
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

 @item{The @racket[syntax-local-value] function recognizes
       rename-transformer bindings and consult their targets.}

]

@examples[#:eval stx-eval
  (define-syntax my-or (make-rename-transformer #'or))
  (my-or #f #t)
  (free-identifier=? #'my-or #'or)
]

@history[#:changed "6.3" @elem{Removed an optional second argument.}]}


@defproc[(rename-transformer-target [transformer rename-transformer?])
         identifier?]{

Returns the identifier passed to @racket[make-rename-transformer] to
create @racket[transformer] or as indicated by a
@racket[prop:rename-transformer] property on @racket[transformer].

@examples[#:eval stx-eval
  (rename-transformer-target (make-rename-transformer #'or))
]}


@defthing[prop:rename-transformer struct-type-property?]{

A @tech{structure type property} to identify structure types that act
as @tech{rename transformers} like the ones created by
@racket[make-rename-transformer].

The property value must be an exact integer, an identifier
@tech{syntax object}, or a procedure that takes one argument.
In the former case, the integer designates a
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
with an empty context is used, instead.

If the property value is a procedure that takes one argument, then the
procedure is called to obtain the identifier that the rename
transformer will use as a target identifier. The returned identifier
should probably have the @racket['not-free-identifier=?] syntax
property. If the procedure returns any value that is not an
identifier, the @racket[exn:fail:contract] exception is raised.

@examples[#:eval stx-eval #:escape UNSYNTAX
  (code:comment "Example of a procedure argument for prop:rename-transformer")
  (define-syntax slv-1 'first-transformer-binding)
  (define-syntax slv-2 'second-transformer-binding)
  (begin-for-syntax
    (struct slv-cooperator (redirect-to-first?)
      #:property prop:rename-transformer
      (λ (inst)
        (if (slv-cooperator-redirect-to-first? inst)
            #'slv-1
            #'slv-2))))
  (define-syntax (slv-lookup stx)
    (syntax-case stx ()
      [(_ id)
       #`(quote #,(syntax-local-value #'id))]))
  (define-syntax slv-inst-1 (slv-cooperator #t))
  (define-syntax slv-inst-2 (slv-cooperator #f))
  (slv-lookup slv-inst-1)
  (slv-lookup slv-inst-2)
]

@history[#:changed "6.3" "the property now accepts a procedure of one argument."]}


@defproc[(local-expand [stx any/c]
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
a list indicates an @tech{internal-definition context}, and more
information on the form of the list is below. If @racket[stx] is not
already a @tech{syntax object}, it is coerced with
@racket[(datum->syntax #f stx)] before expansion.

When an identifier in @racket[stop-ids] is encountered by the expander
in a sub-expression, expansions stops for the sub-expression. If
@racket[stop-ids] is a non-empty list and does not contain just @racket[module*], then
@racket[begin], @racket[quote], @racket[set!], @racket[lambda],
@racket[case-lambda], @racket[let-values], @racket[letrec-values],
@racket[if], @racket[begin0], @racket[with-continuation-mark],
@racket[letrec-syntaxes+values], @racket[#%app],
@racket[#%expression], @racket[#%top], and
@racket[#%variable-reference] are added to @racket[stop-ids].  If
@racket[#%app] or @racket[#%datum] appears in
@racket[stop-ids], then application and
literal data expressions without the respective explicit form are not
wrapped with the explicit form, and @racket[#%top] wrappers are
never added (even with an empty @racket[stop-ids] list). If @racket[stop-ids] is @racket[#f]
instead of a list, then @racket[stx] is expanded only as long as the
outermost form of @racket[stx] is a macro (i.e., expansion does not
proceed to sub-expressions). A fully expanded form can include the
bindings listed in @secref["fully-expanded"] plus the
@racket[letrec-syntaxes+values] form and @racket[#%expression]
in any expression position.

When @racket[#%plain-module-begin] is not itself in @racket[stop-ids]
and @racket[module*] is in @racket[stop-ids], then the
@racket[#%plain-module-begin] transformer refrains from expanding
@racket[module*] sub-forms. Otherwise, the
@racket[#%plain-module-begin] transformer detects and expands sub-forms
(such as @racket[define-values]) independent of the corresponding
identifier's presence in @racket[stop-ids].

When @racket[context-v] is @racket['module-begin], and the result of
expansion is a @racket[#%plain-module-begin] form, then a
@racket['submodule] @tech{syntax property} is added to each enclosed
@racket[module] form (but not @racket[module*] forms) in the same way as by
@racket[module] expansion.

The optional @racket[intdef-ctx] argument must be either @racket[#f],
the result of @racket[syntax-local-make-definition-context], or a list
of such results. In the latter two cases, lexical information for
internal definitions is added to @racket[stx] before it is expanded
(in reverse order relative to the list). The lexical information is
also added to the expansion result (because the expansion might
introduce bindings or references to internal-definition bindings).

For a particular @tech{internal-definition context}, generate a unique
value and put it into a list for @racket[context-v]. To allow
@tech{liberal expansion} of @racket[define] forms, the generated value
should be an instance of a structure with a true value for
@racket[prop:liberal-define-context]. If the internal-definition
context is meant to be self-contained, the list for @racket[context-v]
should contain only the generated value; if the internal-definition
context is meant to splice into an immediately enclosing context, then
when @racket[syntax-local-context] produces a list, @racket[cons] the
generated value onto that list.

When expressions are expanded via @racket[local-expand] with an
internal-definition context @racket[intdef-ctx], and when the expanded
expressions are incorporated into an overall form @racket[_new-stx],
then typically @racket[internal-definition-context-track] should be
applied to @racket[intdef-ctx] and @racket[_new-stx] to provide
expansion history to external tools.

@transform-time[]

@examples[#:eval stx-eval
(define-syntax-rule (do-print x ...)
  (printf x ...))

(define-syntax-rule (hello x)
  (do-print "hello ~a" x))

(define-syntax (show stx)
  (syntax-case stx ()
    [(_ x)
     (let ([partly (local-expand #'(hello x)
                                 'expression
                                 (list #'do-print))]
           [fully (local-expand #'(hello x)
                                'expression
                                #f)])
       (printf "partly expanded: ~s\n" (syntax->datum partly))
       (printf "fully expanded: ~s\n" (syntax->datum fully))
       fully)]))

(show 1)
]

@history[#:changed "6.0.1.3" @elem{Changed treatment of @racket[#%top]
                                   so that it is never introduced as
                                   an explicit wrapper.}]}


@defproc[(syntax-local-expand-expression [stx any/c])
         (values syntax? syntax?)]{

Like @racket[local-expand] given @racket['expression] and an empty
stop list, but with two results: a syntax object for the fully
expanded expression, and a syntax object whose content is opaque. The
latter can be used in place of the former (perhaps in a larger
expression produced by a macro transformer), and when the macro
expander encounters the opaque object, it substitutes the fully
expanded expression without re-expanding it; the
@exnraise[exn:fail:syntax] if the expansion context includes
@tech{scopes} that were not present for the original expansion, in which
case re-expansion might produce different results. Consistent use of
@racket[syntax-local-expand-expression] and the opaque object thus
avoids quadratic expansion times when local expansions are nested.

@transform-time[]}


@defproc[(local-transformer-expand [stx any/c]
                       [context-v (or/c 'expression 'top-level list?)]
                       [stop-ids (or/c (listof identifier?) #f)]
                       [intdef-ctx (or/c internal-definition-context? #f) #f])
         syntax?]{

Like @racket[local-expand], but @racket[stx] is expanded as a
transformer expression instead of a run-time expression.
For @racket['expression] expansion, any
lifted expressions---from calls to
@racket[syntax-local-lift-expression] during the expansion of
@racket[stx]---are captured into a @racket[let-values] form in the
result.}


@defproc[(local-expand/capture-lifts [stx any/c]
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
@racket[begin] form.

If @racket[context-v] is @racket['top-level] or @racket['module], then
@racket[module] forms can appear in the result as added via
@racket[syntax-local-lift-module]. If @racket[context-v] is
@racket['module], then @racket[module*] forms can appear, too.}


@defproc[(local-transformer-expand/capture-lifts [stx any/c]
                       [context-v (or/c 'expression 'top-level list?)]
                       [stop-ids (or/c (listof identifier?) #f)]
                       [intdef-ctx (or/c internal-definition-context? #f) #f]
                       [lift-ctx any/c (gensym 'lifts)])
         syntax?]{

Like @racket[local-expand/capture-lifts], but @racket[stx] is expanded
as a transformer expression instead of a run-time expression. Lifted
expressions are reported as @racket[define-values] forms (in the
transformer environment).}


@defproc[(internal-definition-context? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{internal-definition
context}, @racket[#f] otherwise.}


@defproc[(syntax-local-make-definition-context
          [intdef-ctx (or/c internal-definition-context? #f) #f]
          [add-scope? any/c #f])
         internal-definition-context?]{

Creates an opaque @tech{internal-definition context} value to be used
with @racket[local-expand] and other functions. A transformer should
create one context for each set of internal definitions to be
expanded, and use it when expanding any form whose lexical context
should include the definitions. After discovering an internal
@racket[define-values] or @racket[define-syntaxes] form, use
@racket[syntax-local-bind-syntaxes] to add bindings to the context.

An @tech{internal-definition context} internally creates a
@tech{scope} to represent the context. Unless @racket[add-scope?] is
@racket[#f], the @tech{scope} is added to any form that is expanded
within the context or that appears as the result of a (partial)
expansion within the context.

If @racket[intdef-ctx] is not @racket[#f], then the new
internal-definition context extends the given one. An extending
definition context adds all @tech{scopes} that are added by
@racket[intdef-ctx], and expanding in the new internal-definition context
can use bindings previously introduced into @racket[intdef-ctx].

@transform-time[]

@history[#:changed "6.3" @elem{Added the @racket[add-scope?] argument,
                               and made calling
                               @racket[internal-definition-context-seal]
                               no longer necessary.}]}


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


@defproc[(internal-definition-context-binding-identifiers
          [intdef-ctx internal-definition-context?])
         (listof identifier?)]{

Returns a list of all binding identifiers registered for
@racket[intdef-ctx] through @racket[syntax-local-bind-syntaxes]. Each
identifier in the returned list includes the @tech{internal-definition
context}'s @tech{scope}.

@history[#:added "6.3.0.4"]}


@defproc[(internal-definition-context-introduce [intdef-ctx internal-definition-context?]
                                                [stx syntax?]
                                                [mode (or/c 'flip 'add 'remove) 'flip])
         syntax?]{

Flips, adds, or removes (depending on @racket[mode]) the @tech{scope}
for @racket[intdef-ctx] for all parts of @racket[stx].

@history[#:added "6.3"]}



@defproc[(internal-definition-context-seal [intdef-ctx internal-definition-context?])
         void?]{

For backward compatibility only; has no effect.}


@defproc[(identifier-remove-from-definition-context [id-stx identifier?]
                                                    [intdef-ctx (or/c internal-definition-context?
                                                                      (listof internal-definition-context?))])
         identifier?]{

Removes all of the @tech{scopes} of @racket[intdef-ctx] (or of each
element in a list @racket[intdef-ctx]) from @racket[id-stx].

The @racket[identifier-remove-from-definition-context] function is
provided for backward compatibility; the more general
@racket[internal-definition-context-introduce] function is preferred.

@history[#:changed "6.3" @elem{Simplified the operation to @tech{scope} removal.}]}




@defthing[prop:expansion-contexts struct-type-property?]{

A @tech{structure type property} to constrain the use of macro
@tech{transformers} and @tech{rename transformers}. The property's
value must be a list of symbols, where the allowed symbols are
@racket['expression], @racket['top-level], @racket['module],
@racket['module-begin], and @racket['definition-context]. Each symbol
corresponds to an expansion context in the same way as for
@racket[local-expand] or as reported by @racket[syntax-local-context],
except that @racket['definition-context] is used (instead of a list)
to represent an @tech{internal-definition context}.

If an identifier is bound to a transformer whose list does not include
a symbol for a particular use of the identifier, then the use is
adjusted as follows:
@;
@itemlist[

 @item{In a @racket['module-begin] context, then the use is wrapped in
       a @racket[begin] form.}

 @item{In a @racket['module], @racket['top-level],
       @racket['internal-definition] or context, if
       @racket['expression] is present in the list, then the use is
       wrapped in an @racket[#%expression] form.}

 @item{Otherwise, a syntax error is reported.}

]

The @racket[prop:expansion-contexts] property is most useful in
combination with @racket[prop:rename-transformer], since a general
@tech{transformer} procedure can use @racket[syntax-local-context].
Furthermore, a @racket[prop:expansion-contexts] property makes the
most sense when a @tech{rename transformer}'s identifier has the
@racket['not-free-identifier=?] property, otherwise a definition of
the binding creates a binding alias that effectively routes around the
@racket[prop:expansion-contexts] property.

@history[#:added "6.3"]}


@defproc[(syntax-local-value [id-stx syntax?]
                             [failure-thunk (or/c (-> any) #f)
                                            #f]
                             [intdef-ctx (or/c internal-definition-context?
                                               #f)
                                         #f])
         any]{

Returns the @tech{transformer} binding value of @racket[id-stx] in
either the context associated with @racket[intdef-ctx] (if not
@racket[#f]) or the context of the expression being expanded (if
@racket[intdef-ctx] is @racket[#f]).  If @racket[intdef-ctx] is
provided, it must be an extension of the context of the expression
being expanded.

If @racket[id-stx] is bound to a @tech{rename transformer} created
with @racket[make-rename-transformer], @racket[syntax-local-value]
effectively calls itself with the target of the rename and returns
that result, instead of the @tech{rename transformer}.

If @racket[id-stx] has no @tech{transformer} binding (via
@racket[define-syntax], @racket[let-syntax], etc.) in that
environment, the result is obtained by applying @racket[failure-thunk]
if not @racket[#f]. If @racket[failure-thunk] is @racket[false], the
@exnraise[exn:fail:contract].

@transform-time[]

@examples[#:eval stx-eval
  (define-syntax swiss-cheeses? #t)
  (define-syntax (transformer stx)
    (if (syntax-local-value #'swiss-cheeses?)
        #''(gruyère emmental raclette)
        #''(roquefort camembert boursin)))
  (transformer)
]
@examples[#:eval stx-eval
  (define-syntax (transformer-2 stx)
    (syntax-local-value #'something-else (λ () (error "no binding"))))
  (eval:error (transformer-2))
]
@examples[#:eval stx-eval
  (define-syntax nachos #'(printf "nachos~n"))
  (define-syntax chips (make-rename-transformer #'nachos))
  (define-syntax (transformer-3 stx)
    (syntax-local-value #'chips))
  (transformer-3)
]}


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
transformer. @margin-note*{Beware that @racket[provide] on an
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
@racket[begin-for-syntax] is lifted to a @racket[define]
declaration just before the requesting expression within the 
@racket[begin-for-syntax].

Other syntactic forms can capture lifts by using
@racket[local-expand/capture-lifts] or
@racket[local-transformer-expand/capture-lifts].

@transform-time[] In addition, this procedure can be called only when
a lift target is available, as indicated by
@racket[syntax-transforming-with-lifts?].}

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


@defproc[(syntax-local-lift-module [stx syntax?])
         void?]{

Cooperates with the @racket[module] form or top-level expansion to add
@racket[stx] as a module declaration in the enclosing module or top-level.
The @racket[stx] form must start with @racket[module] or @racket[module*],
where the latter is only allowed within the expansion of a module.

The module is not immediately declared when
@racket[syntax-local-lift-module] returns. Instead, the module
declaration is recorded for processing when expansion returns to the
enclosing module body or top-level sequence.

@transform-time[] If the current expression being transformed is not
within a @racket[module] form or within a top-level expansion, then
the @exnraise[exn:fail:contract]. If @racket[stx] form does not start with
@racket[module] or @racket[module*], or if it starts with @racket[module*]
in a top-level context, the @exnraise[exn:fail:contract].

@history[#:added "6.3"]}


@defproc[(syntax-local-lift-module-end-declaration [stx syntax?])
         void?]{

Cooperates with the @racket[module] form to insert @racket[stx] as
a top-level declaration at the end of the module currently being
expanded. If the current expression being
transformed is in @tech{phase level} 0 and not in the module top-level, then @racket[stx] is
eventually expanded in an expression context. If the current expression being
transformed is in a higher @tech{phase level} (i.e., nested within some
number of @racket[begin-for-syntax]es within a module top-level), then the lifted declaration
is placed at the very end of the module (under a suitable number of
@racket[begin-for-syntax]es), instead of merely the end of the
enclosing @racket[begin-for-syntax].

@transform-time[] If the current expression being transformed is not
within a @racket[module] form (see @racket[syntax-transforming-module-expression?]), 
then the @exnraise[exn:fail:contract].}


@defproc[(syntax-local-lift-require [raw-require-spec any/c] [stx syntax?])
         syntax?]{

Lifts a @racket[#%require] form corresponding to
@racket[raw-require-spec] (either as a @tech{syntax object} or datum)
to the top-level or to the top of the module currently being expanded
 or to an enclosing @racket[begin-for-syntax].

The resulting syntax object is the same as @racket[stx], except that a
fresh @tech{scope} is added. The same @tech{scope} is
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
expanded or to an enclosing @racket[begin-for-syntax].

@transform-time[] If the current expression being transformed is not
within a @racket[module] form (see @racket[syntax-transforming-module-expression?]),
then the @exnraise[exn:fail:contract].}

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


@defproc[(syntax-local-phase-level) exact-integer?]{

During the dynamic extent of a @tech{syntax transformer} application
by the expander, the result is the @tech{phase level} of the form
being expanded. Otherwise, the result is @racket[0].

@examples[#:eval stx-eval
  (code:comment "a macro bound at phase 0")
  (define-syntax (print-phase-level stx)
    (printf "phase level: ~a~n" (syntax-local-phase-level))
    #'(void))
  (require (for-meta 2 racket/base))
  (begin-for-syntax
    (code:comment "a macro bound at phase 1")
    (define-syntax (print-phase-level stx)
      (printf "phase level: ~a~n" (syntax-local-phase-level))
      #'(void)))
  (print-phase-level)
  (begin-for-syntax (print-phase-level))
]
}


@defproc[(syntax-local-module-exports [mod-path (or/c module-path?
                                                      (and/c syntax?
                                                             (lambda (stx)
                                                               (module-path? (syntax->datum stx)))))])
         (listof (cons/c (or/c exact-integer? #f) (listof symbol?)))]{

Returns an association list from @tech{phase-level} numbers (or
@racket[#f] for the @tech{label phase level}) to lists of symbols,
where the symbols are the names of @racket[provide]d
bindings from @racket[mod-path] at the corresponding @tech{phase level}.

@transform-time[]}


@defproc[(syntax-local-submodules) (listof symbol?)]{

Returns a list of submodule names that are declared via
@racket[module] (as opposed to @racket[module*]) in the current
expansion context.

@transform-time[]}


@defproc[(syntax-local-get-shadower [id-stx identifier?]
                                    [only-generated? any/c #f])
         identifier?]{

Adds @tech{scopes} to @racket[id-stx] so that it refers to bindings
in the current expansion context or could bind any identifier obtained
via @racket[(syntax-local-get-shadower id-stx)] in more nested contexts.
If @racket[only-generated?] is true, the phase-spanning @tech{scope}
of the enclosing module or namespace is omitted from the added scopes,
however, which limits the bindings that can be referenced (and
therefore avoids certain ambiguous references).

This function is intended for the implementation of
@racket[syntax-parameterize] and @racket[local-require].

@transform-time[]

@history[#:changed "6.3" @elem{Simplified to the minimal functionality
                               needed for @racket[syntax-parameterize]
                               and @racket[local-require].}]}


@defproc[(syntax-local-make-delta-introducer [id-stx identifier?]) procedure?]{

For (limited) backward compatibility only; raises @racket[exn:fail:supported].

@history[#:changed "6.3" @elem{changed to raise @racket[exn:fail:supported].}]}



@defproc[(syntax-local-certifier [active? boolean? #f])
         ((syntax?) (any/c (or/c procedure? #f)) 
          . ->* . syntax?)]{

For backward compatibility only; returns a procedure that returns its
first argument.}

@defproc[(syntax-transforming?) boolean?]{

Returns @racket[#t] during the dynamic extent of a @tech{syntax
transformer} application by the expander and while a module is being
@tech{visit}ed, @racket[#f] otherwise.}


@defproc[(syntax-transforming-with-lifts?) boolean?]{

Returns @racket[#t] if @racket[(syntax-transforming?)] produces
@racket[#t] and a target context is available for lifting expressions
(via @racket[syntax-local-lift-expression]), @racket[#f] otherwise.

For example, during an immedate macro expansion triggered by
@racket[local-expand], as opposed to
@racket[local-expand/capture-lifts], @racket[(syntax-transforming?)]
produces @racket[#t] while @racket[(syntax-transforming-with-lifts?)]
produces @racket[#f].

@history[#:added "6.3.0.9"]}


@defproc[(syntax-transforming-module-expression?) boolean?]{

Returns @racket[#t] during the dynamic extent of a @tech{syntax
transformer} application by the expander for an expression
within a @racket[module] form, @racket[#f] otherwise.}


@defproc[(syntax-local-identifier-as-binding [id-stx identifier?]) identifier?]{

Returns an identifier like @racket[id-stx], but without @tech{use-site
scopes} that were previously added to the identifier as part of a
macro expansion in the current definition context.

In a @tech{syntax transformer} that runs in a non-expression context
and forces the expansion of subforms with @racket[local-expand], use
@racket[syntax-local-identifier-as-binding] on an identifier from the
expansion before moving it into a binding position or comparing with
with @racket[bound-identifier=?]. Otherwise, the results can be
inconsistent with the way that @racket[define] works in the same
definition context.

@transform-time[]

@history[#:added "6.3"]}

@defproc[(syntax-local-introduce [stx syntax?]) syntax?]{

Produces a syntax object that is like @racket[stx], except that the
presence of @tech{scopes} for the current expansion---both the
macro-introduction scope and the use-site scope, if any---is flipped
on all parts of the syntax object. See @secref["transformer-model"] for information
on macro-introduction and use-site @tech{scopes}.

@transform-time[]}


@defproc[(make-syntax-introducer [as-use-site? any/c #f])
         ((syntax?) ((or/c 'flip 'add 'remove)) . ->* . syntax?)]{

Produces a procedure that encapsulates a fresh @tech{scope} and flips,
adds, or removes it in a given syntax object. By default, the fresh
scope is a macro-introduction scope, but providing a true value for
@racket[as-use-site?] creates a scope that is like a use-site scope;
the difference is in how the scopes are treated by
@racket[syntax-original?].

The action of the generated procedure can be @racket['flip] (the
default) to flip the presence of a scope in each part of a given
syntax object, @racket['add] to add the scope to each regardless of
whether it is present already, or @racket['remove] to remove the scope
when it is currently present in any part.

Multiple applications of the same
@racket[make-syntax-introducer] result procedure use the same scope,
and different result procedures use distinct scopes.

@history[#:changed "6.3" @elem{Added the optional
                               @racket[as-use-site?] argument, and
                               added the optional operation argument
                               in the result procedure.}]}

@defproc[(make-syntax-delta-introducer [ext-stx identifier?] 
                                       [base-stx (or/c syntax? #f)]
                                       [phase-level (or/c #f exact-integer?)
                                                    (syntax-local-phase-level)])
         ((syntax?) ((or/c 'flip 'add 'remove)) . ->* . syntax?)]{

Produces a procedure that behaves like the result of
@racket[make-syntax-introducer], but using the @tech{scopes} of
@racket[ext-stx] that are not shared with @racket[base-stx].
A @racket[#f] value for @racket[base-stx] is equivalent to a syntax
object with no @tech{scopes}.

This procedure is potentially useful when some @racket[_m-id] has a
transformer binding that records some @racket[_orig-id], and a use of
@racket[_m-id] introduces a binding of @racket[_orig-id]. In that
case, the @tech{scopes} one the use of @racket[_m-id] added since the
binding of @racket[_m-id] should be transferred to the binding
instance of @racket[_orig-id], so that it captures uses with the same
lexical context as the use of @racket[_m-id].

If @racket[ext-stx] is @tech{tainted} or @tech{armed}, then an
identifier result from the created procedure is @tech{tainted}.}


@defproc[(syntax-local-transforming-module-provides?) boolean?]{

Returns @racket[#t] while a @tech{provide transformer} is running (see
@racket[make-provide-transformer]) or while an @racketidfont{expand} sub-form of
@racket[#%provide] is expanded, @racket[#f] otherwise.}


@defproc[(syntax-local-module-defined-identifiers) (and/c hash? immutable?)]{

Can be called only while
@racket[syntax-local-transforming-module-provides?] returns
@racket[#t].

It returns a hash table mapping a @tech{phase-level} number (such as
@racket[0]) to a list of all definitions at that @tech{phase level}
within the module being expanded. This information is used for
implementing @racket[provide] sub-forms like @racket[all-defined-out].

Beware that the @tech{phase-level} keys are absolute relative to the
enclosing module, and not relative to the current transformer phase
level as reported by @racket[syntax-local-phase-level].}


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
with a @racket[phase-level] shift, or all shifts if
@racket[phase-level] is @racket[#t].

When an identifier is renamed on import, the result association list
includes the identifier by its internal name. Use
@racket[identifier-binding] to obtain more information about the
identifier.

Beware that the @tech{phase-level} keys are absolute relative to the
enclosing module, and not relative to the current transformer phase
level as reported by @racket[syntax-local-phase-level].}

@deftogether[(
@defthing[prop:liberal-define-context struct-type-property?]
@defproc[(liberal-define-context? [v any/c]) boolean?]
)]{

An instance of a structure type with a true value for the
@racket[prop:liberal-define-context] property can be used as an
element of an @tech{internal-definition context} representation in the
result of @racket[syntax-local-context] or the second argument of
@racket[local-expand]. Such a value indicates that the context
supports @deftech{liberal expansion} of @racket[define] forms into
potentially multiple @racket[define-values] and
@racket[define-syntaxes] forms. The @racket['module] and
@racket['module-body] contexts implicitly allow @tech{liberal
expansion}.

The @racket[liberal-define-context?] predicate returns @racket[#t] if
@racket[v] is an instance of a structure with a true value for the
@racket[prop:liberal-define-context] property, @racket[#f] otherwise.}

@; ----------------------------------------------------------------------

@section[#:tag "require-trans"]{@racket[require] Transformers}

@note-lib-only[racket/require-transform]

A @tech{transformer} binding whose value is a structure with the
@racket[prop:require-transformer] property implements a derived
@racket[_require-spec] for @racket[require] as a @deftech{require
transformer}.

A @tech{require transformer} is called with the syntax object representing its use
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

Creates a @tech{require transformer} using the given procedure as the
transformer.
Often used in combination with @racket[expand-import].

@examples[
#:eval stx-eval
(require (for-syntax racket/require-transform))

(define-syntax printing
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ path)
        (printf "Importing: ~a~n" #'path)
        (expand-import #'path)]))))

(require (printing racket/match))
]}


@defthing[prop:require-transformer struct-type-property?]{

A property to identify @tech{require transformers}. The property
value must be a procedure that takes the structure and returns a transformer
procedure; the returned transformer procedure takes a syntax object and returns
import and import-source lists.}


@defproc[(require-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the
@racket[prop:require-transformer] property, @racket[#f] otherwise.}


@defstruct[import ([local-id identifier?]
                   [src-sym symbol?]
                   [src-mod-path (or/c module-path? 
                                       (and/c syntax?
                                              (lambda (stx) 
                                                (module-path? (syntax->datum stx)))))]
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


@defparam[current-require-module-path module-path (or/c #f module-path-index?)]{

A @tech{parameter} that determines how relative @racket[require]-level module
paths are expanded to @racket[#%require]-level module paths by
@racket[convert-relative-module-path] (which is used implicitly by all
built-in @racket[require] sub-forms).

When the value of @racket[current-require-module-path] is @racket[#f],
relative module paths are left as-is, which means that the
@racket[require] context determines the resolution of the module
path.

The @racket[require] form @racket[parameterize]s
@racket[current-require-module-path] as @racket[#f] while invoking
sub-form transformers, while @racket[relative-in] @racket[parameterize]s
to a given module path.}


@defproc[(convert-relative-module-path [module-path
                                        (or/c module-path?
                                              (and/c syntax?
                                                     (lambda (stx)
                                                       (module-path? (syntax-e stx)))))])
          (or/c module-path?
                (and/c syntax?
                       (lambda (stx)
                         (module-path? (syntax-e stx)))))]{

Converts @racket[module-path] according to @racket[current-require-module-path].

If @racket[module-path] is not relative or if the value of
@racket[current-require-module-path] is @racket[#f], then
@racket[module-path] is returned. Otherwise, @racket[module-path] is
converted to an absolute module path that is equivalent to
@racket[module-path] relative to the value of
@racket[current-require-module-path].}


@defproc[(syntax-local-require-certifier)
         ((syntax?) (or/c #f (syntax? . -> . syntax?)) 
          . ->* . syntax?)]{

For backward compatibility only; returns a procedure that returns its
first argument.}

@; ----------------------------------------------------------------------

@section[#:tag "provide-trans"]{@racket[provide] Transformers}

@note-lib-only[racket/provide-transform]

A @tech{transformer} binding whose value is a structure with the
@racket[prop:provide-transformer] property implements a derived
@racket[_provide-spec] for @racket[provide] as a @deftech{provide transformer}.
A @tech{provide transformer} is applied as part of the last phase of
a module's expansion, after all other declarations and expressions within
the module are expanded.

A @tech{transformer} binding whose value is a structure with the
@racket[prop:provide-pre-transformer] property implements a derived
@racket[_provide-spec] for @racket[provide] as a @deftech{provide
pre-transformer}.  A @tech{provide pre-transformer} is applied as part
of the first phase of a module's expansion. Since it is used in the
first phase, a @tech{provide pre-transformer} can use functions such
as @racket[syntax-local-lift-expression] to introduce expressions and
definitions in the enclosing module.

An identifier can have a @tech{transformer} binding to a value that
acts both as a @tech{provide transformer} and @tech{provide
pre-transformer}. The result of a @tech{provide
pre-transformer} is @emph{not} automatically re-expanded, so a
@tech{provide pre-transformer} can usefully expand to itself in that case.

A transformer is called with the syntax object representing its use as
a @racket[_provide-spec] within a @racket[provide] form and a list of
symbols representing the export modes specified by enclosing
@racket[_provide-spec]s. The result of a @tech{provide transformer}
must be a list of @racket[export]s, while the result of a
@tech{provide pre-transformer} is a syntax object to be used as a
@racket[_provide-spec] in the last phase of module expansion.

If a derived form contains a sub-form that is a
@racket[_provide-spec], then it can call @racket[expand-export] or
@racket[pre-expand-export] to transform the sub-@racket[_provide-spec]
sub-form.

See also @racket[define-provide-syntax], which supports macro-style
@tech{provide transformers}.


@defproc[(expand-export [stx syntax?] [modes (listof (or/c exact-integer? #f))])
         (listof export?)]{

Expands the given @racket[_provide-spec] to a list of exports. The
@racket[modes] list controls the expansion of
sub-@racket[_provide-specs]; for example, an identifier refers to a
binding in the @tech{phase level} of the enclosing @racket[provide]
form, unless the @racket[modes] list specifies otherwise. Normally,
@racket[modes] is either empty or contains a single element.}


@defproc[(pre-expand-export [stx syntax?] [modes (listof (or/c exact-integer? #f))])
         syntax?]{

Expands the given @racket[_provide-spec] at the level of @tech{provide
pre-transformers}. The @racket[modes] argument is the same as for
@racket[expand-export].}


@defproc*[([(make-provide-transformer [proc (syntax? (listof (or/c exact-integer? #f))
                                             . -> . (listof export?))])
            provide-transformer?]
           [(make-provide-transformer [proc (syntax? (listof (or/c exact-integer? #f))
                                             . -> . (listof export?))]
                                      [pre-proc (syntax? (listof (or/c exact-integer? #f))
                                                 . -> . syntax?)])
            (and/c provide-transformer? provide-pre-transformer?)])]{

Creates a @tech{provide transformer} (i.e., a structure with the
@racket[prop:provide-transformer] property) using the given procedure
as the transformer. If a @racket[pre-proc] is provided, then the result is also a
@tech{provide pre-transformer}.}


@defproc[(make-provide-pre-transformer [pre-proc (syntax? (listof (or/c exact-integer? #f))
                                                  . -> . syntax?)])
         provide-pre-transformer?]{

Like @racket[make-provide-transformer], but for a value that is a
@tech{provide pre-transformer}, only.}


@defthing[prop:provide-transformer struct-type-property?]{

A property to identify @tech{provide transformers}. The property
value must be a procedure that takes the structure and returns a transformer
procedure; the returned transformer procedure takes a syntax object and mode list and
returns an export list.}


@defthing[prop:provide-pre-transformer struct-type-property?]{

A property to identify @tech{provide pre-transformers}. The property
value must be a procedure that takes the structure and returns a transformer
procedure; the returned transformer procedure takes a syntax object and mode list and
returns a syntax object.}


@defproc[(provide-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the
@racket[prop:provide-transformer] property, @racket[#f] otherwise.}


@defproc[(provide-pre-transformer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the
@racket[prop:provide-pre-transformer] property, @racket[#f] otherwise.}


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

For backward compatibility only; returns a procedure that returns its
first argument.}

@; ----------------------------------------------------------------------

@section[#:tag "keyword-trans"]{Keyword-Argument Conversion Introspection}

@note-lib-only[racket/keyword-transform]

@deftogether[(
@defproc[(syntax-procedure-alias-property [stx syntax?])
         (or/c #f 
               (letrec ([val? (recursive-contract
                               (or/c (cons/c identifier? identifier?)
                                     (cons/c val? val?)))])
                 val?))]
@defproc[(syntax-procedure-converted-arguments-property [stx syntax?])
         (or/c #f 
               (letrec ([val? (recursive-contract
                               (or/c (cons/c identifier? identifier?)
                                     (cons/c val? val?)))])
                 val?))]
)]{

Reports the value of a syntax property that can be
attached to an identifier by the expansion of a keyword-application
form. See @racket[lambda] for more
information about the property.

The property value is normally a pair consisting of the original
identifier and an identifier that appears in the
expansion. Property-value merging via @racket[syntax-track-origin] can make
the value a pair of such values, and so on.}


@; ----------------------------------------------------------------------

@close-eval[stx-eval]
