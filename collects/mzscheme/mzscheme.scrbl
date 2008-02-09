#lang scribble/doc
@(require scribble/manual
          (for-label mzscheme
                     (only-in r5rs set-car! set-cdr!)
                     (only-in scheme/base
                              for-syntax #%plain-module-begin
                              exact-nonnegative-integer?
                              exact-positive-integer?
                              syntax?
                              #%plain-lambda #%plain-app
                              syntax->datum datum->syntax
                              make-base-empty-namespace)))

@(define-syntax-rule (def-base base-define base-define-struct
                               base-if base-cond base-case base-top-interaction
                               base-open-input-file
                               base-free-identifier=? base-free-template-identifier=?
                               base-free-transformer-identifier=? base-free-label-identifier=?)
   (begin
    (require (for-label scheme/base))
    (define base-define (scheme define))
    (define base-define-struct (scheme define-struct))
    (define base-if (scheme if))
    (define base-cond (scheme cond))
    (define base-case (scheme case))
    (define base-top-interaction (scheme #%top-interaction))
    (define base-open-input-file (scheme open-input-file))
    (define base-free-identifier=? (scheme free-identifier=?))
    (define base-free-template-identifier=? (scheme free-template-identifier=?))
    (define base-free-transformer-identifier=? (scheme free-transformer-identifier=?))
    (define base-free-label-identifier=? (scheme free-label-identifier=?))))
@(def-base base-define base-define-struct
           base-if base-cond base-case base-top-interaction
           base-open-input-file
           base-free-identifier=? base-free-template-identifier=?
           base-free-transformer-identifier=? base-free-label-identifier=?)

@(define old-vers @elem{version 372})

@title{@bold{MzScheme}: Legacy Module Language}

@defmodule[mzscheme]{

The @schememodname[mzscheme] language provides nearly the same
bindings as the @schememodname[mzscheme] module of PLT Scheme
@|old-vers| and earlier.}

Unlike old version, the @schememodname[mzscheme] language does not
include @scheme[set-car!]  or @scheme[set-cdr!], and @scheme[cons]
makes immutable pairs, as in @scheme[scheme/base]; those changes make
modules built on @schememodname[mzscheme] reasonably compatible with
modules built on @schememodname[scheme/base].

Otherwise, the @schememodname[mzscheme] language shares many bindings
with @schememodname[scheme/base]. It renames a few bindings, such as
@scheme[syntax-object->datum] instead of @scheme[syntax->datum], and
it provides old versions of some syntactic forms, such as
@scheme[lambda] without support for keyword and optional arguments.

@table-of-contents[]

@; ----------------------------------------

@section{Old Syntactic Forms}

@defform[(#%module-begin form ...)]{

Like @scheme[#%plain-module-begin] from @schememodname[scheme/base],
but @scheme[(require-for-syntax mzscheme)] is added to the beginning
of the @scheme[form] sequence, thus importing @schememodname[mzscheme]
into the transformer environment for the module body. (In contrast,
@schememodname[scheme/base] exports @scheme[for-syntax] minimal
transformer support, while @schememodname[scheme] exports all of
@schememodname[scheme/base] @scheme[for-syntax].}


@defform[(#%plain-module-begin form ...)]{

The same binding as @scheme[#%plain-module-begin] from
@schememodname[scheme/base].}


@defform[(#%plain-lambda formals body ...+)]{

The same binding as @scheme[#%plain-lambda] in
@schememodname[scheme/base].  (This binding was not present in
@|old-vers| and earlier.)}


@deftogether[(
@defform[(lambda formals body ...+)]
@defform[(λ formals body ...+)]
)]{

The same bindings as @scheme[#%plain-lambda].}


@defform*[[(#%app proc-expr arg-expr ...)
           (#%app)]]{

The same binding as @scheme[#%plain-app] from
@schememodname[scheme/base].}

@defform*[[(#%plain-app proc-expr arg-expr ...)
           (#%plain-app)]]{

The same binding as @scheme[#%app].  (This binding was not present in
@|old-vers| and earlier.)}

@defform*/subs[[(define id expr)
                (define (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg-id ...)
                       (code:line arg-id ... #, @schemeparenfont{.} rest-id)])]{

Like @|base-define| in @schememodname[scheme/base], but without
support for keyword arguments or optional arguments.}

@defform*[[(if test-expr then-expr else-expr)
           (if test-expr then-expr)]]{

Like @|base-if| in @schememodname[scheme/base], but @scheme[else-expr]
defaults to @scheme[(void)].}

@deftogether[(
@defform[(cond cond-clause ...)]
@defform[(case val-expr case-clause ...)]
)]{

Like @|base-cond| and @|base-case| in @schememodname[scheme/base], but
@scheme[else] and @scheme[=>] are recognized as unbound identifiers,
instead of as the @schememodname[scheme/base] bindings. }

@defform[(fluid-let ([id expr] ...) body ...+)]{

Provides a kind of dynamic binding via mutation of the @scheme[id]s.

The @scheme[fluid-let] form first evaluates each @scheme[expr] to
obtain an @defterm{entry value} for each @scheme[id]. As evaluation
moves into @scheme[body], either though normal evaluation or a
continuation jump, the current value of each @scheme[id] is swapped
with the entry value. On exit from @scheme[body], then the current
value and entry value are swapped again.}

@defform/subs[(define-struct id-maybe-super (field-id ...) maybe-inspector-expr)
              ([maybe-inspector-expr code:blank
                                     expr])]{

Like @base-define-struct from @scheme[scheme/base], but with fewer
options. Each field is implicitly mutable, and the optional
@scheme[expr] is analogous to supplying an @scheme[#:inspector]
expression.}

@defform[(let-struct id-maybe-super (field-id ...) body ...+)]{

Expands to 

@schemeblock[
(let ()
  (define-struct id-maybe-super (field-id ...))
  body ...+)
]}

@deftogether[(
@defform[(require raw-require-spec)]
@defform[(require-for-syntax raw-require-spec)]
@defform[(require-for-template raw-require-spec)]
@defform[(require-for-label raw-require-spec)]
@defform[(provide raw-provide-spec)]
@defform[(provide-for-syntax raw-provide-spec)]
@defform[(provide-for-label raw-provide-spec)]
)]{

Like @scheme[#%require] and @scheme[#%provide]. The
@schemeidfont{-for-syntax}, @schemeidfont{-for-template}, and
@schemeidfont{-for-label} forms are translated to @scheme[#%require]
and @scheme[#%provide] using @schemeidfont{for-syntax},
@schemeidfont{for-template}, and @schemeidfont{for-label} sub-forms,
respectively.}

@defform[(#%datum . datum)]{

Expands to @scheme[(quote datum)], even if @scheme[datum] is a
keyword.}

@defform[(#%top-interaction . form)]{

The same as @|base-top-interaction| in @schememodname[scheme/base].}

@; ----------------------------------------

@section{Old Functions}

@deftogether[(
@defproc[(open-input-file [file path-string?] [mode (one-of/c 'text 'binary) 'binary])
         input-port?]
@defproc[(open-output-file [file path-string?]
                           [mode (one-of/c 'text 'binary) 'binary]
                           [exists (one-of/c 'error 'append 'update
                                             'replace 'truncate 'truncate/replace) 'error])
         input-port?]
@defproc[(open-input-output-file [file path-string?]
                           [mode (one-of/c 'text 'binary) 'binary]
                           [exists (one-of/c 'error 'append 'update
                                             'replace 'truncate 'truncate/replace) 'error])
         (values input-port? output-port?)]
@defproc[(with-input-from-file [file path-string?] 
                               [thunk (-> any)]
                               [mode (one-of/c 'text 'binary) 'binary])
         any]
@defproc[(with-output-to-file [file path-string?] 
                              [thunk (-> any)]
                              [mode (one-of/c 'text 'binary) 'binary]
                              [exists (one-of/c 'error 'append 'update
                                                'replace 'truncate 'truncate/replace) 'error])
         any]
@defproc[(call-with-input-file [file path-string?] 
                               [proc (input-port? -> any)]
                               [mode (one-of/c 'text 'binary) 'binary])
         any]
@defproc[(call-with-output-file [file path-string?] 
                                [proc (output-port? -> any)]
                                [mode (one-of/c 'text 'binary) 'binary]
                                [exists (one-of/c 'error 'append 'update
                                                  'replace 'truncate 'truncate/replace) 'error])
         any]
)]{

Like @base-open-input-file, etc. from @schememodname[scheme/base], but
@scheme[mode] and @scheme[exists] arguments are not keyword
arguments. When both @scheme[mode] and @scheme[exists] are accepted,
they are accepted in either order.}


@deftogether[(
@defproc[(syntax-object->datum [stx syntax?]) any]
@defproc[(datum->syntax-object [ctxt (or/c syntax? false/c)]
                        [v any/c]
                        [srcloc (or/c syntax? false/c
                                      (list/c any/c
                                              (or/c exact-positive-integer? false/c)
                                              (or/c exact-nonnegative-integer? false/c)
                                              (or/c exact-nonnegative-integer? false/c)
                                              (or/c exact-positive-integer? false/c))
                                      (vector/c any/c
                                                (or/c exact-positive-integer? false/c)
                                                (or/c exact-nonnegative-integer? false/c)
                                                (or/c exact-nonnegative-integer? false/c)
                                                (or/c exact-positive-integer? false/c)))]
                        [prop (or/c syntax? false/c) #f]
                        [cert (or/c syntax? false/c) #f])
          syntax?]
)]{

The same as @scheme[syntax->datum] and @scheme[datum->syntax].}

@deftogether[(
@defproc[(module-identifier=? [a-id syntax?][b-id syntax?]) boolean?]
@defproc[(module-transformer-identifier=? [a-id syntax?][b-id syntax?]) boolean?]
@defproc[(module-template-identifier=? [a-id syntax?][b-id syntax?]) boolean?]
@defproc[(module-label-identifier=? [a-id syntax?][b-id syntax?]) boolean?]
@defproc[(free-identifier=? [a-id syntax?][b-id syntax?]) boolean?]
)]{

The @scheme[module-identifier=?], @|etc| functions are the same as
@base-free-identifier=?, @|etc| in @schememodname[scheme/base].

The @scheme[free-identifier=?] procedure returns

@schemeblock[
(and (eq? (syntax-e a) (syntax-e b)) 
     (module-identifier=? a b))
]}


@defproc[(make-namespace [mode (one-of/c 'initial 'empty) 'initial]) namespace?]{

Creates a namespace with @schememodname[mzscheme] attached. If the
@scheme[mode] is empty, the namespace's top-level environment is left
empty. If @scheme[mode] is @scheme['initial], then the namespace's
top-level environment is initialized with
@scheme[(namespace-require/copy 'mzscheme)]. See also
@scheme[make-base-empty-namespace].}


@defproc[(namespace-transformer-require [req any/c]) void?]{

Equivalent to @scheme[(namespace-require `(for-syntax ,req))].}

@deftogether[(
@defproc[(transcript-on [filename any/c]) any]
@defproc[(transcript-off) any]
)]{

Raises @scheme[exn:fail], because the operations are not supported.}
