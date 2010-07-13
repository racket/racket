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
                              make-base-empty-namespace
                              make-hash make-hasheq make-weak-hash make-weak-hasheq
                              make-immutable-hash
                              make-immutable-hasheq
			      exn:fail exn:fail:contract)))

@(define-syntax-rule (def-base base-define base-define-syntax base-define-for-syntax
                               base-define-struct
                               base-if base-cond base-case base-top-interaction
                               base-open-input-file base-apply base-prop:procedure
                               base-free-identifier=? base-free-template-identifier=?
                               base-free-transformer-identifier=? base-free-label-identifier=?)
   (begin
    (require (for-label scheme/base))
    (define base-define (scheme define))
    (define base-define-syntax (scheme define-syntax))
    (define base-define-for-syntax (scheme define-for-syntax))
    (define base-define-struct (scheme define-struct))
    (define base-if (scheme if))
    (define base-cond (scheme cond))
    (define base-case (scheme case))
    (define base-top-interaction (scheme #%top-interaction))
    (define base-open-input-file (scheme open-input-file))
    (define base-apply (scheme apply))
    (define base-prop:procedure (scheme prop:procedure))
    (define base-free-identifier=? (scheme free-identifier=?))
    (define base-free-template-identifier=? (scheme free-template-identifier=?))
    (define base-free-transformer-identifier=? (scheme free-transformer-identifier=?))
    (define base-free-label-identifier=? (scheme free-label-identifier=?))))
@(def-base base-define base-define-syntax base-define-for-syntax base-define-struct
           base-if base-cond base-case base-top-interaction
           base-open-input-file base-apply base-prop:procedure
           base-free-identifier=? base-free-template-identifier=?
           base-free-transformer-identifier=? base-free-label-identifier=?)

@(define old-vers @elem{version 372})

@title{@bold{MzScheme}: Legacy Module Language}

@defmodule[mzscheme]{

The @schememodname[mzscheme] language provides nearly the same
bindings as the @schememodname[mzscheme] module of PLT Scheme
@|old-vers| and earlier.}

Unlike @|old-vers|, the @schememodname[mzscheme] language does not
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
                       (code:line arg-id ... @#,schemeparenfont{.} rest-id)])]{

Like @|base-define| in @schememodname[scheme/base], but without
support for keyword arguments or optional arguments.}

@deftogether[(
@defform*[[(define-syntax id expr)
           (define-syntax (head args) body ...+)]]
@defform*[[(define-for-syntax id expr)
           (define-for-syntax (head args) body ...+)]]
)]{

Like @|base-define-syntax| and @|base-define-for-syntax| in
@schememodname[scheme/base], but without support for keyword arguments
or optional arguments (i.e., @scheme[head] is as for @scheme[define]).}

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

@defproc[(apply [proc procedure?] [v any/c] ... [lst list?]) any]{

Like @base-apply from @schememodname[scheme/base], but without support
for keyword arguments.}

@defthing[prop:procedure struct-type-property?]{

Like @base-prop:procedure from @schememodname[scheme/base], but even
if the property's value for a structure type is a procedure that
accepts keyword arguments, then instances of the structure type still
do not accept keyword arguments. (In contrast, if the property's value
is an integer for a field index, then a keyword-accepting procedure in
the field for an instance causes the instance to accept keyword
arguments.)}

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


@defproc*[([(hash-table? [v any/c]) 
            hash-table?]
           [(hash-table? [v any/c] [flag (one-of/c 'weak 'equal 'eqv)]) 
            hash-table?]
           [(hash-table? [v any/c] 
                         [flag (one-of/c 'weak 'equal 'eqv)]
                         [flag (one-of/c 'weak 'equal 'eqv)]) 
            hash-table?])]{

Returns @scheme[#t] if @scheme[v] like a hash table created by
@scheme[make-hash-table] or @scheme[make-immutable-hash-table] with
the given @scheme[flag]s (or more), @scheme[#f] otherwise. Each
provided @scheme[flag] must be distinct and @scheme['equal] cannot be
used with @scheme['eqv], otherwise the @scheme[exn:fail:contract]
exception is raised.}


@defproc*[([(make-hash-table) 
            hash-table?]
           [(make-hash-table [flag (one-of/c 'weak 'equal 'eqv)]) 
            hash-table?]
           [(make-hash-table [flag (one-of/c 'weak 'equal 'eqv)]
                             [flag (one-of/c 'weak 'equal 'eqv)]) 
            hash-table?])]{

Creates and returns a new hash table. If provided, each @scheme[flag]
must one of the following:

 @itemize[

  @item{@indexed-scheme['weak] --- creates a hash table with
   weakly-held keys via @scheme[make-weak-hash],
   @scheme[make-weak-hasheq], or @scheme[make-weak-hasheqv].}

  @item{@indexed-scheme['equal] --- creates a hash table that compares
   keys using @scheme[equal?] instead of @scheme[eq?] using
   @scheme[make-hash] or @scheme[make-weak-hash].}

  @item{@indexed-scheme['eqv] --- creates a hash table that compares
   keys using @scheme[eqv?] instead of @scheme[eq?] using
   @scheme[make-hasheqv] or @scheme[make-weak-hasheqv].}

 ]

By default, key comparisons use @scheme[eq?] (i.e., the hash table is
created with @scheme[make-hasheq]). If the second @scheme[flag] is
redundant or @scheme['equal] is provided with @scheme['eqv], the
@scheme[exn:fail:contract] exception is raised.}


@defproc*[([(make-immutable-hash-table [assocs (listof pair?)])
            (and/c hash-table? immutable?)]
           [(make-immutable-hash-table [assocs (listof pair?)]
                                       [flag (one-of/c 'equal 'eqv)])
            (and/c hash-table? immutable?)])]{

Like @scheme[make-immutable-hash], @scheme[make-immutable-hasheq], or
@scheme[make-immutable-hasheqv], depending on whether an
@scheme['equal] or @scheme['eqv] @scheme[flag] is provided.}
