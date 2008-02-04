#lang scribble/doc
@(require scribble/manual
          (for-label mzscheme
                     (only-in r5rs set-car! set-cdr!)
                     (only-in scheme/base
                              exact-nonnegative-integer?
                              exact-positive-integer?
                              syntax?
                              #%plain-lambda #%plain-app
                              syntax->datum datum->syntax)))

@(define-syntax-rule (def-base base-define  
                               base-free-identifier=? base-free-template-identifier=?
                               base-free-transformer-identifier=? base-free-label-identifier=?)
   (begin
    (require (for-label scheme/base))
    (define base-define (scheme define))
    (define base-free-identifier=? (scheme base-free-identifier=?))
    (define base-free-template-identifier=? (scheme base-free-template-identifier=?))
    (define base-free-transformer-identifier=? (scheme base-free-transformer-identifier=?))
    (define base-free-label-identifier=? (scheme base-free-label-identifier=?))))
@(def-base base-define
           base-free-identifier=? base-free-template-identifier=?
           base-free-transformer-identifier=? base-free-label-identifier=?)


@title{@bold{MzScheme}: Legacy Module Language}

@defmodule[mzscheme]{

The @schememodname[mzscheme] language provides nearly the same
bindings as the @schememodname[mzscheme] module of PLT Scheme version
372 and earlier.}

Unlike old version, the @schememodname[mzscheme] language does not
include @scheme[set-car!]  or @scheme[set-cdr!], and @scheme[cons]
makes immutable pairs, as in @scheme[scheme/base]; those changes make
modules built on @schememodname[mzscheme] reasonably compatible with
modules built on @schememodname[scheme/base].

Otherwise, the @schememodname[mzscheme] language provides some
functions in @schememodname[scheme/base] under old names, such as
@scheme[syntax-object->datum] instead of @scheme[syntax->datum], and
it provides old versions of some syntactic forms, such as
@scheme[lambda] without support for keyword and optional arguments.

@table-of-contents[]

@; ----------------------------------------

@section{Old Syntactic Forms}

@defform[(lambda formals body ...+)]{

The same as @scheme[#%plain-lambda].}


@defform*[[(#%app proc-expr arg-expr ...)
           (#%app)]]{

The same as @scheme[#%plain-app].}


@defform*/subs[[(define id expr)
                (define (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg-id ...)
                       (code:line arg-id ... #, @schemeparenfont{.} rest-id)])]{

Like @|base-define| in @schememodname[scheme/base], but without
support for keyword arguments or optional arguments.}

@; ----------------------------------------

@section{Old Functions}

@defproc[(syntax-object->datum [stx syntax?]) any]{

The same as @scheme[syntax->datum].}

@defproc[(datum->syntax-object [ctxt (or/c syntax? false/c)]
                        [v any/c]
                        [srcloc (or/c syntax? false/c
                                      (list/c any/c
                                              (or/c exact-positive-integer? false/c)
                                              (or/c exact-nonnegative-integer? false/c)
                                              (or/c exact-nonnegative-integer? false/c)
                                              (or/c exact-positive-integer? false/c)))]
                        [prop (or/c syntax? false/c) #f]
                        [cert (or/c syntax? false/c) #f])
          syntax?]{

The same as @scheme[datum->syntax].}


@defproc[(module-identifier=? [a-id syntax?][b-id syntax?]) boolean?]{

The same as @base-free-identifier=? in @schememodname[scheme/base].}

@defproc[(module-transformer-identifier=? [a-id syntax?][b-id syntax?]) boolean?]{

The same as @base-free-transformer-identifier=? in @schememodname[scheme/base].}

@defproc[(module-template-identifier=? [a-id syntax?][b-id syntax?]) boolean?]{

The same as @base-free-template-identifier=? in @schememodname[scheme/base].}

@defproc[(module-label-identifier=? [a-id syntax?][b-id syntax?]) boolean?]{

The same as @base-free-label-identifier=? in @schememodname[scheme/base].}


