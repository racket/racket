#lang scribble/manual
@(require (for-syntax racket)
          (for-label (only-in scheme/foreign unsafe! provide* define-unsafer)))

@(define-syntax-rule (def-extras unit-struct)
  (begin
    (require (for-label scheme))
    (define unit-struct (racket struct))))
@(def-extras unit-struct)

@(define-syntax-rule (compat-except sid rid . rest)
   (begin
     @section[@schememodname[sid]]
     @defmodule[sid]
     "The " @schememodname[sid] " library re-exports " @racketmodname[rid] (begin . rest) "."))
@(define-syntax-rule (compat sid rid)
   (compat-except sid rid))

@title{@bold{Scheme}: Compatibility Libraries}

Racket was once called ``PLT Scheme,'' and a number of libraries with
names starting @schemeidfont{scheme} provide compatibility with the
old name.

@table-of-contents[]

@compat-except[scheme racket]{, except that @schememodname[racket]'s
@scheme[struct] is not exported, and a @|unit-struct| from
@schememodname[scheme/unit] is exported, instead}

@compat-except[scheme/base racket/base]{, except that
@schememodname[racket]'s @scheme[struct] is not exported}

@compat[scheme/async-channel racket/async-channel]
@compat[scheme/bool racket/bool]
@compat[scheme/class racket/class]
@compat[scheme/cmdline racket/cmdline]
@compat[scheme/contract racket/contract]
@compat[scheme/control racket/control]
@compat[scheme/date racket/date]
@compat[scheme/dict racket/dict]
@; @compat[scheme/fasl racket/fasl]
@compat[scheme/file racket/file]
@compat[scheme/fixnum racket/fixnum]
@compat[scheme/flonum racket/flonum]

@; ----------------------------------------------------------------------

@compat-except[scheme/foreign racket/unsafe/ffi]{, except that @scheme[unsafe!]
must be used to import the unsafe bindings of @scheme[racket/unsafe/ffi]}

@defform[(unsafe!)]{

Makes unsafe bindings available.}


@defform/subs[#:literals (unsafe rename-out)
              (provide* provide-star-spec ...)
              ([provide-star-spec (unsafe id)
                                  (unsafe (rename-out [id external-id]))
                                  provide-spec])]{

Like @scheme[provide], but @scheme[id]s under @scheme[unsafe] are not
actually provided. Instead, they are collected for introduction into
an importing module via a macro created by @scheme[define-unsafer].}

@defform[(define-unsafer id)]{

Cooperates with @scheme[provide*] to define @scheme[id] as a
@scheme[unsafe!]-like form that introduces definitions for each
binding provided as @scheme[unsafe].  The @scheme[define-unsafer] form
must occur after all the @scheme[provide*] forms to which it refers.}

@; ----------------------------------------------------------------------

@compat[scheme/function racket/function]
@compat[scheme/future racket/future]
@compat[scheme/generator racket/generator]
@compat-except[scheme/gui racket/gui]{, except that it builds on
@schememodname[scheme] instead of @schememodname[racket]}
@compat[scheme/gui/base racket/gui/base]
@compat[scheme/gui/dynamic racket/gui/dynamic]
@compat[scheme/help racket/help]
@compat[scheme/include racket/include]
@compat[scheme/init racket/init]
@compat[scheme/list racket/list]
@compat[scheme/load racket/load]
@compat[scheme/local racket/local]
@compat[scheme/match racket/match]
@compat[scheme/math racket/math]
@compat[scheme/mpair racket/mpair]
@compat[scheme/nest racket/nest]
@compat[scheme/package racket/package]
@compat[scheme/path racket/path]
@compat[scheme/port racket/port]
@compat[scheme/pretty racket/pretty]
@compat[scheme/promise racket/promise]
@compat[scheme/provide racket/provide]
@compat[scheme/provide-syntax racket/provide-syntax]
@compat[scheme/provide-transform racket/provide-transform]
@compat[scheme/require racket/require]
@compat[scheme/require-syntax racket/require-syntax]
@compat[scheme/require-transform racket/require-transform]
@compat[scheme/runtime-path racket/runtime-path]
@compat[scheme/serialize racket/serialize]
@compat[scheme/set racket/set]
@compat[scheme/signature racket/signature]
@compat[scheme/shared racket/shared]
@compat[scheme/splicing racket/splicing]
@compat[scheme/string racket/string]
@compat[scheme/struct-info racket/struct-info]
@compat[scheme/stxparam racket/stxparam]
@compat[scheme/stxparam-exptime racket/stxparam-exptime]
@compat[scheme/surrogate racket/surrogate]
@compat[scheme/system racket/system]
@compat[scheme/tcp racket/tcp]
@; @compat[scheme/trace racket/trace]
@compat[scheme/trait racket/trait]
@compat[scheme/udp racket/udp]
@compat[scheme/unit racket/unit]
@compat[scheme/unit-exptime racket/unit-exptime]
@compat[scheme/unsafe/ops racket/unsafe/ops]
@compat[scheme/vector racket/vector]

