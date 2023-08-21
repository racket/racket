#lang scribble/manual
@(require (for-label zuo-doc/fake-kernel
                     (except-in zuo-doc/fake-zuo
                                lambda
                                let
                                quote
                                if
                                define
                                begin))
          "real-racket.rkt")

@title[#:tag "zuo-kernel"]{Zuo Kernel Language}

@defmodulelang[zuo/kernel #:no-declare #:packages ()]
@declare-exporting[zuo-doc/fake-kernel #:packages ()]

The body of a @racketmodname[zuo/kernel] module is a single expression
using a set of core @seclink["kernel-syntax"]{syntactic forms}
and @seclink["kernel-primitives"]{primitives}. The expression
must produce a @tech{hash table} that serves as the module's
representation (see @secref["module-protocol"]).


@section[#:tag "kernel-syntax"]{Syntactic Forms}

@deftogether[(
@defform[#:link-target? #f #:id not-id id]
@defform[#:link-target? #f #:id not-literal literal]
@defform[#:link-target? #f #:id not-expr (expr expr ...)]
@defform[(lambda formals maybe-name maybe-arity-mask expr)
         #:grammar ([formals (id ...)
                             id
                             (id ... . id)]
                    [maybe-name string
                                code:blank]
                    [maybe-arity-mask integer
                                      code:blank])]
@defform[(quote datum)]
@defform[(if expr expr expr)]
@defform[(let ([id expr]) expr)]
@defform[(begin expr ...+)]
)]{

These forms are analogous to a variable reference, literal, procedure
application, @realracket*[lambda quote if let begin] in
@racketmodname[racket], but often restricted to a single expression or
binding clause. Unlike the corresponding @racketmodname[racket] or
@racketmodname[zuo] forms, the names of syntactic forms are not
shadowed by a @racket[lambda] or @racket[let] binding, and they refer
to syntactic forms only at the head of a term. A reference to an
unbound variable is a run-time error. If an @racket[id] appears
multiple times in @racket[formals], the last instance shadows the
others.

A @racket[lambda] form can optionally include a name and/or
arity mask. If an arity mask is provided, it must be a subset of the mask
implied by the @racket[formals]. If @racket[formals] allows 63 or more
arguments, then it must allow any number of arguments (to be
consistent with the possible arities expressed by a mask).

Although @racket[let] and @racket[begin] could be encoded with
@racket[lambda] easily enough, they're useful shortcuts to make
explicit internally.}


@section[#:tag "kernel-primitives"]{Primitives}

The following names provided by @racketmodname[zuo] are also available
in @racketmodname[zuo/kernel] (and the values originate there):

@racketblock[

  pair? null? list? cons car cdr list append reverse length
  list-ref list-set

  integer? + - * quotient modulo < <= = >= >
  bitwise-and bitwise-ior bitwise-xor bitwise-not

  string? string-length string-ref string-u32-ref substring string
  string=? string-ci=? string-sha256 string-split

  symbol? symbol->string string->symbol string->uninterned-symbol
  
  hash? hash hash-ref hash-set hash-remove
  hash-keys hash-count hash-keys-subset?

  procedure? apply call/cc call/prompt

  eq? not void

  opaque opaque-ref

  path-string? build-path build-raw-path split-path relative-path?
  module-path? build-module-path

  variable? variable variable-ref variable-set!

  handle? fd-open-input fd-open-output fd-close fd-read fd-write eof
  fd-terminal? cleanable-file cleanable-cancel

  stat ls rm mv mkdir rmdir symlink readlink cp
  runtime-env current-time

  process process-status process-wait string->shell shell->strings

  string-read ~v ~a ~s alert error arity-error arg-error 

  kernel-env kernel-eval module->hash dump-image-and-exit exit
  suspend-signal resume-signal

]
