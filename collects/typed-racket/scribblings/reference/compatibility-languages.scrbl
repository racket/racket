#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/scheme for])))]

@title{Compatibility Languages}

@(defmodulelang*/no-declare (typed/scheme typed/scheme/base typed-scheme))
Typed versions of the @racketmod[scheme] and @racketmod[scheme/base]
languages. The @racketmod[typed-scheme] language is equivalent to the
@racketmod[typed/scheme/base] language.

@(declare-exporting typed/scheme/base typed/scheme typed-scheme
                    #:use-sources
                    (typed-racket/typed-racket
                     typed-racket/base-env/prims
                     typed-racket/base-env/extra-procs
                     typed-racket/base-env/base-types
                     typed-racket/base-env/base-types-extra))

@(define-syntax-rule (def-racket rts rt)
  (begin
    (require (for-label (only-in typed/racket/base require-typed-struct require/typed)))
    (define rts (racket require-typed-struct))
    (define rt (racket require/typed))))
@(def-racket rts-id rt-id)


@defform/subs[#:literals (struct opaque)
(require/typed m rt-clause ...)
([rt-clause [r t]
            [struct name ([f : t] ...)
                 struct-option ...]
            [struct (name parent) ([f : t] ...)
                 struct-option ...]
            [opaque t pred]]
 [struct-option
   (code:line #:constructor-name constructor-id)
   (code:line #:extra-constructor-name constructor-id)])]{
 Similar to @|rt-id|, but as if @racket[#:extra-constructor-name make-name] was supplied.
}

@defidform[require-typed-struct]{Similar to using the @racket[struct]
keyword with @racket[require/typed].}
