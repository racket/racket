#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/serialize))

@(begin
   (define-syntax-rule (bind id id2)
     (begin
       (require (for-label racket/serialize))
       (define id (racket define-serializable-struct))
       (define id2 (racket define-serializable-struct/versions))))
   (bind racket-define-serializable-struct
         racket-define-serializable-struct/versions))

@mzlib[#:mode title serialize]

@deprecated[@racketmodname[racket/serialize]]{}

The @racketmodname[mzlib/serialize] library provides the same bindings
as @racketmodname[racket/serialize], except that
@racket[define-serializable-struct] and
@racket[define-serializable-struct/versions] are based on the syntax
of @racket[define-struct] from @racketmodname[mzscheme].

@deftogether[(
@defform[(define-serializable-struct id-maybe-super (field-id ...) maybe-inspector-expr)]
@defform/subs[(define-serializable-struct/versions id-maybe-super vers-num (field-id ...)
                                                   (other-version-clause ...)
                                                   maybe-inspector-expr)
              ([id-maybe-super id
                               (id super-id)]
               [maybe-inspector-expr code:blank
                                     inspector-expr]
               [other-version-clause (other-vers make-proc-expr 
                                                 cycle-make-proc-expr)])]
)]{

Like @racket-define-serializable-struct and
@racket-define-serializable-struct/versions, but with the syntax of
closer to @racket[define-struct] of @racketmodname[mzscheme].}
