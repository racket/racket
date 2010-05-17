#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/serialize))

@(begin
  (define-syntax-rule (bind id id2)
    (begin
     (require (for-label scheme/serialize))
     (define id (scheme define-serializable-struct))
     (define id2 (scheme define-serializable-struct/versions))))
  (bind scheme-define-serializable-struct scheme-define-serializable-struct/versions))

@mzlib[#:mode title serialize]

The @schememodname[mzlib/serialize] library provides the same bindings
as @schememodname[scheme/serialize], except that
@scheme[define-serializable-struct] and
@scheme[define-serializable-struct/versions] are based on the syntax
of @scheme[define-struct] from @schememodname[mzscheme].

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

Like @scheme-define-serializable-struct and
@scheme-define-serializable-struct/versions, but with the syntax of
closer to @scheme[define-struct] of @schememodname[mzscheme].}
