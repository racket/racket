#lang scheme/base

(require
 (except-in typed-racket/base-env/prims
            require/typed require/opaque-type require-typed-struct)
 typed-racket/base-env/base-types-extra
 (for-syntax scheme/base syntax/parse syntax/struct))
(provide (all-from-out scheme/base)
	 (all-defined-out)
	 (all-from-out typed-racket/base-env/prims 
                       typed-racket/base-env/base-types-extra))


(define-syntax (require/typed stx)
  (define-syntax-class opt-rename
    #:attributes (nm spec)
    (pattern nm:id
             #:with spec #'nm)
    (pattern (orig-nm:id internal-nm:id)
	     #:with spec #'(orig-nm internal-nm)
	     #:with nm #'internal-nm))
  (define-syntax-class simple-clause
    #:attributes (nm ty)
    (pattern [nm:opt-rename ty]))
  (define-syntax-class struct-clause
    ;#:literals (struct)
    #:attributes (nm (body 1))
    (pattern [struct nm:opt-rename (body ...)]
             #:fail-unless (eq? 'struct (syntax-e #'struct)) #f))
  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [opaque ty:id pred:id]
             #:fail-unless (eq? 'opaque (syntax-e #'opaque)) #f
             #:with opt #'())
    (pattern [opaque ty:id pred:id #:name-exists]
             #:fail-unless (eq? 'opaque (syntax-e #'opaque)) #f
             #:with opt #'(#:name-exists)))
  (syntax-parse stx
    [(_ lib (~or sc:simple-clause strc:struct-clause oc:opaque-clause) ...)
     #'(begin
	 (require/opaque-type oc.ty oc.pred lib . oc.opt) ...
	 (require/typed sc.nm sc.ty lib) ...
	 (require-typed-struct strc.nm (strc.body ...) lib) ...)]
    [(_ nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
     #'(require (only-in lib nm.spec))]))

(define-syntax-rule (require/opaque-type ty pred lib . _)
  (require (only-in lib pred)))

(define-syntax (require-typed-struct stx)
  (syntax-parse stx #:literals (:)
    [(_ (~or nm:id (nm:id _:id)) ([fld : ty] ...) lib)
     (with-syntax ([(struct-info maker pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)])
       #'(require (only-in lib struct-info maker pred sel ...)))]))

