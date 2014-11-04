#lang racket/base

;; This module provides compatibility macros for no-check mode

(require
 (except-in typed-racket/base-env/prims
            require/typed require/opaque-type require-typed-struct require/typed/provide)
 typed-racket/base-env/base-types-extra
 (for-syntax racket/base syntax/parse syntax/struct
             syntax/parse/experimental/template))
(provide (all-from-out racket/base)
         (all-defined-out)
         (all-from-out typed-racket/base-env/prims
                       typed-racket/base-env/base-types-extra))


(define-syntaxes (require/typed require/typed/provide)
  (let ()
    (define-syntax-class opt-rename
      #:attributes (nm spec)
      (pattern nm:id
               #:with spec #'nm)
      (pattern (orig-nm:id internal-nm:id)
               #:with spec #'(orig-nm internal-nm)
               #:with nm #'internal-nm))
    (define-syntax-class simple-clause
      #:attributes (nm ty name)
      (pattern [nm:opt-rename ty]
               #:with name (attribute nm.nm)))
    (define-syntax-class struct-clause
      #:attributes (nm (body 1) (opts 1))
      (pattern [(~or #:struct (~datum struct)) nm:opt-rename (body ...)
                                               opts:struct-option ...]))
    (define-syntax-class opaque-clause
      #:attributes (ty pred opt)
      (pattern [(~or #:opaque (~datum opaque)) ty:id pred:id]
               #:with opt #'())
      (pattern [(~or #:opaque (~datum opaque)) ty:id pred:id #:name-exists]
               #:with opt #'(#:name-exists)))
    (define-splicing-syntax-class struct-option
      (pattern (~seq #:constructor-name cname:id))
      (pattern (~seq #:extra-constructor-name extra-cname:id)))
    (values
     (syntax-parser 
       [(_ lib (~or sc:simple-clause strc:struct-clause oc:opaque-clause) ...)
        (template
         (begin
           (require/opaque-type oc.ty oc.pred lib . oc.opt) ...
           (require/typed sc.nm sc.ty lib) ...
           (require-typed-struct strc.nm (strc.body ...) (?@ . strc.opts) ... lib) ...))]
       [(_ nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
        #'(require (only-in lib nm.spec))]) 
     (syntax-parser
       [(_ lib (~or sc:simple-clause strc:struct-clause oc:opaque-clause) ...)
        (template
         (begin
           (require/opaque-type oc.ty oc.pred lib . oc.opt) ...
           (provide oc.pred) ...
           (require/typed sc.nm sc.ty lib) ...
           (provide sc.nm) ...
           (require-typed-struct strc.nm (strc.body ...) (?@ . strc.opts) ... lib) ...
           (provide (struct-out strc.nm)) ...))]
       [(_ nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
        #'(begin (require (only-in lib nm.spec))
                 (provide nm.nm))]))))

(define-syntax-rule (require/opaque-type ty pred lib . _)
  (require (only-in lib pred)))

(define-syntax (require-typed-struct stx)
  (syntax-parse stx #:literals (:)
    [(_ (~or nm:id (nm:id _:id)) ([fld : ty] ...)
        (~or (~and (~seq) (~bind [cname #'#f] [extra-cname #'#f]))
             (~and (~seq #:constructor-name cname)
                   (~bind [extra-cname #'#f]))
             (~and (~seq #:extra-constructor-name extra-cname)
                   (~bind [cname #'#f])))
        lib)
     (with-syntax ([(struct-info maker pred sel ...)
                    (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t
                                        #:constructor-name
                                        (if (syntax-e #'cname) #'cname #'nm))])
       #`(require (only-in lib
                           struct-info
                           maker
                           #,@(if (syntax-e #'extra-cname)
                                  #'(extra-cname)
                                  #'())
                           pred sel ...)))]))

