#lang scheme/base
(require scheme/foreign
         (for-syntax scheme/base
                     scheme/provide-transform))

(error 'unsafe! "only `for-label' use in the documentation")

(unsafe!)

;; This is like `all-defined-out', but it ignores the 'not-provide-all-defined
;;  property, so that the bindings introduced by `unsafe!' are exported.
(define-syntax all-unsafe-defined-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_) 
        (let-values ([(ids stx-ids) (syntax-local-module-defined-identifiers)]
                     [(same-ctx?) (lambda (free-identifier=?)
                                      (lambda (id)
                                        (free-identifier=? id
                                                           (datum->syntax
                                                            stx
                                                            (syntax-e id)))))])
          (map (lambda (id)
                 (make-export id (syntax-e id) 0 #f stx))
               (filter (same-ctx? free-identifier=?)
                       ids)))]))))

(provide (protect-out (all-unsafe-defined-out))
         (all-from-out scheme/foreign))

