#lang scheme/base

(require (for-syntax scheme/base syntax/parse mzlib/etc scheme/match)
         scheme/require 
         "base-env.ss" 
	 "base-special-env.ss"
	 "base-env-numeric.ss"
	 "base-env-indexing-old.ss"
	 "extra-procs.ss"
         "prims.ss"
         "base-types.ss"
         scheme/contract/regions scheme/contract/base
         (for-syntax "base-types-extra.ss")
         (for-syntax (except-in (path-up "utils/utils.ss") infer)
                     (path-up "utils/tc-utils.ss")
                     (except-in (combine-in (path-up "types/convenience.ss") (path-up "types/abbrev.ss")) ->)                     
                     (path-up "types/utils.ss")
                     (path-up "infer/infer.ss")
                     (path-up "env/type-env.ss")
                     (path-up "env/type-environments.ss")
                     (path-up "env/type-name-env.ss")
                     (path-up "env/type-alias-env.ss")
                     (path-up "infer/infer-dummy.ss")
                     (path-up "private/parse-type.ss")
                     (path-up "private/type-contract.ss")
                     (path-up "typecheck/typechecker.ss")))

(provide with-type)
(define-syntax (with-type stx)
  (define-splicing-syntax-class free-vars
    #:attributes ((id 1) (ty 1))
    [pattern (~seq #:freevars ([id ty] ...))]
    [pattern (~seq)
             #:with (id ...) null
             #:with (ty ...) null])
  (syntax-parse stx
    [(_ region-ty-stx fv:free-vars . body)
     (begin-with-definitions
       (define old-context (unbox typed-context?))
       (set-box! typed-context? #t)
       (define region-tc-result (parse-tc-results #'region-ty-stx))
       (define region-cnt (match region-tc-result 
                            [(tc-result1: t) (type->contract 
                                              t 
                                              (lambda () (tc-error/stx #'region-ty-stx "Type ~a could not be converted to a contract." t)))]))
       (define fv-types (for/list ([t (syntax->list #'(fv.ty ...))])
                          (parse-type t)))
       (define fv-cnts (for/list ([t (in-list fv-types)]
                                  [stx (in-list (syntax->list #'(fv.ty ...)))])
                         (type->contract t #:typed-side #f 
                                         (lambda () (tc-error/stx stx "Type ~a could not be converted to a contract." t)))))
       (for ([i (in-list (syntax->list #'(fv.id ...)))]
             [ty (in-list fv-types)])
         (register-type i ty))
       (define expanded-body (local-expand #'(let () . body) 'expression null))
       (parameterize (;; disable fancy printing?
                      [custom-printer #t]
                      ;; a cheat to avoid units
                      [infer-param infer]
                      ;; do we report multiple errors
                      [delay-errors? #t]
                      ;; this parameter is for parsing types
                      [current-tvars initial-tvar-env]
                      ;; this parameter is just for printing types
                      ;; this is a parameter to avoid dependency issues
                      [current-type-names
                       (lambda ()
                         (append 
                          (type-name-env-map (lambda (id ty)
                                               (cons (syntax-e id) ty)))
                          (type-alias-env-map (lambda (id ty)
                                                (cons (syntax-e id) ty)))))]
                      ;; reinitialize seen type variables
                      [type-name-references null]
                      ;; for error reporting
                      [orig-module-stx stx]
                      [expanded-module-stx expanded-body])
         (tc-expr/check expanded-body region-tc-result))
       (report-all-errors)
       (set-box! typed-context? old-context)
       ;; then clear the new entries from the env ht
       (for ([i (in-list (syntax->list #'(fv.id ...)))])
         (unregister-type i))
       (with-syntax ([(cnt ...) fv-cnts]
                     [region-cnt region-cnt]
                     [check-syntax-help (syntax-property #'(void) 'disappeared-use (type-name-references))])
         (quasisyntax/loc stx
           (begin check-syntax-help
                  (with-contract typed-region
                                 #:result region-cnt
                                 #:freevars ([fv.id cnt] ...)                                
                                 . body)))))]))
 