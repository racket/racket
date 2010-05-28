#lang scheme/base

(require (for-syntax scheme/base syntax/parse mzlib/etc scheme/match)
         scheme/require 
         "base-env.rkt" 
	 "base-special-env.rkt"
	 "base-env-numeric.rkt"
	 "base-env-indexing.rkt"
	 "extra-procs.rkt"
         "prims.rkt"
         racket/contract/regions racket/contract/base
         (for-syntax 
          "base-types-extra.rkt"
          unstable/debug
          (path-up "env/type-name-env.rkt"
                   "env/type-alias-env.rkt"
                   "infer/infer-dummy.rkt"
                   "private/parse-type.rkt"
                   "private/type-contract.rkt"
                   "typecheck/typechecker.rkt"
                   "env/type-environments.rkt" 
                   "env/type-env.rkt"
                   "infer/infer.rkt"
                   "utils/tc-utils.rkt"
                   "types/utils.rkt")
          (except-in (path-up "utils/utils.rkt" "types/convenience.rkt" "types/abbrev.rkt") infer ->)))

(provide with-type)

(define-for-syntax (with-type-helper stx body fvids fvtys exids extys resty expr? ctx)
  (begin-with-definitions
    (define old-context (unbox typed-context?))
    (set-box! typed-context? #t)
    (define fv-types (for/list ([t (in-list (syntax->list fvtys))])
                       (parse-type t)))
    (define fv-cnts (for/list ([t (in-list fv-types)]
                               [stx (in-list (syntax->list fvtys))])
                      (type->contract t #:typed-side #f 
                                      (lambda () (tc-error/stx stx "Type ~a could not be converted to a contract." t)))))
    (define ex-types (for/list ([t (syntax->list extys)])
                       (parse-type t)))
    (define ex-cnts (for/list ([t (in-list ex-types)]
                               [stx (in-list (syntax->list extys))])
                      (type->contract t #:typed-side #t
                                      (lambda () (tc-error/stx stx "Type ~a could not be converted to a contract." t)))))
    (define region-tc-result 
      (and expr? (parse-tc-results resty)))
    (define region-cnts 
      (if region-tc-result
          (match region-tc-result 
            [(tc-result1: t)
             (list (type->contract 
                    t 
                    #:typed-side #t
                    (lambda () (tc-error/stx #'region-ty-stx "Type ~a could not be converted to a contract." t))))]
            [(tc-results: ts)
             (for/list ([t (in-list ts)])
               (type->contract
                t #:typed-side #t
                (lambda () (tc-error/stx #'region-ty-stx "Type ~a could not be converted to a contract." t))))])
          null))
    (for ([i (in-list (syntax->list fvids))]
          [ty (in-list fv-types)])
      (register-type i ty))
    (define expanded-body 
      (if expr?
          (with-syntax ([body body])
            (local-expand #'(let () . body) ctx null))
          (with-syntax ([(body ...) body]
                        [(id ...) exids]
                        [(ty ...) extys])
            (local-expand #'(let () (begin (: id ty) ... body ... (values id ...))) ctx null))))
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
      (if expr?
          (tc-expr/check expanded-body region-tc-result)
          (tc-expr/check expanded-body (ret ex-types))))
    (report-all-errors)
    (set-box! typed-context? old-context)
    ;; then clear the new entries from the env ht
    (for ([i (in-list (syntax->list fvids))])
      (unregister-type i))
    (with-syntax ([(fv.id ...) fvids]
                  [(cnt ...) fv-cnts]
                  [(ex-id ...) exids]
                  [(ex-cnt ...) ex-cnts]
                  [(region-cnt ...) region-cnts]
                  [body expanded-body]
                  [check-syntax-help (syntax-property #'(void) 'disappeared-use (type-name-references))])
      (if expr?
          (quasisyntax/loc stx
            (begin check-syntax-help
                   (with-contract typed-region
                     #:results (region-cnt ...)
                     #:freevars ([fv.id cnt] ...)                                
                     body)))
          (syntax/loc stx
            (begin
              (define-values () (begin check-syntax-help (values)))
              (with-contract typed-region
                ([ex-id ex-cnt] ...)
                (define-values (ex-id ...) body))))))))

(define-syntax (with-type stx)
  (define-syntax-class typed-id
    #:description "[id type]"
    [pattern (id ty)])
  (define-splicing-syntax-class free-vars
    #:description "free variable specification"
    #:attributes ((id 1) (ty 1))
    [pattern (~seq #:freevars (:typed-id ...))]
    [pattern (~seq)
             #:with (id ...) null
             #:with (ty ...) null])
  (define-syntax-class typed-ids
    #:description "sequence of typed identifiers"
    #:attributes ((id 1) (ty 1))
    [pattern (t:typed-id ...)
             #:with (id ...) #'(t.id ...)
             #:with (ty ...) #'(t.ty ...)])
  (define-splicing-syntax-class result-ty
    #:description "result specification"
    [pattern (~seq #:result ty:expr)])
  (syntax-parse stx
    [(_ :typed-ids fv:free-vars . body)
     (with-type-helper stx #'body #'(fv.id ...) #'(fv.ty ...) #'(id ...) #'(ty ...) #f #f (syntax-local-context))]
    [(_ :result-ty fv:free-vars . body)
     (with-type-helper stx #'body #'(fv.id ...) #'(fv.ty ...) #'() #'() #'ty #t (syntax-local-context))]))
 
