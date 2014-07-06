#lang racket/base

(require racket/require racket/promise
         (for-template
          (except-in racket/base for for* with-handlers lambda λ define
                     let let* letrec letrec-values let-values
                     let/cc let/ec do case-lambda struct define-struct
                     default-continuation-prompt-tag
                     for/list for/vector for/hash for/hasheq for/hasheqv
                     for/and for/or for/sum for/product for/lists
                     for/first for/last for/fold for*/list for*/lists
                     for*/vector for*/hash for*/hasheq for*/hasheqv for*/and
                     for*/or for*/sum for*/product for*/first for*/last
                     for*/fold)
          "../base-env/prims.rkt"
          (prefix-in c: (combine-in racket/contract/region racket/contract/base)))
         "../base-env/extra-procs.rkt" (except-in "../base-env/prims.rkt" with-handlers λ lambda define)
         "../tc-setup.rkt"
         "../standard-inits.rkt"
         syntax/parse racket/match
         unstable/sequence  "../base-env/base-types-extra.rkt"
         (path-up "env/type-name-env.rkt"
                  "env/type-alias-env.rkt"
                  "private/parse-type.rkt"
                  "private/type-contract.rkt"
                  "typecheck/typechecker.rkt"
                  "env/type-env-structs.rkt"
                  "env/global-env.rkt"
                  "env/tvar-env.rkt"
                  "utils/tc-utils.rkt"
                  "utils/disarm.rkt"
                  "utils/arm.rkt"
                  "types/utils.rkt"))

(provide wt-core)

(define (with-type-helper stx body fvids fvtys exids extys resty expr? ctx)
  (define old-context (unbox typed-context?))
  (unless (not old-context)
    (tc-error/stx stx "with-type cannot be used in a typed module."))
  (define (no-contract t [stx stx])
    (type->contract-fail t stx))
  (set-box! typed-context? #t)
  (do-standard-inits)
  (define fv-types (for/list ([t (in-syntax fvtys)])
                     (parse-type t)))
  (define fv-cnts (for/list ([t (in-list fv-types)]
                             [stx (in-syntax fvtys)])
                    (type->contract t #:typed-side #f (no-contract t))))
  (define ex-types (for/list ([t (in-syntax extys)])
                     (parse-type t)))
  (define ex-cnts (for/list ([t (in-list ex-types)]
                             [stx (in-syntax extys)])
                    (type->contract t #:typed-side #t (no-contract t))))
  (define region-tc-result
    (and expr? (parse-tc-results resty)))
  (define region-cnts
    (if region-tc-result
        (match region-tc-result
          [(tc-result1: t)
           (list (type->contract t #:typed-side #t (no-contract t #'region-ty-stx)))]
          [(tc-results: ts)
           (for/list ([t (in-list ts)])
             (type->contract
              t #:typed-side #t
              (no-contract t #'region-ty-stx)))])
        null))
  (for ([i (in-syntax fvids)]
        [ty (in-list fv-types)])
    (register-type i ty))
  (define expanded-body
    (disarm*
      (if expr?
          (with-syntax ([body body])
            (local-expand #'(let () . body) ctx null))
          (with-syntax ([(body ...) body]
                        [(id ...) exids]
                        [(ty ...) extys])
            (local-expand #'(let () (begin (: id ty) ... body ... (values id ...))) ctx null)))))
  (parameterize (;; do we report multiple errors
                 [delay-errors? #t]
                 ;; this parameter is just for printing types
                 ;; this is a parameter to avoid dependency issues
                 [current-type-names
                  (lazy
                    (append 
                     (type-name-env-map (lambda (id ty)
                                          (cons (syntax-e id) ty)))
                     (type-alias-env-map (lambda (id ty)
                                           (cons (syntax-e id) ty)))))]
                 ;; reinitialize disappeared uses
                 [disappeared-use-todo      null]
                 [disappeared-bindings-todo null]
                 ;; for error reporting
                 [orig-module-stx stx]
                 [expanded-module-stx expanded-body])
    (tc-expr/check expanded-body (if expr? region-tc-result (ret ex-types))))
  (report-all-errors)
  (set-box! typed-context? old-context)
  ;; then clear the new entries from the env ht
  (for ([i (in-syntax fvids)])
    (unregister-type i))
  (with-syntax ([(fv.id ...) fvids]
                [(cnt ...) fv-cnts]
                [(ex-id ...) exids]
                [(ex-cnt ...) ex-cnts]
                [(region-cnt ...) region-cnts]
                [(body) (maybe-optimize #`(#,expanded-body))]
                [check-syntax-help (syntax-property
                                    (syntax-property
                                     #'(void)
                                     'disappeared-binding (disappeared-bindings-todo))
                                    'disappeared-use (disappeared-use-todo))])
    (arm
      (if expr?
          (quasisyntax/loc stx
            (begin check-syntax-help
                   (c:with-contract typed-region
                                    #:results (region-cnt ...)
                                    #:freevars ([fv.id cnt] ...)
                                    body)))
          (syntax/loc stx
            (begin
              (define-values () (begin check-syntax-help (values)))
              (c:with-contract typed-region
                               ([ex-id ex-cnt] ...)
                               (define-values (ex-id ...) body))))))))

(define (wt-core stx)
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

