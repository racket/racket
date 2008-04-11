#lang scheme/base

(require (only-in srfi/1/list s:member)
         syntax/kerncase syntax/struct syntax/stx
         mzlib/trace
         "type-contract.ss"
         "signatures.ss"
         "tc-structs.ss"
         "utils.ss" ;; doesn't need tests
         "type-rep.ss" ;; doesn't need tests
         "unify.ss" ;; needs tests
         "infer.ss"
         "type-effect-convenience.ss" ;; maybe needs tests
         "union.ss"
         "subtype.ss" ;; has tests
         "internal-forms.ss" ;; doesn't need tests
         "planet-requires.ss" ;; doesn't need tests
         "type-env.ss" ;; maybe needs tests
         "parse-type.ss" ;; has tests
         "tc-utils.ss" ;; doesn't need tests
         "type-environments.ss" ;; doesn't need tests
         "lexical-env.ss" ;; maybe needs tests
         "type-annotation.ss" ;; has tests
         "type-name-env.ss" ;; maybe needs tests
         "init-envs.ss"
         "effect-rep.ss"
         "mutated-vars.ss"
         "def-binding.ss")

(require (for-template scheme/base
                       scheme/contract))

(provide remove-provides provide? generate-prov)

(define (provide? form)    
  (kernel-syntax-case form #f
    [(#%provide . rest) form]
    [_ #f]))


(define (remove-provides forms)
  (filter (lambda (e) (not (provide? e))) (syntax->list forms)))

(define ((generate-prov stx-defs val-defs) form)
  (define (mem? i vd)
    (cond [(s:member i vd (lambda (i j) (free-identifier=? i (binding-name j)))) => car]
          [else #f]))
  (define (lookup-id i vd)
    (def-binding-ty (mem? i vd)))
  (define (mk internal-id external-id)
    (cond
      [(mem? internal-id val-defs)
       =>
       (lambda (b)
         (with-syntax ([id internal-id]
                       [out-id external-id])
           (cond [(type->contract (def-binding-ty b) (lambda () #f)) 
                  =>
                  (lambda (cnt)                                    
                    (with-syntax ([(export-id cnt-id) (generate-temporaries #'(id id))])
                      #`(begin 
                          (define/contract cnt-id #,cnt id)
                          (define-syntax export-id
                            (if (unbox typed-context?)
                                (make-rename-transformer #'id)
                                (make-rename-transformer #'cnt-id)))
                          (#%provide (rename export-id out-id)))))]
                 [else 
                  (with-syntax ([(export-id) (generate-temporaries #'(id))])
                    #`(begin                             
                        (define-syntax export-id
                          (if (unbox typed-context?)
                              (make-rename-transformer #'id)
                              (lambda (stx) (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id)))))
                        (provide (rename-out [export-id out-id]))))])))]
      [(mem? internal-id stx-defs) 
       =>
       (lambda (b)
         (with-syntax ([id internal-id]
                       [out-id external-id])
           (with-syntax ([(export-id cnt-id) (generate-temporaries #'(id id))])
             #`(begin                    
                 (define-syntax export-id
                   (if (unbox typed-context?)
                       (make-rename-transformer #'id)
                       (lambda (stx)
                         (tc-error/stx stx "Macro ~a from typed module used in untyped code" (syntax-e #'out-id)))))
                 (provide (rename-out [export-id out-id]))))))]
      [(eq? (syntax-e internal-id) (syntax-e external-id))
       #`(provide #,internal-id)]
      [else #`(provide (rename-out [#,internal-id #,external-id]))]))
  (kernel-syntax-case form #f
    [(#%provide form ...)
     (map 
      (lambda (f)
        (parameterize ([current-orig-stx f])
          (syntax-case* f (struct rename all-defined protect all-defined-except all-from all-from-except) 
            (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
            [id 
             (identifier? #'id)
             (mk #'id #'id)]
            [(rename in out)
             (mk #'in #'out)]
            [(protect . _)
             (tc-error "provide: protect not supported by Typed Scheme")]
            [_ (int-err "unknown provide form")])))
      (syntax->list #'(form ...)))]
    [_ (int-err "non-provide form! ~a" (syntax->datum form))]))
