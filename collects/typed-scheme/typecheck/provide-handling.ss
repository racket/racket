#lang scheme/base

(require "../utils/utils.ss" 
	 (only-in srfi/1/list s:member)
         syntax/kerncase syntax/boundmap
         (env type-name-env type-alias-env)
         mzlib/trace
         (only-in (private type-contract) type->contract)
         (private typed-renaming)
         (rep type-rep)
	 (utils tc-utils)
         scheme/contract/private/provide
         unstable/syntax
         "def-binding.ss")

(require (for-template scheme/base
                       scheme/contract))

(provide remove-provides provide? generate-prov
         get-alternate)

(define (provide? form)    
  (kernel-syntax-case form #f
    [(#%provide . rest) form]
    [_ #f]))


(define (remove-provides forms)
  (filter (lambda (e) (not (provide? e))) (syntax->list forms)))


(define (renamer id #:alt [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))

(define (generate-prov stx-defs val-defs pos-blame-id)
  (define mapping (make-free-identifier-mapping))
  (lambda (form)
    (define (mem? i vd)
      (cond [(s:member i vd (lambda (i j) (free-identifier=? i (binding-name j)))) => car]
            [else #f]))
    (define (lookup-id i vd)
      (def-binding-ty (mem? i vd)))
    (define (mk internal-id external-id)
      (cond
        ;; if it's already done, do nothing
        [(free-identifier-mapping-get mapping internal-id
                                      ;; if it wasn't there, put it in, and skip this case
                                      (lambda ()
                                        (free-identifier-mapping-put! mapping internal-id #t)
                                        #f))
         #'(begin)]
        [(mem? internal-id val-defs)
         =>
         (lambda (b)
           (with-syntax ([id internal-id]
                         [out-id external-id])
             (cond [(type->contract (def-binding-ty b) (lambda () #f) #:out #t)
                    =>
                    (lambda (cnt)                                    
                      (with-syntax ([(export-id cnt-id) (generate-temporaries #'(id id))]
                                    [module-source pos-blame-id]
                                    [the-contract (generate-temporary 'generated-contract)])
                        #`(begin 
                            (define the-contract #,cnt)
                            (define-syntax cnt-id
                              (make-provide/contract-transformer
                               (quote-syntax the-contract)
                               (quote-syntax id)
                               (quote-syntax module-source)))
                            (define-syntax export-id
                              (if (unbox typed-context?)
                                  (renamer #'id #:alt #'cnt-id)
                                  (renamer #'cnt-id)))
                            (#%provide (rename export-id out-id)))))]
                   [else 
                    (with-syntax ([(export-id error-id) (generate-temporaries #'(id id))])
                      #`(begin        
                          (define-syntax error-id
                            (lambda (stx) (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id))))
                          (define-syntax export-id
                            (if (unbox typed-context?)
                                (renamer #'id #:alt #'error-id)
                                (renamer #'error-id)))
                          (provide (rename-out [export-id out-id]))))])))]
        [(mem? internal-id stx-defs) 
         =>
         (lambda (b)
           (with-syntax ([id internal-id]
                         [out-id external-id])
             (with-syntax ([(export-id error-id) (generate-temporaries #'(id id))])
               #`(begin  
                   (define-syntax error-id
                     (lambda (stx)
                       (tc-error/stx stx "Macro ~a from typed module used in untyped code" (syntax-e #'out-id))))
                   (define-syntax export-id
                     (if (unbox typed-context?)
                         (begin                           
                           (add-alias #'export-id #'id)
                           (renamer #'id #:alt #'error-id))
                         (renamer #'error-id)))
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
      [_ (int-err "non-provide form! ~a" (syntax->datum form))])))
