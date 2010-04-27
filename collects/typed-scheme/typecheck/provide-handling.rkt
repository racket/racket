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
         racket/contract/private/provide unstable/list
         unstable/debug
         unstable/syntax scheme/struct-info scheme/match
         "def-binding.ss" syntax/parse)

(require (for-template scheme/base
                       scheme/contract))

(provide remove-provides provide? generate-prov
         get-alternate)

(define (provide? form)    
  (syntax-parse form 
    #:literals (#%provide)
    [(#%provide . rest) form]
    [_ #f]))

(define (remove-provides forms)
  (filter (lambda (e) (not (provide? e))) (syntax->list forms)))

(define (renamer id #:alt [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))

;; maps ids defined in this module to an identifier which is the possibly-contracted version of the key
(define mapping (make-free-identifier-mapping))

(define (mem? i vd)
  (cond [(s:member i vd (lambda (i j) (free-identifier=? i (binding-name j)))) => car]
        [else #f]))

;; generate-contract-defs : listof[def-binding] listof[def-binding] id -> syntax -> syntax
;; val-defs: define-values in this module
;; stx-defs: define-syntaxes in this module
;; pos-blame-id: a #%variable-reference for the module

;; internal-id : the id being provided 
;; if `internal-id' is defined in this module, we will produce a (begin def ... provide) block 
;;    and a name to provide instead of internal-id

;; anything already recorded in the mapping is given an empty (begin) and the already-recorded id
;; otherwise, we will map internal-id to the fresh id in `mapping'
(define ((generate-prov stx-defs val-defs pos-blame-id) form)
  ;; mk : id [id] -> (values syntax id)
  (define (mk internal-id [new-id (generate-temporary internal-id)])
    (cond
      ;; if it's already done, do nothing
      [(free-identifier-mapping-get mapping internal-id
                                    ;; if it wasn't there, put it in, and skip this case
                                    (lambda ()
                                      (free-identifier-mapping-put! mapping internal-id new-id)
                                      #f))
       => (lambda (mapped-id)
            (values #'(begin) mapped-id))]
      [(mem? internal-id val-defs)
       =>
       (lambda (b)
         (values
          (with-syntax ([id internal-id])
            (cond [(type->contract (def-binding-ty b) (lambda () #f) #:out #t)
                   =>
                   (lambda (cnt)                                    
                     (with-syntax ([(cnt-id) (generate-temporaries #'(id))]
                                   [export-id new-id]
                                   [module-source pos-blame-id]
                                   [the-contract (generate-temporary 'generated-contract)])
                       #`(begin 
                           (define the-contract #,cnt)
                           (define-syntax cnt-id
                             (make-provide/contract-transformer
                              (quote-syntax the-contract)
                              (quote-syntax id)
                              (quote-syntax out-id)
                              (quote-syntax module-source)))
                           (define-syntax export-id
                             (if (unbox typed-context?)
                                 (renamer #'id #:alt #'cnt-id)
                                 (renamer #'cnt-id))))))]
                  [else 
                   (with-syntax ([(error-id) (generate-temporaries #'(id))]
                                 [export-id new-id])
                     #`(begin        
                         (define-syntax error-id
                           (lambda (stx) (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id))))
                         (define-syntax export-id
                           (if (unbox typed-context?)
                               (renamer #'id #:alt #'error-id)
                               (renamer #'error-id)))))]))
          new-id))]
      [(mem? internal-id stx-defs) 
       =>
       (lambda (b)
         (define (mk-untyped-syntax defn-id internal-id)
           (match b
             [(struct def-struct-stx-binding (_ (? struct-info? si)))
               (match-let ([(list type-desc constr pred (list accs ...) muts super) (extract-struct-info si)])
                 (let-values ([(defns new-ids) (map/values 2 (lambda (e) (if (identifier? e)
                                                                             (mk e)
                                                                             (values #'(begin) e)))
                                                           (list* type-desc constr pred super accs))])
                   (with-syntax ([(type-desc* constr* pred* super* accs* ...) (for/list ([i new-ids])
                                                                                (if (identifier? i)
                                                                                    #`(syntax #,i)
                                                                                    i))])
                   #`(begin
                       #,@defns
                       (define-syntax #,defn-id
                         (list type-desc* constr* pred* (list accs* ...) (list #,@(map (lambda x #'#f) accs)) super*))))))]
             [_
               #`(define-syntax #,defn-id
                   (lambda (stx)
                     (tc-error/stx stx "Macro ~a from typed module used in untyped code" (syntax-e #'#,internal-id))))]))
         (with-syntax* ([id internal-id]
                        [export-id new-id]
                        [(untyped-id) (generate-temporaries #'(id))])
           (values
            #`(begin  
                #,(mk-untyped-syntax #'untyped-id internal-id)
                (define-syntax export-id
                  (if (unbox typed-context?)
                      (begin                           
                        (add-alias #'export-id #'id)
                        (renamer #'id #:alt #'untyped-id))
                      (renamer #'untyped-id))))
            new-id)))]
      ;; otherwise, not defined in this module, not our problem
      [else (values #'(begin) internal-id)]))
  ;; do-one : id [id] -> syntax
  (define (do-one internal-id [external-id internal-id])
    (define-values (defs id) (mk internal-id))
    #`(begin #,defs (provide (rename-out [#,id #,external-id]))))
  (syntax-parse form #:literals (#%provide)
      [(#%provide form ...)
       (for/list ([f (syntax->list #'(form ...))])
         (parameterize ([current-orig-stx f])
           (syntax-parse f
             [i:id 
              (do-one #'i)]
             [((~datum rename) in out)
              (do-one #'in #'out)]
             [((~datum protect) . _)
              (tc-error "provide: protect not supported by Typed Scheme")]
             [_ (int-err "unknown provide form")])))]
      [_ (int-err "non-provide form! ~a" (syntax->datum form))]))
