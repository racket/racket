#lang scheme/base

(require "../utils/utils.rkt"
	 (only-in srfi/1/list s:member)
         syntax/kerncase syntax/boundmap
         (env type-name-env type-alias-env)
         mzlib/trace
         (only-in (private type-contract) type->contract)
         (private typed-renaming)
         (rep type-rep)
	 (utils tc-utils)
         (for-syntax syntax/parse racket/base)
         racket/contract/private/provide unstable/list
          syntax/id-table racket/dict
         racket/syntax scheme/struct-info racket/match
         "def-binding.rkt" syntax/parse
         (for-template scheme/base "def-export.rkt" scheme/contract))

(provide remove-provides provide? generate-prov get-alternate)

(define (provide? form)
  (syntax-parse form
    #:literals (#%provide)
    [(#%provide . rest) form]
    [_ #f]))

(define (remove-provides forms)
  (filter (lambda (e) (not (provide? e))) (syntax->list forms)))

(define (mem? i vd)
  (cond [(s:member i vd (lambda (i j) (free-identifier=? i (binding-name j)))) => car]
        [else #f]))

;; generate-contract-defs : dict[id -> def-binding] dict[id -> id] id -> syntax
;; defs: defines in this module
;; provs: provides in this module
;; pos-blame-id: a #%variable-reference for the module

;; internal-id : the id being provided
;; if `internal-id' is defined in this module, we will produce a (begin def ... provide) block
;;    and a name to provide instead of internal-id

;; anything already recorded in the mapping is given an empty (begin) and the already-recorded id
;; otherwise, we will map internal-id to the fresh id in `mapping'
(define (generate-prov defs provs pos-blame-id)
  ;; maps ids defined in this module to an identifier which is the possibly-contracted version of the key
  (define mapping (make-free-id-table))

  ;; mk : id [id] -> (values syntax id)
  (define (mk internal-id [new-id (generate-temporary internal-id)])
    (define (mk-untyped-syntax b defn-id internal-id)
      (match b
        [(def-struct-stx-binding _ (? struct-info? si))
         (define type-is-constructor? #t) ;Conservative estimate (provide/contract does the same)
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
                     (let ((info (list type-desc* constr* pred* (list accs* ...) (list #,@(map (lambda x #'#f) accs)) super*)))
                      #,(if type-is-constructor?
                            #'(make-struct-info-self-ctor constr* info)
                            #'info)))))))]
        [_
         #`(define-syntax #,defn-id
             (lambda (stx)
               (tc-error/stx stx "Macro ~a from typed module used in untyped code" (syntax-e #'#,internal-id))))]))
    (cond
      ;; if it's already done, do nothing
      [(dict-ref mapping internal-id
                 ;; if it wasn't there, put it in, and skip this case
                 (λ () (dict-set! mapping internal-id new-id) #f))
       => (λ (mapped-id) (values #'(begin) mapped-id))]
      [(dict-ref defs internal-id #f)
       =>
       (match-lambda
         [(def-binding _ (app (λ (ty) (type->contract ty (λ () #f) #:out #t)) (? values cnt)))
          (values
           (with-syntax* ([id internal-id]
                          [cnt-id (generate-temporary #'id)]
                          [export-id new-id]
                          [module-source pos-blame-id]
                          [the-contract (generate-temporary 'generated-contract)])
             #`(begin
                 (define the-contract #,cnt)
                 (define-syntax cnt-id
                   (make-provide/contract-transformer
                    (quote-syntax the-contract)
                    (quote-syntax id)
                    (quote-syntax export-id)
                    (quote-syntax module-source)))
                 (def-export export-id id cnt-id)))
           new-id)]
         [(def-binding id ty)
          (values
           (with-syntax* ([id internal-id]
                          [error-id (generate-temporary #'id)]
                          [export-id new-id])
               #'(begin
                   (define-syntax (error-id stx)
                     (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id)))
                   (def-export export-id id error-id)))
           new-id)]
         [(and b (def-stx-binding _))
          (with-syntax* ([id internal-id]
                         [export-id new-id]
                         [untyped-id (generate-temporary #'id)]
                         [def (mk-untyped-syntax b #'untyped-id internal-id)])
            (values
             #`(begin def (def-export export-id id untyped-id #:alias))
             new-id))])]
      ;; otherwise, not defined in this module, not our problem
      [else (values #'(begin) internal-id)]))
  ;; do-one : id id -> syntax
  (for/list ([(internal-id external-id) (in-dict provs)])
    (define-values (defs id) (mk internal-id))
    #`(begin #,defs (provide (rename-out [#,id #,external-id])))))
