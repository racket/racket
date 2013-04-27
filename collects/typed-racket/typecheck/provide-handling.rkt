#lang racket/base

(require "../utils/utils.rkt"
         (only-in srfi/1/list s:member)
         syntax/kerncase syntax/boundmap
         (env type-name-env type-alias-env)
         (only-in (private type-contract) type->contract)
         "renamer.rkt"
         (rep type-rep)
         (utils tc-utils)
         (for-syntax syntax/parse racket/base)
         racket/contract/private/provide unstable/list
         syntax/id-table syntax/location racket/dict
         racket/syntax racket/struct-info racket/match
         "def-binding.rkt" syntax/parse
         (for-template racket/base "def-export.rkt" racket/contract))

(provide remove-provides provide? generate-prov get-alternate)

(define (provide? form)
  (syntax-parse form
    #:literals (#%provide)
    [(#%provide . rest) form]
    [_ #f]))

(define (remove-provides forms)
  (filter (lambda (e) (not (provide? e))) (syntax->list forms)))

(define (freshen-id id)
  ((make-syntax-introducer) id))

;; generate-prov : dict[id -> def-binding] dict[id -> list[id]] id
;;                 -> (values listof[syntax] listof[listof[list[id id]]])
;; defs: defines in this module
;; provs: provides in this module
;; pos-blame-id: a #%variable-reference for the module
(define (generate-prov defs provs pos-blame-id)
  ;; maps ids defined in this module to an identifier which is the possibly-contracted version of the key
  (define mapping (make-free-id-table))

  ;; triple? in the signatures corresponds to three values:
  ;; (values syntax? identfier? (listof (list/c identifier? identifier?))
  ;; First return value is a syntax object of definitions
  ;; Second is the id to export
  ;; Third is a list of two element lists representing type aliases

  ;; mk : id -> triple?
  ;;
  ;; internal-id : the id being provided
  ;; if `internal-id' is defined in this module, we will produce a (begin def ... provide) block
  ;; and a name to provide instead of internal-id.
  ;;
  ;; Anything already recorded in the mapping is given an empty (begin) and the already-recorded id
  ;; otherwise, we will map internal-id to the fresh id in `mapping'
  (define (mk internal-id)
    (define new-id (freshen-id internal-id))
    (cond
      ;; if it's already done, do nothing
      [(dict-ref mapping internal-id
                 ;; if it wasn't there, put it in, and skip this case
                 (λ () (dict-set! mapping internal-id new-id) #f))
       => (λ (mapped-id) (values #'(begin) mapped-id null))]
      [(dict-ref defs internal-id #f)
       =>
       (match-lambda
         [(def-binding _ ty)
          (mk-value-triple internal-id new-id ty)]
         [(def-struct-stx-binding _ (? struct-info? si) constructor-type)
          (mk-struct-syntax-triple internal-id new-id si constructor-type)]
         [(def-stx-binding _)
          (mk-syntax-triple internal-id new-id)])]
      ;; otherwise, not defined in this module, not our problem
      [else (values #'(begin) internal-id null)]))

  ;; mk-struct-syntax-triple : identifier? identifier? struct-info? Type/c? -> triple?
  (define (mk-struct-syntax-triple internal-id new-id si constructor-type)
    (define type-is-constructor? #t) ;Conservative estimate (provide/contract does the same)
    (match-define (list type-desc constr pred (list accs ...) muts super) (extract-struct-info si))
    (define-values (defns new-ids aliases)
      (map/values 3
                  (lambda (id)
                    (if (identifier? id)
                        (mk id)
                        (values #'(begin) #f null)))
                  (list* type-desc constr pred super accs)))
    (define/with-syntax inner-constr* (freshen-id constr))

    (define/with-syntax (type-desc* constr* pred* super* accs* ...)
      (for/list ([i new-ids])
         (if (identifier? i) #`(syntax #,i) i)))


    (with-syntax* ([id internal-id]
                   [export-id new-id]
                   [untyped-id (freshen-id #'id)])
      (values
        #`(begin
            #,(mk-protected-value-definitions constr #'inner-constr* #'constr* constructor-type)
            #,@defns
            (define-syntax untyped-id
              (let ((info (list type-desc* constr* pred* (list accs* ...)
                                (list #,@(map (lambda x #'#f) accs)) super*)))
                #,(if type-is-constructor?
                      #'(make-struct-info-self-ctor #'inner-constr* info)
                      #'info)))
            (def-export export-id id untyped-id))
        new-id
        (cons (list #'export-id internal-id) (apply append aliases)))))


  ;; mk-syntax-triple : identifier? identifier? -> triple?
  (define (mk-syntax-triple internal-id new-id)
    (with-syntax* ([id internal-id]
                   [export-id new-id]
                   [untyped-id (freshen-id #'id)])
      (define/with-syntax def
        #`(define-syntax untyped-id
            (lambda (stx)
              (tc-error/stx stx "Macro ~a from typed module used in untyped code" 'untyped-id))))
      (values
       #`(begin def (def-export export-id id untyped-id))
       new-id
       (list (list #'export-id #'id)))))

  ;; mk-value-triple : identifier? identifier? Type/c? -> triple?
  (define (mk-value-triple internal-id new-id type)
    (define untyped-id (freshen-id internal-id))
    (values
      #`(begin
          #,(mk-protected-value-definitions internal-id new-id untyped-id type)
          (def-export #,new-id #,internal-id #,untyped-id))
      new-id
      null))

  (define (mk-protected-value-definitions internal-id new-id untyped-id type)
    (define cnt (type->contract type (λ () #f)))
    (with-syntax ([id internal-id]
                  [export-id new-id]
                  [untyped-id untyped-id])
      (if cnt
          (with-syntax* ([module-source pos-blame-id]
                         [the-contract (generate-temporary 'generated-contract)])
            #`(begin
                (define the-contract #,cnt)
                (define-syntax untyped-id
                  (make-provide/contract-transformer
                   (quote-syntax the-contract)
                   (datum->syntax ; preserve source location in expanded code
                    (quote-syntax id)
                    (syntax->datum (quote-syntax id))
                    (list (quote-source-file id)
                          (quote-line-number id)
                          (quote-column-number id)
                          (quote-character-position id)
                          (quote-character-span id))
                    (quote-syntax id))
                   (quote-syntax export-id)
                   (quote-syntax module-source)))))
          #'(define-syntax (untyped-id stx)
              (tc-error/stx stx "The type of ~a cannot be converted to a contract" 'id)))))


  ;; Build the final provide with auxilliary definitions
  (for/lists (l l*) ([(internal-id external-ids) (in-dict provs)])
    (define-values (defs id alias) (mk internal-id))
    (define provide-forms
      (for/list ([external-id (in-list external-ids)])
        #`(rename-out [#,id #,external-id])))
    (values #`(begin #,defs (provide #,@provide-forms))
            alias)))
