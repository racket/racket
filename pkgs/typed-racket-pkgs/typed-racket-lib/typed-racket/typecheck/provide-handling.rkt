#lang racket/base

(require "../utils/utils.rkt"
         unstable/list unstable/sequence syntax/id-table racket/dict racket/syntax
         racket/struct-info racket/match syntax/parse syntax/location
         (only-in (private type-contract) type->contract)
         (typecheck renamer def-binding)
         (utils tc-utils)
         (for-syntax racket/base)
         (for-template racket/base "def-export.rkt" racket/contract))

(provide remove-provides provide? generate-prov get-alternate)

(define (provide? form)
  (syntax-parse form
    #:literals (#%provide)
    [(#%provide . rest) form]
    [_ #f]))

(define (remove-provides forms)
  (for/list ([e (in-syntax forms)]
             #:unless (provide? e))
    e))

(define (freshen-id id)
  ((make-syntax-introducer) id))

;; generate-prov : dict[id -> def-binding] dict[id -> list[id]] id
;;                 -> (values listof[syntax] listof[listof[list[id id]]])
;; defs: defines in this module
;; provs: provides in this module
;; pos-blame-id: a #%variable-reference for the module

;; The first returned value is a syntax object of definitions that defines the
;; contracted versions of the provided identifiers, and the corresponding
;; provides.
;;
;; The second value is a list of two element lists, which are type name aliases.
(define (generate-prov defs provs pos-blame-id)
  ;; maps ids defined in this module to an identifier which is the possibly-contracted version of the key
  (define mapping (make-free-id-table))

  ;; triple/c in the signatures corresponds to three values:
  ;; (values syntax? identfier? (listof (list/c identifier? identifier?))
  ;; First return value is a syntax object of definitions
  ;; Second is the id to export
  ;; Third is a list of two element lists representing type aliases

  ;; mk : id -> triple/c
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
         [(def-struct-stx-binding _ (? struct-info? si) constr-type)
          (mk-struct-syntax-triple internal-id new-id si constr-type)]
         [(def-stx-binding _)
          (mk-syntax-triple internal-id new-id)])]
      ;; otherwise, not defined in this module, not our problem
      [else (values #'(begin) internal-id null)]))

  ;; mk-struct-syntax-triple : identifier? identifier? struct-info? Type/c -> triple/c
  (define (mk-struct-syntax-triple internal-id new-id si constr-type)
    (define type-is-constructor? #t) ;Conservative estimate (provide/contract does the same)
    (match-define (list type-desc constr pred (list accs ...) muts super) (extract-struct-info si))
    (define-values (defns new-ids aliases)
      (map/values 3
                  (lambda (e) (if (identifier? e)
                                  (mk e)
                                  (values #'(begin) e null)))
                  (list* type-desc pred super accs)))
    (define-values (constr-defn constr-new-id constr-aliases)
      (cond
       [(not (identifier? constr))
        (values #'(begin) #f null)]
       [(free-identifier=? constr internal-id)
        (mk-value-triple constr (generate-temporary constr) constr-type)]
       [else
        (mk constr)]))

    (define/with-syntax (constr* type-desc* pred* super* accs* ...)
      (for/list ([i (in-list (cons constr-new-id new-ids))])
        (and (identifier? i) #`(quote-syntax #,i))))

    (with-syntax* ([id internal-id]
                   [export-id new-id]
                   [protected-id (freshen-id #'id)])
      (values
        #`(begin
            #,constr-defn
            #,@defns
            (define-syntax protected-id
              (let ((info (list type-desc* (syntax export-id) pred* (list accs* ...)
                                (list #,@(map (lambda (x) #'#f) accs)) super*)))
                #,(if type-is-constructor?
                      #'(make-struct-info-self-ctor constr* info)
                      #'info)))
            (def-export export-id protected-id protected-id))
        new-id
        (cons (list #'export-id internal-id)
              (apply append constr-aliases aliases)))))


  ;; mk-syntax-triple : identifier? identifier? -> triple/c
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

  ;; mk-value-triple : identifier? identifier? (or/c syntax? #f) -> triple/c
  (define (mk-value-triple internal-id new-id ty)
    (define contract (type->contract ty (λ (#:reason [reason #f]) #f)))

    (with-syntax* ([id internal-id]
                   [untyped-id (freshen-id #'id)]
                   [export-id new-id])
      (define/with-syntax definitions
        (if contract
            (with-syntax* ([module-source pos-blame-id]
                           [the-contract (generate-temporary 'generated-contract)])
              #`(begin
                  (define the-contract #,contract)
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
                (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id)))))
      (values
        #'(begin
            definitions
            (def-export export-id id untyped-id))
        new-id
        null)))


  ;; Build the final provide with auxilliary definitions
  (for/lists (l l*) ([(internal-id external-ids) (in-dict provs)])
    (define-values (defs id alias) (mk internal-id))
    (define provide-forms
      (for/list ([external-id (in-list external-ids)])
        #`(rename-out [#,id #,external-id])))
    (values #`(begin #,defs (provide #,@provide-forms))
            alias)))
