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

(define (mem? i vd)
  (cond [(s:member i vd (lambda (i j) (free-identifier=? i (binding-name j)))) => car]
        [else #f]))

(define new-id-introducer (make-syntax-introducer))
(define cnt-id-introducer (make-syntax-introducer))
(define error-id-introducer (make-syntax-introducer))
(define untyped-id-introducer (make-syntax-introducer))

;; generate-prov : dict[id -> def-binding] dict[id -> list[id]] id ->
;;                 (values syntax list[(list/c identifier? identifier?)])
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


  (define (mk-struct-stx-binding defn-id internal-id si constr-type)
    (define type-is-constructor? #t) ;Conservative estimate (provide/contract does the same)
    (match-define (list type-desc constr pred (list accs ...) muts super) (extract-struct-info si))
    (define-values (defns new-ids aliases)
      (map/values 3
                  (lambda (e) (if (identifier? e)
                                  (mk e)
                                  (values #'(begin) #f null)))
                  (list* type-desc pred super accs)))
    (define-values (constr-defn constr-new-id constr-aliases)
      (cond
       ((not (identifier? constr))
        (values #'(begin) #f null))
       ((free-identifier=? constr internal-id)
        (mk-value-binding constr (new-id-introducer constr) constr-type))
       (else
        (mk constr))))

    (define/with-syntax (constr* type-desc* pred* super* accs* ...)
      (for/list ([i (cons constr-new-id new-ids)])
         (if (identifier? i) #`(syntax #,i) i)))

    (values
     #`(begin
         #,@(cons constr-defn defns)
         (define-syntax #,defn-id
           (let ((info (list type-desc* constr* pred* (list accs* ...) (list #,@(map (lambda x #'#f) accs)) super*)))
             #,(if type-is-constructor?
                   #'(make-struct-info-self-ctor constr* info)
                   #'info))))
     (apply append (cons constr-aliases aliases))))

  (define (mk-untyped-syntax defn-id internal-id)
    (values
     #`(define-syntax #,defn-id
         (lambda (stx)
           (tc-error/stx stx "Macro ~a from typed module used in untyped code" (syntax-e #'#,internal-id))))
     null))

  (define (mk-value-binding internal-id new-id ty)
    (define contract (type->contract ty (λ () #f) #:out #t))
    (if contract
        (mk-protected-binding internal-id new-id contract)
        (mk-error-binding internal-id new-id)))


  (define (mk-protected-binding internal-id new-id cnt)
    (values
      (with-syntax* ([id internal-id]
                     [cnt-id (cnt-id-introducer #'id)]
                     [export-id new-id]
                     [module-source pos-blame-id]
                     [the-contract (generate-temporary 'generated-contract)])
        #`(begin
            (define the-contract #,cnt)
            (define-syntax cnt-id
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
               (quote-syntax module-source)))
            (def-export export-id id cnt-id)))
      new-id
      null))
  (define (mk-error-binding internal-id new-id)
    (values
      (with-syntax* ([id internal-id]
                     [error-id (error-id-introducer #'id)]
                     [export-id new-id])
          #'(begin
              (define-syntax (error-id stx)
                (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id)))
              (def-export export-id id error-id)))
      new-id
      null))

  ;; mk : id [id] -> (values syntax id aliases)
  ;; First return value is a syntax object of definitions
  ;; Second is the id to export
  ;; Third is a list of two element list representing type aliases
  (define (mk internal-id [new-id (untyped-id-introducer internal-id)])
    (cond
      ;; if it's already done, do nothing
      [(dict-ref mapping internal-id
                 ;; if it wasn't there, put it in, and skip this case
                 (λ () (dict-set! mapping internal-id new-id) #f))
       => (λ (mapped-id) (values #'(begin) mapped-id null))]
      [(dict-ref defs internal-id #f)
       =>
       (match-lambda
         [(def-binding _ (app (λ (ty) (type->contract ty (λ () #f))) (? values cnt)))
          (values
           (with-syntax* ([id internal-id]
                          [cnt-id (cnt-id-introducer #'id)]
                          [export-id new-id]
                          [module-source pos-blame-id]
                          [the-contract (generate-temporary 'generated-contract)])
             #`(begin
                 (define the-contract #,cnt)
                 (define-syntax cnt-id
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
                    (quote-syntax module-source)))
                 (def-export export-id id cnt-id)))
           new-id
           null)]
         [(def-binding id ty)
          (values
           (with-syntax* ([id internal-id]
                          [error-id (error-id-introducer #'id)]
                          [export-id new-id])
               #'(begin
                   (define-syntax (error-id stx)
                     (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id)))
                   (def-export export-id id error-id)))
           new-id
           null)]
         [(def-struct-stx-binding _ si constr-type)
          (define-values (defs aliases)
            (mk-struct-stx-binding new-id internal-id si constr-type))
          (values defs new-id aliases)]
         [(and b (def-stx-binding _))
          (with-syntax* ([id internal-id]
                         [export-id new-id]
                         [untyped-id (untyped-id-introducer #'id)])
            (define-values (d aliases)
              (mk-untyped-syntax #'untyped-id internal-id))
            (define/with-syntax def d)
            (values
             #`(begin def (def-export export-id id untyped-id))
             new-id
             (cons (list #'export-id #'id) aliases)))])]
      ;; otherwise, not defined in this module, not our problem
      [else (values #'(begin) internal-id null)]))
  ;; Build the final provide with auxilliary definitions
  (for/lists (l l*) ([(internal-id external-ids) (in-dict provs)])
    (define-values (defs id alias) (mk internal-id))
    (define provide-forms
      (for/list ([external-id (in-list external-ids)])
        #`(rename-out [#,id #,external-id])))
    (values #`(begin #,defs (provide #,@provide-forms))
            alias)))
