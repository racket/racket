#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         (rename-in "private/generic.rkt"
                    [define-generics define-generics/pre])
         (for-syntax racket/base racket/local racket/syntax))

;; Convenience layer on top of racket/private/generic.
;; To avoid circular dependencies, racket/private/generic cannot use
;; `parse-keyword-options' (which depends on racket/dict). So we do
;; keyword argument parsing here.
;; Files that use racket/private/generic _must_ pass _all_ keyword
;; arguments to define-generics _in_order_.

(provide define-generics define/generic)

(define-syntax (define-generics stx) ; allows out-of-order / optional kw args
  (syntax-case stx () ; can't use syntax-parse, since it depends on us
    [(_ name (generic . generics-args) ... #:defaults defaults)
     #'(define-generics name
         #:defined-table defined-table
         (generic . generics-args) ...
         #:defaults defaults)]
    [(_ name #:defined-table defined-table (generic . generics-args) ...)
     #'(define-generics name
         #:defined-table defined-table
         (generic . generics-args) ...
         #:defaults ())]
    [(_ name (generic . generics-args) ...)
     #'(define-generics name
         #:defined-table defined-table
         (generic . generics-args) ...
         #:defaults ())]
    [(_ name
       #:defined-table defined-table
       (generic . generics-args) ...
       #:defaults defaults)
     (local [(define name-str (symbol->string (syntax-e #'name)))
             (define (id . strs)
               (datum->syntax
                #'name (string->symbol (apply string-append strs)) #'name))]
      (with-syntax ([name? (id name-str "?")]
                    [gen:name (id "gen:" name-str)])
       #'(define-generics/pre (name gen:name prop:name name?
                                    #:defined-table defined-table
                                    #:defaults defaults
                                    ;; the following are not public
                                    #:prop-defined-already? #f
                                    #:define-contract define-generics-contract)
           (generic . generics-args) ...)))]))

;; generate a contract combinator for instances of a generic interface
(define-syntax (define-generics-contract stx)
  (syntax-case stx ()
    [(_ name name? accessor (generic generic-idx) ...)
     (with-syntax ([name/c (format-id #'name "~a/c" #'name)])
       #`(define-syntax (name/c stx)
           (syntax-case stx ()
             [(_ [method-id ctc] (... ...))
              (andmap (λ (id) (and (identifier? id)
                                   ;; make sure the ids are all
                                   ;; in the interface
                                   (member (syntax-e id) (list 'generic ...))))
                      (syntax->list #'(method-id  (... ...))))
              #'(make-generic-instance/c
                 (quote #,(syntax-e #'name/c))
                 name?
                 accessor
                 (list 'method-id (... ...))
                 (list ctc (... ...))
                 (make-immutable-hash
                  (list (cons 'generic generic-idx) ...)))])))]))

;; make a generic instance contract
(define (make-generic-instance/c name name? accessor ids ctc-args method-map)
  (define ctcs (coerce-contracts 'generic-instance/c ctc-args))
  ;; map method table indices to ids & projections
  (define id+ctc-map
    (for/hash ([id ids] [ctc ctcs])
      (values (hash-ref method-map id)
              (cons id (contract-projection ctc)))))
  (cond [(andmap chaperone-contract? ctcs)
         (chaperone-generic-instance/c
          name name? ids ctcs accessor id+ctc-map method-map)]
        [else
         (impersonator-generic-instance/c
          name name? ids ctcs accessor id+ctc-map method-map)]))

(define (generic-instance/c-name ctc)
  (define method-names
    (map (λ (id ctc) (build-compound-type-name id ctc))
         (base-generic-instance/c-ids ctc)
         (base-generic-instance/c-ctcs ctc)))
  (apply build-compound-type-name
         (cons (base-generic-instance/c-name ctc) method-names)))

;; redirect for use with chaperone-vector
(define ((method-table-redirect ctc blame) vec idx val)
  (define id+ctc-map (base-generic-instance/c-id+ctc-map ctc))
  (define maybe-id+ctc (hash-ref id+ctc-map idx #f))
  (cond [maybe-id+ctc
         (define id (car maybe-id+ctc))
         (define proj (cdr maybe-id+ctc))
         (define blame-string (format "the ~a method of" id))
         ((proj (blame-add-context blame blame-string)) val)]
        [else val]))

;; projection for generic methods
(define ((generic-instance/c-proj proxy-struct proxy-vector) ctc)
  (λ (blame)
    ;; for redirecting the method table accessor
    (define (redirect struct v)
      (proxy-vector
       v
       (method-table-redirect ctc blame)
       (λ (vec i v) v)))
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name ctc)
         val))
      (define accessor (base-generic-instance/c-accessor ctc))
      (proxy-struct val accessor redirect))))

;; recognizes instances of this generic interface
(define ((generic-instance/c-first-order ctc) v)
  (cond [((base-generic-instance/c-name? ctc) v)
         (define accessor (base-generic-instance/c-accessor ctc))
         (define method-table (accessor v))
         (define ids (base-generic-instance/c-ids ctc))
         (define ctcs (base-generic-instance/c-ctcs ctc))
         (define method-map (base-generic-instance/c-method-map ctc))
         ;; do sub-contract first-order checks
         (for/and ([id ids] [ctc ctcs])
           (contract-first-order-passes?
            ctc
            (vector-ref method-table (hash-ref method-map id))))]
        [else #f]))

;; name        - for building ctc name
;; name?       - for first-order checks
;; ids         - for method names (used to build the ctc name)
;; ctcs        - for the contract name
;; accessor    - for chaperoning the struct type property
;; id+ctc-map  - for chaperoning the method table vector
;; method-map  - for first-order checks
(struct base-generic-instance/c
  (name name? ids ctcs accessor id+ctc-map method-map))

(struct chaperone-generic-instance/c base-generic-instance/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (generic-instance/c-proj chaperone-struct chaperone-vector)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))

(struct impersonator-generic-instance/c base-generic-instance/c ()
  #:property prop:contract
  (build-contract-property
   #:projection (generic-instance/c-proj impersonate-struct impersonate-vector)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))
