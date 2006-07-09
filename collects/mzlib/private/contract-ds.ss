#|

why make a separate struct for the contract information
instead of putting it into the wrapper struct in an
extra field?

this probably requires putting the contract info into
its own struct from the beginning, rather than passing
it around flattened out.

|#


(module contract-ds mzscheme
  (require "contract-guts.ss")
  (require-for-syntax "contract-ds-helpers.ss"
                      "contract-helpers.scm")
  
  (provide define-contract-struct)
  
  (define-syntax (define-contract-struct stx)
    (syntax-case stx ()
      [(_ name (fields ...)) 
       (syntax (define-contract-struct name (fields ...) (current-inspector)))]
      [(_ name (fields ...) inspector)
       (and (identifier? (syntax name))
            (andmap identifier? (syntax->list (syntax (fields ...)))))
       (let* ([mutable? (syntax-e (syntax mutable?))]
              [add-suffix
               (λ (suffix)
                 (datum->syntax-object (syntax name)
                                       (string->symbol 
                                        (string-append (symbol->string (syntax-e (syntax name)))
                                                       suffix))
                                       stx))]
              [struct-names (build-struct-names (syntax name)
                                                (syntax->list (syntax (fields ...)))
                                                #f
                                                #t
                                                stx)]
              [struct:-name (list-ref struct-names 0)]
              [struct-maker/val (list-ref struct-names 1)]
              [predicate/val (list-ref struct-names 2)]
              [selectors/val (cdddr struct-names)]
              [struct/c-name/val (add-suffix "/c")]
              [struct/dc-name/val(add-suffix "/dc")]
              [field-count/val (length selectors/val)]
              [f-x/vals (generate-temporaries (syntax (fields ...)))])
         (with-syntax ([struct/c struct/c-name/val]
                       [struct/dc struct/dc-name/val]
                       [field-count field-count/val]
                       [(selectors ...) selectors/val]
                       [struct-maker struct-maker/val]
                       [predicate predicate/val]
                       [contract-name (add-suffix "-contract")]
                       [(selector-indicies ...) (nums-up-to field-count/val)]
                       [(selector-indicies+1 ...) (map add1 (nums-up-to field-count/val))]
                       [(ctc-x ...) (generate-temporaries (syntax (fields ...)))]
                       [(f-x ...) f-x/vals]
                       [((f-xs ...) ...) (generate-arglists f-x/vals)]
                       [wrap-name (string->symbol (format "~a/lazy-contract" (syntax-e (syntax name))))])
           #` 
           (begin
             
             ;; `declare' future bindings for the top-level (so that everyone picks them up)
             #,@(if (eq? (syntax-local-context) 'top-level)
                    (list
                     (syntax
                      (define-syntaxes (contract-type contract-maker contract-predicate contract-get contract-set)
                        (values))))
                    (list))
             
             (define-values (wrap-type wrap-maker wrap-predicate wrap-get wrap-set)
               (make-struct-type 'wrap-name
                                 #f  ;; super struct
                                 2   ;; field count
                                 (- field-count 1)   ;; auto-field-k
                                 #f ;; auto-field-v
                                 '() ;; prop-value-list
                                 inspector))
             
             (define-values (type struct-maker raw-predicate get set)
               (make-struct-type 'name
                                 #f  ;; super struct
                                 field-count
                                 0   ;; auto-field-k
                                 '() ;; auto-field-v
                                 '() ;; prop-value-list
                                 inspector))
             
             (define (predicate x) (or (raw-predicate x) (wrap-predicate x)))
             
             (define-syntax (struct/dc stx)
               (syntax-case stx ()
                 [(_ clause (... ...))
                  (with-syntax ([(maker-args (... ...))
                                 (build-clauses 'struct/dc 
                                                (syntax coerce-contract)
                                                stx 
                                                (syntax (clause (... ...))))])
                    (syntax (contract-maker maker-args (... ...))))]))
             
             (define (do-selection stct i+1)
               (let-values ([(stct fields ...) 
                             (let loop ([stct stct])
                               (cond
                                 [(raw-predicate stct)
                                  ;; found the original value
                                  (values #f (get stct selector-indicies) ...)]
                                 [else
                                  (let ([inner (wrap-get stct 0)])
                                    (if inner
                                        ;; we have a contract to update
                                        (let-values ([(_1 fields ...) (loop inner)])
                                          (let-values ([(fields ...) 
                                                        (rewrite-fields (wrap-get stct 1) fields ...)])
                                            (wrap-set stct 0 #f)
                                            (wrap-set stct selector-indicies+1 fields) ...
                                            (values stct fields ...)))

                                        ;; found a cached version of the value
                                        (values #f (wrap-get stct selector-indicies+1) ...)))]))])
                 (wrap-get stct i+1)))
             
             (define (rewrite-fields contract/info ctc-x ...)
               (let* ([f-x (let ([ctc-field (contract-get (contract/info-contract contract/info)
                                                          selector-indicies)])
                             (let ([ctc (if (procedure? ctc-field)
                                            (ctc-field f-xs ...)
                                            ctc-field)])
                               ((((proj-get ctc) ctc) (contract/info-pos contract/info)
                                                      (contract/info-neg contract/info)
                                                      (contract/info-src-info contract/info)
                                                      (contract/info-orig-str contract/info))
                                ctc-x)))] ...)
                 (values f-x ...)))
             
             (define (stronger-lazy-contract? a b)
               (and (contract-predicate b)
                    (check-sub-contract? 
                     (contract-get a selector-indicies)
                     (contract-get b selector-indicies)) ...))
             
             (define (lazy-contract-proj ctc)
               (λ (pos-blame neg-blame src-info orig-str)
                 (let ([contract/info (make-contract/info ctc pos-blame neg-blame src-info orig-str)])
                   (λ (val)
                     (unless (or (wrap-predicate val) 
                                 (raw-predicate val))
                       (raise-contract-error
                        val
                        src-info
                        pos-blame
                        orig-str
                        "expected <~a>, got ~e" 'name val))
                     (cond
                       [(already-there? contract/info val lazy-depth-to-look)
                        val]
                       [else
                        (wrap-maker val contract/info)])))))
             
             (define (already-there? new-contract/info val depth)
               (cond
                 [(raw-predicate val) #f]
                 [(zero? depth) #f]
                 [(wrap-get val 0)
                  (let ([old-contract/info (wrap-get val 1)])
                    (if (and (eq? (contract/info-pos new-contract/info)
                                  (contract/info-pos old-contract/info))
                             (eq? (contract/info-neg new-contract/info)
                                  (contract/info-neg old-contract/info))
                             (contract-stronger? (contract/info-contract old-contract/info)
                                                 (contract/info-contract new-contract/info)))
                        #t
                        (already-there? new-contract/info (wrap-get val 0) (- depth 1))))]
                 [else 
                  ;; when the zeroth field is cleared out, we don't 
                  ;; have a contract to compare to anymore.
                  #f]))
             
             (define (struct/c ctc-x ...)
               (let ([ctc-x (coerce-contract 'struct/c ctc-x)] ...)
                 (contract-maker ctc-x ...)))
             
             (define (selectors x) (burrow-in x 'selectors selector-indicies)) ...
             
             (define (burrow-in struct selector-name i)
               (cond
                 [(raw-predicate struct)
                  (get struct i)]
                 [(wrap-predicate struct)
                  (if (wrap-get struct 0)
                      (do-selection struct (+ i 1))
                      (wrap-get struct (+ i 1)))]
                 [else
                  (error selector-name "expected <~a>, got ~e" 'name struct)]))
             
             (define (lazy-contract-name ctc)
               (let ([list-of-subcontracts (list (contract-get ctc selector-indicies) ...)])
                 (cond
                   [(andmap contract? list-of-subcontracts)
                    (apply build-compound-type-name 'struct/c list-of-subcontracts)]
                   [else
                    (let ([dots (string->symbol "...")])
                      (apply build-compound-type-name 'struct/dc
                             (map (λ (field ctc) 
                                    (if (contract? ctc)
                                        (build-compound-type-name field ctc)
                                        (build-compound-type-name field dots)))
                                  '(fields ...)
                                  list-of-subcontracts)))])))
             
             (define-values (contract-type contract-maker contract-predicate contract-get contract-set)
               (make-struct-type 'contract-name
                                 #f
                                 field-count
                                 0 ;; auto-field-k
                                 '() ;; auto-field-v
                                 (list (cons proj-prop lazy-contract-proj)
                                       (cons name-prop lazy-contract-name)
                                       (cons first-order-prop (λ (ctc) predicate))
                                       (cons stronger-prop stronger-lazy-contract?)))))))]))
  
  (define-struct contract/info (contract pos neg src-info orig-str))
  
  (define max-cache-size 5)
  (define lazy-depth-to-look 5) 
  
  (define (check-sub-contract? x y)
    (cond
      [(and (stronger-pred? x) (stronger-pred? y))
       (contract-stronger? x y)]
      [(and (procedure? x) (procedure? y))
       (procedure-closure-contents-eq? x y)]
      [else #f]))
  
  #|
test case:
(define-contract-struct s (a b))

this contract:

(s/dc [a (flat number?)]
      [b (λ (x) (and (number? x) (< a b)))])

should not signal a less than error for this value:

(make-s #f 2)

but this one:

(s/dc [a (flat boolean?)]
      [b (λ (x) (and (number? x) (< a b)))])

should

|#
  
  #|

test-case:
  (define-contract-struct s (a b))
  (s/dc [x 1])
  => wrong field count exn
  |#
  
  
  )