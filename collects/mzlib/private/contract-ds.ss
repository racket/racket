
(module contract-ds mzscheme
  (require "contract-util.ss"
           "same-closure.ss")
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
       (let* ([add-suffix
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
                       [wrap-name (string->symbol (format "~a-wrap" (syntax-e (syntax name))))])
           #` 
           (begin
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
               ;(ensure-well-formed stx field-count)
               (syntax-case stx ()
                 [(_ clause (... ...))
                  (with-syntax ([(maker-args (... ...))
                                 (build-clauses 'struct/dc 
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
                                 [(wrap-get stct 0)
                                  ;; we have a contract to update
                                  (let-values ([(_1 fields ...) (loop (wrap-get stct 0))])
                                    (let-values ([(fields ...) 
                                                  (rewrite-fields (wrap-get stct 1) fields ...)])
                                      (wrap-set stct 0 #f)
                                      (wrap-set stct selector-indicies+1 fields) ...
                                      (values stct fields ...)))]
                                 [else
                                  ;; found a cached version of the value
                                  (values #f (wrap-get stct selector-indicies+1) ...)]))])
                 (wrap-get stct i+1)))
             
             (define (rewrite-fields stct ctc-x ...)
               (let* ([f-x (let ([ctc-field (contract-get stct selector-indicies)])
                             (let ([ctc (if (procedure? ctc-field)
                                            (ctc-field f-xs ...)
                                            ctc-field)])
                               (((proj-get ctc) ctc) ctc-x)))] ...)
                 (values f-x ...)))
             
             (define (stronger-lazy-contract? a b)
               (and (contract-predicate b)
                    (check-sub-contract? 
                     (contract-get a selector-indicies)
                     (contract-get b selector-indicies)) ...))
             
             (define (lazy-contract-proj ctc)
               (λ (val)
                 (unless (or (wrap-predicate val) 
                             (raw-predicate val))
                   (blame (format "expected <~a>, got ~e" 'name val)))
                 (cond
                   [(already-there? ctc val lazy-depth-to-look)
                    val]
                   [else
                    (wrap-maker val ctc)])))
             
             (define (already-there? ctc val depth)
               (cond
                 [(raw-predicate val) #f]
                 [(zero? depth) #f]
                 [(wrap-get val 0)
                  (if (contract-stronger? (wrap-get val 1) ctc)
                      #t
                      (already-there? ctc (wrap-get val 0) (- depth 1)))]
                 [else 
                  ;; when the zeroth field is cleared out, we don't 
                  ;; have a contract to compare to anymore.
                  #f]))
             
             (define (struct/c ctc-x ...)
               (contract-maker ctc-x ...))
             
             (define (no-depend-apply-to-fields ctc fields ...)
               (let ([ctc-x (contract-get ctc selector-indicies)] ...)
                 (values (((proj-get ctc-x) ctc-x) fields) ...)))
             
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
             
             (define-values (contract-type contract-maker contract-predicate contract-get contract-set)
               (make-struct-type 'contract-name
                                 #f
                                 field-count
                                 0 ;; auto-field-k
                                 '() ;; auto-field-v
                                 (list (cons proj-prop lazy-contract-proj)
                                       (cons stronger-prop stronger-lazy-contract?)))))))]))
  
  (define max-cache-size 5)
  (define lazy-depth-to-look 5) 
  
  (define (check-sub-contract? x y)
    (cond
      [(and (proj-pred? x) (proj-pred? y))
       (contract-stronger? x y)]
      [(and (procedure? x) (procedure? y))
       (same-closure? x y)]
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