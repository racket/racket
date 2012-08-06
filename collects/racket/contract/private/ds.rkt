#lang racket/base

#|

Need to break the parent pointer links at some point. 
This leaks, as is.

---

why make a separate struct for the contract information
instead of putting it into the wrapper struct in an
extra field?

this probably requires putting the contract info into
its own struct from the beginning, rather than passing
it around flattened out.

|#

(require "guts.rkt"
         "prop.rkt"
         "blame.rkt"
         "opt.rkt"
         "misc.rkt")
(require (for-syntax racket/base)
         (for-syntax "ds-helpers.rkt")
         (for-syntax "helpers.rkt")
         (for-syntax "opt-guts.rkt"))

(provide define-contract-struct
         contract-struct)

;; main : syntax syntax[list-of-identifier] syntax boolean -> syntax
;; define-struct? tells us if this is a 'contract-struct' or a 'define-contract-struct'
(define-for-syntax (main stx name fields inspector define-struct?)
  (with-syntax ([name name]
                [(fields ...) fields]
                [inspector inspector])
    
    (let* ([mutable? (syntax-e (syntax mutable?))]
           [add-suffix
            (λ (suffix)
              (datum->syntax (syntax name)
                             (string->symbol 
                              (string-append (symbol->string (syntax-e (syntax name)))
                                             suffix))
                             stx))]
           [struct-names (build-struct-names (syntax name)
                                             (syntax->list (syntax (fields ...)))
                                             #f
                                             #t
                                             stx)]
           [struct:-name/val (list-ref struct-names 0)]
           [struct-maker/val (list-ref struct-names 1)]
           [predicate/val (list-ref struct-names 2)]
           [selectors/val (cdddr struct-names)]
           [struct/c-name/val (add-suffix "/c")]
           [struct/dc-name/val (add-suffix "/dc")]
           [field-count/val (length selectors/val)]
           [f-x/vals (generate-temporaries (syntax (fields ...)))]
           [f-xs/vals (generate-arglists f-x/vals)])
      
      (with-syntax ([struct/c struct/c-name/val]
                    [struct/dc struct/dc-name/val]
                    [field-count field-count/val]
                    [(selectors ...) selectors/val]
                    [struct:-name struct:-name/val]
                    [struct-maker struct-maker/val]
                    [predicate predicate/val]
                    [the-contract (add-suffix "-contract")]
                    [(selector-indices ...) (nums-up-to field-count/val)]
                    [(selector-indices+1 ...) (map add1 (nums-up-to field-count/val))]
                    [(ctc-x ...) (generate-temporaries (syntax (fields ...)))]
                    [(f-x ...) f-x/vals]
                    [((f-xs ...) ...) f-xs/vals]
                    [wrap-name (string->symbol (format "~a/lazy-contract" (syntax-e (syntax name))))]
                    [opt-wrap-name (string->symbol (format "~a/lazy-opt-contract" (syntax-e (syntax name))))])
        #` 
        (begin
          
          ;; `declare' future bindings for the top-level (so that everyone picks them up)
          #,@(if (eq? (syntax-local-context) 'top-level)
                 (list
                  (syntax
                   (define-syntaxes (contract-type contract-maker contract-predicate contract-get contract-set 
                                                   already-there? burrow-in rewrite-fields wrap-get)
                     (values))))
                 (list))
          
          (define-syntax name 
            #,(let ([the-info-list 
                     #`(list #'struct:-name 
                             #'struct-maker 
                             #'predicate
                             (reverse (list #'selectors ...))
                             (list #,@(map (λ (x) #f) (syntax->list #'(selectors ...))))
                             #t)])
                (if define-struct?
                    the-info-list
                    #`(contract-struct-transformer #,the-info-list
                                                   (λ (stx) 
                                                     (if (identifier? stx)
                                                         #'struct-maker
                                                         (let ([pr (syntax-e stx)])
                                                           (if (pair? pr)
                                                               (datum->syntax #'here (cons #'struct-maker (cdr pr)))
                                                               (raise-syntax-error #f "what happened?" stx)))))))))
          
          (define (evaluate-attrs stct contract/info)
            (when (wrap-parent-get stct 0) ;; test to make sure this even has attributes
              (let* ([any-unknown? #f]
                     [any-became-known? #f]
                     [synth-info (wrap-parent-get stct 0)]
                     [ht (synth-info-vals synth-info)])
                (hash-for-each 
                 ht
                 (lambda (k v)
                   (when (unknown? v)
                     (let ([proc (unknown-proc v)])
                       (let ([new (proc (wrap-get stct selector-indices+1) ...)])
                         (cond
                           [(unknown? new) 
                            (set! any-unknown? #t)]
                           [else
                            (set! any-became-known? #t)
                            (hash-set! ht k new)]))))))
                (unless any-unknown?
                  (check-synth-info-test stct synth-info contract/info))
                (when any-became-known?
                  (for-each 
                   (lambda (x) ((evaluate-attr-prop-accessor x) x contract/info))
                   (synth-info-parents synth-info)))
                (unless any-unknown?
                  (set-synth-info-parents! synth-info '())))))
          
          (define-values (wrap-type wrap-maker wrap-predicate wrap-get wrap-set)
            (make-struct-type 'wrap-name
                              wrap-parent-type  ;; super struct
                              2   ;; field count
                              (max 0 (- field-count 1))   ;; auto-field-k
                              #f ;; auto-field-v
                              (list (cons evaluate-attr-prop evaluate-attrs))
                              inspector))
          
          (define-values (opt-wrap-type opt-wrap-maker opt-wrap-predicate opt-wrap-get opt-wrap-set)
            (make-struct-type 'opt-wrap-name
                              #f  ;; super struct
                              2   ;; field count
                              (+ 1 field-count)   ;; auto-field-k
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
          
          (define (predicate x) (or (raw-predicate x) (opt-wrap-predicate x) (wrap-predicate x)))
          
          (define-syntax (struct/dc stx)
            (syntax-case stx ()
              [(_ clause (... ...))
               (with-syntax ([((maker-args (... ...))
                               (names (... ...)))
                              (build-clauses 'struct/dc 
                                             (syntax coerce-contract)
                                             stx 
                                             (syntax (clause (... ...))))])
                 (syntax 
                  (let ([names 'names] (... ...))
                    (contract-maker maker-args (... ...)))))]))
          
          (define (do-selection stct i+1)
            (let-values ([(stct fields ...) 
                          (let loop ([stct stct])
                            (cond
                              [(raw-predicate stct)
                               ;; found the original value
                               (values #f (get stct selector-indices) ...)]
                              
                              [(opt-wrap-predicate stct)
                               (let ((inner (opt-wrap-get stct 0)))
                                 (if inner
                                     (let* ((info (opt-wrap-get stct 1))
                                            (enforcer (opt-contract/info-enforcer info)))
                                       (let-values ([(inner-stct fields ...) (loop inner)])
                                         (let-values ([(fields ...) (enforcer stct fields ...)])
                                           (opt-wrap-set stct 0 #f)
                                           (opt-wrap-set stct selector-indices+1 fields) ...
                                           (values stct fields ...))))
                                     
                                     ;; found a cached version
                                     (values #f (opt-wrap-get stct selector-indices+1) ...)))]
                              [(wrap-predicate stct)
                               (let ([inner (wrap-get stct 0)])
                                 (if inner
                                     ;; we have a contract to update
                                     (let ([contract/info (wrap-get stct 1)])
                                       (let-values ([(_1 fields ...) (loop inner)])
                                         (let-values ([(fields ...) 
                                                       (rewrite-fields stct contract/info fields ...)])
                                           (wrap-set stct 0 #f)
                                           (wrap-set stct selector-indices+1 fields) ...
                                           (evaluate-attrs stct contract/info)
                                           (values stct fields ...))))
                                     
                                     ;; found a cached version of the value
                                     (values #f (wrap-get stct selector-indices+1) ...)))]))])
              (cond 
                [(opt-wrap-predicate stct) (opt-wrap-get stct i+1)]
                [(wrap-predicate stct) (wrap-get stct i+1)])))
          
          (define (rewrite-fields parent contract/info ctc-x ...)
            (let* ([f-x (let* ([ctc-field (contract-get (contract/info-contract contract/info)
                                                        selector-indices)]
                               [ctc (if (contract-struct? ctc-field)
                                        ctc-field
                                        (ctc-field f-xs ...))]
                               
                               [ctc-field-val
                                (((contract-projection ctc)
                                  (contract/info-blame contract/info))
                                 ctc-x)])
                          (update-parent-links parent ctc-field-val)
                          ctc-field-val)] ...)
              (values f-x ...)))
          
          (define (stronger-lazy-contract? a b)
            (and (contract-predicate b)
                 (let ([a-sel (contract-get a selector-indices)]
                       [b-sel (contract-get b selector-indices)])
                   (if (contract-struct? a-sel)
                       (if (contract-struct? b-sel)
                           (contract-stronger? a-sel b-sel)
                           #f)
                       (if (contract-struct? b-sel)
                           #f
                           (procedure-closure-contents-eq? a-sel b-sel))))
                 ...))
          
          (define (lazy-contract-proj ctc)
            (λ (blame)
              (let ([contract/info (make-contract/info ctc blame)])
                (λ (val)
                  (unless (or (wrap-predicate val)
                              (opt-wrap-predicate val)
                              (raw-predicate val))
                    (raise-blame-error
                     blame
                     val
                     '(expected: "~s" given: "~e") 'name val))
                  (cond
                    [(already-there? contract/info val lazy-depth-to-look)
                     val]
                    [else
                     (let ([wrapper (wrap-maker val contract/info)])
                       (let ([synth-setup-stuff (contract-get ctc field-count)])
                         (when synth-setup-stuff
                           (let ([ht (make-hash)])
                             (for-each (λ (pr) (hash-set! ht (car pr) (make-unknown (cdr pr))))
                                       (cdr synth-setup-stuff))
                             (wrap-parent-set wrapper 0 (make-synth-info '() ht (car synth-setup-stuff))))))
                       wrapper)])))))
          
          (define (already-there? new-contract/info val depth)
            (cond
              [(raw-predicate val) #f]
              [(zero? depth) #f]
              [(wrap-predicate val)
               (and (wrap-get val 0)
                    (let ([old-contract/info (wrap-get val 1)])
                      (if (and (equal? (contract/info-blame new-contract/info)
                                       (contract/info-blame old-contract/info))
                               (contract-stronger? (contract/info-contract old-contract/info)
                                                   (contract/info-contract new-contract/info)))
                          #t
                          (already-there? new-contract/info (wrap-get val 0) (- depth 1)))))]
              [else 
               ;; when the zeroth field is cleared out, we don't 
               ;; have a contract to compare to anymore.
               #f]))
          
          (define (struct/c ctc-x ...)
            (let ([ctc-x (coerce-contract 'struct/c ctc-x)] ...)
              (contract-maker ctc-x ... #f)))
          
          (define (selectors x)
            (burrow-in x 'selectors selector-indices))
          ...
          
          (define (burrow-in struct selector-name i)
            (cond
              [(raw-predicate struct)
               (get struct i)]
              [(opt-wrap-predicate struct)
               (if (opt-wrap-get struct 0)
                   (do-selection struct (+ i 1))
                   (opt-wrap-get struct (+ i 1)))]
              [(wrap-predicate struct)
               (if (wrap-get struct 0)
                   (do-selection struct (+ i 1))
                   (wrap-get struct (+ i 1)))]
              [else
               (error selector-name "expected: ~s, got ~e" 'name struct)]))
          
          (define (lazy-contract-name ctc)
            (do-contract-name 'struct/c
                              'struct/dc
                              (list (contract-get ctc selector-indices) ...)
                              '(fields ...)
                              (contract-get ctc field-count)))
          
          (define lazy-contract-property
            (build-contract-property
             #:projection lazy-contract-proj
             #:name lazy-contract-name
             #:first-order (lambda (ctc) predicate)
             #:stronger stronger-lazy-contract?))
          
          (define-values (contract-type contract-maker contract-predicate contract-get contract-set)
            (make-struct-type 'the-contract
                              #f
                              (+ field-count 1) ;; extra field is for synthesized attribute ctcs
                              ;; it is a list whose first element is
                              ;; a procedure (called once the attrs are known) that
                              ;; indicates if the test passes. the rest of the elements are
                              ;; procedures that build the attrs
                              ;; this field is #f when there is no synthesized attrs
                              0 ;; auto-field-k
                              '() ;; auto-field-v
                              (list (cons prop:contract lazy-contract-property))))
          
          (define-for-syntax (build-enforcer opt/i opt/info name stx clauses
                                             helper-id-var helper-info helper-freev
                                             enforcer-id-var)
            (define (make-free-vars free-vars freev)
              (let loop ([i 0]
                         [stx null]
                         [free-vars free-vars])
                (cond
                  [(null? free-vars) (reverse stx)]
                  [else (loop (+ i 1)
                              (cons (with-syntax ((var (car free-vars))
                                                  (freev freev)
                                                  (j (+ i 2)))
                                      (syntax (var (opt-wrap-get stct j)))) stx)
                              (cdr free-vars))])))
            
            (let*-values ([(inner-val) #'val]
                          [(clauses lifts superlifts stronger-ribs)
                           (build-enforcer-clauses opt/i
                                                   (opt/info-change-val inner-val opt/info)
                                                   name
                                                   stx
                                                   clauses
                                                   (list (syntax f-x) ...)
                                                   (list (list (syntax f-xs) ...) ...)
                                                   helper-id-var
                                                   helper-info
                                                   helper-freev)])
              (with-syntax ([(clause (... ...)) clauses]
                            [enforcer-id enforcer-id-var]
                            [helper-id helper-id-var]
                            [((free-var free-var-val) (... ...))
                             (make-free-vars (append (opt/info-free-vars opt/info)) #'freev)])
                (values
                 #`(λ (stct f-x ...)
                     (let ((free-var free-var-val) (... ...))
                       #,(bind-lifts
                          lifts
                          #'(let* (clause (... ...))
                              (values f-x ...)))))
                 lifts
                 superlifts
                 stronger-ribs))))
          
          ;;
          ;; struct/dc opter
          ;;
          (define/opter (struct/dc opt/i opt/info stx)
            (syntax-case stx ()
              [(_ clause (... ...))
               (let ((enforcer-id-var (car (generate-temporaries (syntax (enforcer)))))
                     (helper-id-var (car (generate-temporaries (syntax (helper)))))
                     (contract/info-var (car (generate-temporaries (syntax (contract/info)))))
                     (id-var (car (generate-temporaries (syntax (id))))))
                 (let-values ([(enforcer lifts superlifts stronger-ribs)
                               (build-enforcer opt/i
                                               opt/info
                                               'struct/dc
                                               stx
                                               (syntax (clause (... ...)))
                                               helper-id-var
                                               #'info
                                               #'freev
                                               enforcer-id-var)])
                   (let ([to-save (append (opt/info-free-vars opt/info)
                                          (lifts-to-save lifts))])
                     (with-syntax ((val (opt/info-val opt/info))
                                   (blame (opt/info-blame opt/info))
                                   (ctc (opt/info-contract opt/info))
                                   (enforcer-id enforcer-id-var)
                                   (helper-id helper-id-var)
                                   (contract/info contract/info-var)
                                   (id id-var)
                                   ((j (... ...)) (let loop ([i 2]
                                                             [lst to-save])
                                                    (cond
                                                      [(null? lst) null]
                                                      [else (cons i (loop (+ i 1) (cdr lst)))])))
                                   ((free-var (... ...)) to-save))
                       (with-syntax ([(stronger-this-var (... ...)) (map stronger-rib-this-var stronger-ribs)]
                                     [(stronger-that-var (... ...)) (map stronger-rib-that-var stronger-ribs)]
                                     [(stronger-exps (... ...)) (map stronger-rib-stronger-exp stronger-ribs)]
                                     [(stronger-indexes (... ...)) (build-list (length stronger-ribs) 
                                                                               (λ (x) (+ x 2)))]
                                     [(stronger-var (... ...)) (map stronger-rib-save-id stronger-ribs)])
                         
                         (let ([partials
                                (list (cons id-var #'(begin-lifted (box 'identity)))
                                      (cons enforcer-id-var enforcer)
                                      (cons contract/info-var
                                            (syntax 
                                             (make-opt-contract/info ctc enforcer-id id))))])
                           (build-optres
                            #:exp
                            (syntax 
                             (cond
                               [(opt-wrap-predicate val)
                                (if (and (opt-wrap-get val 0)
                                         (let ([stronger-this-var stronger-var] 
                                               (... ...)
                                               
                                               ;; this computation is bogus
                                               ;; it only works if the stronger vars and the things
                                               ;; saved in the wrapper are the same
                                               [stronger-that-var (opt-wrap-get val stronger-indexes)]
                                               (... ...))
                                           (and 
                                            ;; make sure this is the same contract -- if not,
                                            ;; the rest of this test is bogus and may fail at runtime
                                            (eq? id (opt-contract/info-id (opt-wrap-get val 1)))
                                            stronger-exps (... ...))))
                                    val
                                    (let ([w (opt-wrap-maker val contract/info)])
                                      (opt-wrap-set w j free-var) (... ...)
                                      w))]
                               [(or (raw-predicate val)
                                    (wrap-predicate val))
                                (let ([w (opt-wrap-maker val contract/info)])
                                  (opt-wrap-set w j free-var) (... ...)
                                  w)]
                               [else
                                (raise-blame-error
                                 blame
                                 val
                                 '(expected: "~s" given: "~e")
                                 (contract-name ctc)
                                 val)]))
                            #:lifts lifts
                            #:superlifts superlifts
                            #:partials partials
                            #:flat #f
                            #:opt #f
                            #:stronger-ribs stronger-ribs
                            #:chaperone #f)))))))])))))))

(define-syntax (define-contract-struct stx)
  (syntax-case stx ()
    [(_ name (fields ...)) 
     (syntax (define-contract-struct name (fields ...) (current-inspector)))]
    [(_ name (fields ...) inspector)
     (and (identifier? (syntax name))
          (andmap identifier? (syntax->list (syntax (fields ...)))))
     (main stx #'name #'(fields ...) #'inspector #t)]))

(define-syntax (contract-struct stx)
  (syntax-case stx()
    [(_ name (fields ...))
     #'(contract-struct name (fields ...) #:inspector (current-inspector))]
    [(_ name (fields ...) #:inspector e)
     (main stx #'name #'(fields ...) #'e #f)]))

(define (do-contract-name name/c name/dc list-of-subcontracts fields attrs)
  (cond
    [(and (andmap contract-struct? list-of-subcontracts) (not attrs))
     (apply build-compound-type-name name/c list-of-subcontracts)]
    [else
     (let ([fields 
            (map (λ (field ctc) 
                   (if (contract-struct? ctc)
                       (build-compound-type-name field ctc)
                       (build-compound-type-name field '...)))
                 fields
                 list-of-subcontracts)])
       (cond
         [attrs 
          (apply build-compound-type-name
                 name/dc
                 (append fields
                         (list 'where)
                         (map (λ (x) `(,(car x) ...))
                              (reverse (cdr attrs)))
                         (list 'and '...)))]
         [else (apply build-compound-type-name name/dc fields)]))]))

(define-struct contract/info (contract blame))
(define-struct opt-contract/info (contract enforcer id))

;; parents : (listof wrap-parent)
;; vals : hash-table[symbol -o> (union (make-unknown proc[field-vals -> any]) any)
;; test : proc[vals-hash-table -> boolean]
(define-struct synth-info (parents vals test) #:mutable)

(define-struct unknown (proc))

(define secret (gensym))
(define (synthesized-value wrap key)
  (unless (wrap-parent-predicate wrap)
    (error 'synthesized-value "expected struct value with contract as first argument, got ~e" wrap))
  (let ([ans (hash-ref (synth-info-vals (wrap-parent-get wrap 0))
                       key
                       secret)])
    (when (eq? ans secret)
      (error 'synthesized-value "the key ~e is not mapped in ~e" key wrap))
    ans))

(define (check-synth-info-test stct synth-info contract/info)
  (unless ((synth-info-test synth-info) (synth-info-vals synth-info))
    (raise-blame-error
     (contract/info-blame contract/info)
     stct
     '("failed `and' clause" given: "~e")
     stct)))

(define-values (evaluate-attr-prop evaluate-attr-prop-predicate evaluate-attr-prop-accessor)
  (make-struct-type-property 'evaluate-attr-prop))

(define-values (wrap-parent-type wrap-parent-maker wrap-parent-predicate wrap-parent-get wrap-parent-set)
  (make-struct-type 'wrap-parent
                    #f ;; parent
                    0  ;; fields
                    1  ;; auto fields
                    ))

(define (update-parent-links parent ctc-field-val)
  (when (wrap-parent-predicate ctc-field-val)
    (let ([old (wrap-parent-get ctc-field-val 0)])
      (when old
        ;; add in new parent
        (wrap-parent-set ctc-field-val 0 
                         (make-synth-info
                          (cons parent (synth-info-parents old))
                          (synth-info-vals old)
                          (synth-info-test old)))))))

(define max-cache-size 5)
(define lazy-depth-to-look 5) 

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

