#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info
                     "opt-guts.rkt"
                     (only-in "ds-helpers.rkt" defeat-inlining))
         syntax/location
         racket/list
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt"
         "opt.rkt")

(provide struct/c
         (rename-out [-struct/dc struct/dc]))

(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
       (syntax/loc stx (#%expression x)))]))

;; name is symbol
;; predicate is (-> any bool)
;; immutables is (listof (list natural contract selector-proc))
;; mutables is (listof (list natural contract selector-proc mutator-proc))
(define-struct base-struct/c (name predicate immutables mutables))

(define (struct/c-name ctc)
  (let ([ctcs (map second
                   (sort (append (base-struct/c-immutables ctc) (base-struct/c-mutables ctc))
                         < #:key first))])
    (apply build-compound-type-name 'struct/c (base-struct/c-name ctc) ctcs)))

(define (check-struct/c ctc)
  (let ([name (base-struct/c-name ctc)]
        [pred? (base-struct/c-predicate ctc)]
        [ctc/ref-pairs (map (λ (l) (cons (second l) (third l)))
                            (append (base-struct/c-immutables ctc) (base-struct/c-mutables ctc)))])
    (λ (val fail [first-order? #f])
      (unless (pred? val)
        (fail "expected: ~s, got ~e" name val))
      (when first-order?
        (for ([p (in-list ctc/ref-pairs)])
          (let ([c (car p)] [v ((cdr p) val)])
            (unless (contract-first-order-passes? c v)
              (fail "expected: ~s, got ~e" (contract-name c) v)))))
      #t)))

(define (struct/c-first-order ctc)
  (let ([f (check-struct/c ctc)])
    (λ (val)
      (let/ec fail
        (f val (λ args (fail #f)) #t)))))

(define (flat-struct/c-proj ctc)
  (let ([checker (check-struct/c ctc)]
        [name (base-struct/c-name ctc)]
        [pred (base-struct/c-predicate ctc)]
        [projs (map contract-projection (map second (base-struct/c-immutables ctc)))]
        [refs (map third (base-struct/c-immutables ctc))])
    (λ (blame)
      (let ([pos-projs (map (λ (f) (f blame)) projs)])
        (λ (val)
          (checker val (λ args (apply raise-blame-error blame val args)))
          (for ([p (in-list pos-projs)] [ref (in-list refs)])
            (p (ref val)))
          val)))))

(define-struct (flat-struct/c base-struct/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection flat-struct/c-proj))

(define (chaperone-struct/c-proj ctc)
  (let-values ([(flat-imms chap-imms)
                (partition (λ (l) (flat-contract? (second l))) (base-struct/c-immutables ctc))])
    (let ([checker (check-struct/c ctc)]
          [name (base-struct/c-name ctc)]
          [pred (base-struct/c-predicate ctc)]
          [flat-imm-projs (map (compose contract-projection second) flat-imms)]
          [flat-imm-refs (map third flat-imms)]
          [chap-imm-projs (map (compose contract-projection second) chap-imms)]
          [chap-imm-refs (map third chap-imms)]
          [mut-projs (map (compose contract-projection second) (base-struct/c-mutables ctc))]
          [mut-refs (map third (base-struct/c-mutables ctc))]
          [mut-sets (map fourth (base-struct/c-mutables ctc))])
      (λ (blame)
        (let* ([swapped-blame (blame-swap blame)]
               [flat-imm-pos-projs (map (λ (f) (f blame)) flat-imm-projs)]
               [chap-imm-pos-projs (map (λ (f) (f blame)) chap-imm-projs)]
               [mut-pos-projs (map (λ (f) (f blame)) mut-projs)]
               [mut-neg-projs (map (λ (f) (f swapped-blame)) mut-projs)])
          (λ (val)
            (checker val (λ args (apply raise-blame-error blame val args)))
            (for ([p (in-list flat-imm-pos-projs)]
                  [ref (in-list flat-imm-refs)])
              (p (ref val)))
            
            ;; While gathering up the selectors and the appropriate projections,
            ;; we go ahead and apply the projection to check the first order properties.
            (let ([chaperone-args (list impersonator-prop:contracted ctc)])
              
              ;; combined-imm-refs
              (for ([p (in-list chap-imm-pos-projs)]
                    [ref (in-list chap-imm-refs)])
                (p (ref val))
                (set! chaperone-args (list* ref (λ (s v) (p v)) chaperone-args)))
              
              ;; combined-mut-refs
              (for ([p (in-list mut-pos-projs)]
                    [ref (in-list mut-refs)])
                (p (ref val))
                (set! chaperone-args (list* ref (λ (s v) (p v)) chaperone-args)))
              
              ;; combined-mut-sets
              (for ([p (in-list mut-neg-projs)]
                    [set (in-list mut-sets)])
                (set! chaperone-args (list* set (λ (s v) (p v)) chaperone-args)))
              
              (apply chaperone-struct val chaperone-args))))))))

(define-struct (chaperone-struct/c base-struct/c) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name struct/c-name
     #:first-order struct/c-first-order
     #:projection chaperone-struct/c-proj)))

(define (impersonator-struct/c-proj ctc)
  (let-values ([(flat-imms chap-imms)
                (partition (λ (l) (flat-contract? (second l))) (base-struct/c-immutables ctc))])
    (let ([checker (check-struct/c ctc)]
          [name (base-struct/c-name ctc)]
          [pred (base-struct/c-predicate ctc)]
          [flat-imm-projs (map (compose contract-projection second) flat-imms)]
          [flat-imm-refs (map third flat-imms)]
          [chap-imm-projs (map (compose contract-projection second) chap-imms)]
          [chap-imm-refs (map third chap-imms)]
          [mut-projs (map (compose contract-projection second) (base-struct/c-mutables ctc))]
          [mut-refs (map third (base-struct/c-mutables ctc))]
          [mut-sets (map fourth (base-struct/c-mutables ctc))])
      (λ (blame)
        (let* ([swapped-blame (blame-swap blame)]
               [flat-imm-pos-projs (map (λ (f) (f blame)) flat-imm-projs)]
               [chap-imm-pos-projs (map (λ (f) (f blame)) chap-imm-projs)]
               [mut-pos-projs (map (λ (f) (f blame)) mut-projs)]
               [mut-neg-projs (map (λ (f) (f swapped-blame)) mut-projs)])
          (λ (val)
            (checker val (λ args (apply raise-blame-error blame val args)))
            (for ([p (in-list flat-imm-pos-projs)]
                  [ref (in-list flat-imm-refs)])
              (p (ref val)))
            ;; While gathering up the selectors and the appropriate projections,
            ;; we go ahead and apply the projection to check the first order properties.
            (let ([combined-imm-refs 
                   (for/list ([p (in-list chap-imm-pos-projs)]
                              [ref (in-list chap-imm-refs)])
                     (p (ref val))
                     (list ref (λ (s v) (p v))))]
                  [combined-mut-refs
                   (for/list ([p (in-list mut-pos-projs)]
                              [ref (in-list mut-refs)])
                     (p (ref val))
                     (list ref (λ (s v) (p v))))]
                  [combined-mut-sets
                   (for/list ([p (in-list mut-neg-projs)]
                              [set (in-list mut-sets)])
                     (list set (λ (s v) (p v))))])
              (apply impersonate-struct
                     (apply chaperone-struct val
                            combined-imm-refs)
                     (flatten (list combined-mut-refs combined-mut-sets
                                    impersonator-prop:contracted ctc))))))))))

(define-struct (impersonator-struct/c base-struct/c) ()
  #:property prop:contract
  (build-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection impersonator-struct/c-proj))

(define-syntax (do-struct/c stx)
  (syntax-case stx ()
    [(_ struct-name args ...)
     (and (identifier? (syntax struct-name))
          (struct-info? (syntax-local-value (syntax struct-name) (λ () #f))))
     (let* ([si (extract-struct-info (syntax-local-value (syntax struct-name)))]
            [predicate-id (third si)]
            [selector-ids (reverse (fourth si))]
            [mutator-ids (reverse (fifth si))]
            [ctcs (syntax->list #'(args ...))]
            [ctc-names (generate-temporaries #'(args ...))])
       (unless (= (length selector-ids) (length ctcs))
         (raise-syntax-error 'struct/c 
                             (format "expected ~a contracts because struct ~a has ~a fields"
                                     (length selector-ids)
                                     (syntax-e #'struct-name)
                                     (length selector-ids))
                             stx))
       (unless predicate-id
         (raise-syntax-error 'struct/c 
                             (format "could not determine predicate for ~s" (syntax-e #'struct-name))
                             stx))
       (unless (andmap values selector-ids)
         (raise-syntax-error 'struct/c
                             (format "could not determine selectors for ~s" (syntax-e #'struct-name))
                             stx))
       
       (let ([combined-ids (for/list ([n (in-naturals)]
                                      [ctc-name (in-list ctc-names)]
                                      [ref-name (in-list selector-ids)]
                                      [mut-name (in-list mutator-ids)])
                             (list n ctc-name ref-name mut-name))])
         (let-values ([(mutables immutables) (partition (λ (l) (fourth l)) combined-ids)])
           (with-syntax ([(ctc-x ...) ctc-names]
                         [predicate-id predicate-id]
                         [((imm-count imm-ctc-x imm-ref _) ...) immutables]
                         [((mut-count mut-ctc-x mut-ref mut-set) ...) mutables])
             (syntax
              (let ([ctc-x (coerce-contract 'struct/c args)] ...)
                (let ([immutables (list (list imm-count imm-ctc-x imm-ref) ...)]
                      [mutables (list (list mut-count mut-ctc-x mut-ref mut-set) ...)])
                  (struct/c/proc 'struct-name predicate-id immutables mutables))))))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))

(define (struct/c/proc struct-name predicate immutables mutables)
  (for ([lst (in-list immutables)])
    (define imm-count (list-ref lst 0))
    (define imm-ctc (list-ref lst 1))
    (unless (chaperone-contract? imm-ctc)
      (error 'struct/c "expected a chaperone contract for immutable field ~v (counting from 0), got ~e"
             imm-count imm-ctc)))
  (cond
    [(and (null? mutables) (andmap (λ (l) (flat-contract? (second l))) immutables))
     (make-flat-struct/c struct-name predicate immutables mutables)]
    [(andmap (λ (l) (chaperone-contract? (second l))) mutables)
     (make-chaperone-struct/c struct-name predicate immutables mutables)]
    [else
     (make-impersonator-struct/c struct-name predicate immutables mutables)]))

(define unique (box #f))
(define (un-dep ctc obj blame immutable-field)
  (let ([ctc (coerce-contract 'struct/dc ctc)])
    (when immutable-field
      (check-chaperone-contract immutable-field ctc))
    (((contract-projection ctc) blame) obj)))

(define (struct/dc-name ctc)
  'struct/dc)
(define (struct/dc-first-order ctc)
  (struct/dc-pred ctc))

(define (struct/dc-proj ctc)
  (define pred? (struct/dc-pred ctc))
  (define mk-proj ((struct/dc-apply-proj ctc) ctc))
  (λ (blame)
    (define proj (mk-proj blame))
    (λ (v)
      (cond
        [(and (struct/dc-imp-prop-pred? v)
              (contract-stronger? (struct/dc-imp-prop-get v) ctc))
         v]
        [else
         (unless (pred? v)
           (raise-blame-error blame v "expected a ~a"
                              (struct/dc-struct-name ctc)))
         (proj v)]))))

(define (struct/dc-stronger? this that)
  (and (struct/dc? that)
       (eq? (struct/dc-pred this)
            (struct/dc-pred that))
       (let loop ([this-procs/ctcs (struct/dc-procs/ctcs this)]
                  [that-procs/ctcs (struct/dc-procs/ctcs that)])
         (cond
           [(and (null? this-procs/ctcs) (null? that-procs/ctcs)) #t]
           [(and (pair? this-procs/ctcs) (pair? that-procs/ctcs))
            (define fst-this (car this-procs/ctcs))
            (define fst-that (car that-procs/ctcs))
            (cond
              [(and (contract-struct? fst-this) (contract-struct? fst-that))
               (and (contract-stronger? fst-this fst-that)
                    (loop (cdr this-procs/ctcs) (cdr that-procs/ctcs)))]
              [(and (procedure? fst-this) (procedure? fst-that))
               (and (procedure-closure-contents-eq? fst-this fst-that)
                    (loop (cdr this-procs/ctcs) (cdr that-procs/ctcs)))]
              [else #f])]
           [else #f]))))
         
(define-struct struct/dc (apply-proj procs/ctcs pred struct-name here)
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name struct/dc-name
     #:first-order struct/dc-first-order
     #:projection struct/dc-proj
     #:stronger struct/dc-stronger?)))

(define-for-syntax (get-struct-info id stx)
  (define inf (syntax-local-value id (λ () #f)))
  (unless (struct-info? inf)
    (raise-syntax-error 'struct/dc "expected a struct" stx id))
  (define the-info (extract-struct-info inf))
  (unless (list-ref the-info 2)
    (raise-syntax-error 'struct/dc 
                        "expected a struct with a known predicate"
                        stx id))
  the-info)
      
(define-values (struct/dc-imp-prop-desc
                struct/dc-imp-prop-pred?
                struct/dc-imp-prop-get)
  (make-impersonator-property 'struct/dc))


(define-for-syntax (clause->chap-proc struct-id info stx clause-stx)
  (define sel-id (syntax-case clause-stx ()
                   [(sel-id . rest) #'sel-id]))
  (define (add-prefix id)
    (datum->syntax id
                   (string->symbol (format "~a-~a" 
                                           (syntax-e sel-id)
                                           (syntax-e id)))))
  (define immutable-field
    (for/or ([mutator (in-list (list-ref info 4))]
             [selector (in-list (list-ref info 3))])
      (cond
        [(and (not mutator) (not selector))
         ;; end, with some hidden info
         ;; just assume not immutable
         #f]
        [else
         (and (not mutator)
              (let ([id (id->sel-id struct-id sel-id)])
                (and (free-identifier=? id selector)
                     id)))])))
  (define (add-immutable-check ctc-id stx)
    (if immutable-field
        (list stx
              #`(check-chaperone-contract '#,immutable-field #,ctc-id))
        (list stx)))

  (syntax-case clause-stx ()
    ;; with caching
    [(sel-id #:lazy (id ...) exp)
     (with-syntax ([(sel-id ...) (map (λ (x) (id->sel-id struct-id x)) (syntax->list #'(id ...)))])
       (with-syntax ([dep-proc (add-prefix #'dep-proc)])
         #`(((define dep-proc (λ (id ...) #,(defeat-inlining #'exp))))
            (begin)
            (begin)
            (begin)
            (let ([cached unique])
              (λ (strct fld)
                (if (eq? cached unique)
                    (begin
                      (set! cached (un-dep (dep-proc (sel-id strct) ...) fld blame '#,immutable-field))
                      cached)
                    cached))))))]
    [(sel-id (id ...) exp)
     (with-syntax ([(sel-proc-id ...) (map (λ (x) (id->sel-id struct-id x)) (syntax->list #'(id ...)))])
       (with-syntax ([dep-proc (add-prefix #'dep-proc)])
         #`(((define dep-proc (λ (id ...) #,(defeat-inlining #'exp))))
            (begin)
            (begin)
            (un-dep (dep-proc (sel-proc-id v) ...) (#,(id->sel-id struct-id #'sel-id) v) blame '#,immutable-field)
            (λ (strct fld)
              (un-dep (dep-proc (sel-proc-id strct) ...) fld blame '#,immutable-field)))))]
    [(sel-id #:lazy exp)
     (with-syntax ([ctc (add-prefix #'ctc)]
                   [blame-to-proj (add-prefix #'blame-to-proj)]
                   [proj (add-prefix #'proj)])
       #`(#,(add-immutable-check #'ctc #'(define ctc (coerce-contract 'struct/dc exp)))
          (define blame-to-proj (contract-struct-projection ctc))
          (define proj (blame-to-proj blame))
          (begin)
          (let ([cached unique])
            (λ (strct fld)
              (if (eq? cached unique)
                  (begin
                    (set! cached (proj fld))
                    cached)
                  cached)))))]
    [(sel-id exp)
     (with-syntax ([ctc (add-prefix #'ctc)]
                   [blame-to-proj (add-prefix #'blame-to-proj)]
                   [proj (add-prefix #'proj)])
       #`(#,(add-immutable-check #'ctc #'(define ctc (coerce-contract 'struct/dc exp)))
          (define blame-to-proj (contract-struct-projection ctc))
          (define proj (blame-to-proj blame))
          (proj (#,(id->sel-id struct-id #'sel-id) v))
          (if (flat-contract? ctc)
              (λ (strct fld) fld)
              (λ (strct fld) (proj fld)))))]
    [_ (raise-syntax-error #f "malformed clause" stx clause-stx)]))

(define (check-chaperone-contract immutable-field ctc)
  (unless (chaperone-contract? ctc)
    (error 'struct/dc "expected a chaperone contract for the immutable field ~a, got ~e" 
           immutable-field
           ctc)))

(define-for-syntax (id->sel-id struct-id id) 
  (datum->syntax
   id
   (string->symbol
    (format "~a-~a" 
            (syntax-e struct-id)
            (syntax-e id)))))

(define-syntax (-struct/dc stx)
  (syntax-case stx ()
    [(_ struct-id clause ...)
     (let ()
       (define info (get-struct-info #'struct-id stx))
       (with-syntax ([(((before-ctc-bound ...) after-ctc-bound after-blame-bound first-order-check chap-proc) ...)
                      (for/list ([clause (in-list (syntax->list #'(clause ...)))])
                        (clause->chap-proc #'struct-id info stx clause))])
         (with-syntax ([(id ...) (syntax-case #'((before-ctc-bound ...) ...) ()
                                   [(((define id exp) . whatever) ...) #'(id ...)])]
                       [(selectors+chap-procs ...)
                        (apply
                         append
                         (for/list ([clause (in-list (syntax->list #'(clause ...)))]
                                    [chap-proc (in-list (syntax->list #'(chap-proc ...)))])
                           (list (id->sel-id
                                  #'struct-id
                                  (syntax-case clause ()
                                    [(x . rest) #'x]))
                                 chap-proc)))])
           #`(let ()
               before-ctc-bound ... ...
               (letrec ([me
                         (make-struct/dc
                          (λ (ctc)
                            after-ctc-bound ...
                            (λ (blame)
                              after-blame-bound ...
                              (λ (v)
                                first-order-check ...
                                (chaperone-struct
                                 v
                                 selectors+chap-procs ...
                                 struct/dc-imp-prop-desc
                                 me))))
                          (list id ...)
                          #,(list-ref info 2)
                          'struct-id
                          (quote-module-name))])
                 me)))))]))

(define/opter (-struct/dc opt/i opt/info stx)
  (syntax-case stx ()
    [(_ struct-id clause ...)
     (let ()
       (define info (get-struct-info #'struct-id stx))
       (cond
         [(ormap values (list-ref info 4))
          ;; any mutable struct, just give up (could generate impersonator code, but
          ;; would have to check that the compiled subcontracts are all chaperones/flats)
          (opt/unknown opt/i opt/info stx)]
         [else
          (define-values (s-chap-code s-flat-code s-lifts s-super-lifts s-partially-applied can-be-optimized? stronger-ribs chaperone?)
            (for/fold ([s-chap-code '()]
                       [s-flat-code '()]
                       [s-lifts '()]
                       [s-super-lifts '()]
                       [s-partially-applied '()]
                       [can-be-optimized? #t]
                       [stronger-ribs '()]
                       [chaperone? #t])
              ([clause (in-list (syntax->list #'(clause ...)))])
              
              (define-values (sel-id lazy? dep-vars exp)
                (syntax-case clause ()
                  [(sel-id #:lazy exp) (values #'sel-id #t #f #'exp)]
                  [(sel-id exp) (values #'sel-id #f #f #'exp)]
                  [(sel-id #:lazy (dep-id ...) exp) (values #'sel-id #t #'(dep-id ...) #'exp)]
                  [(sel-id (dep-id ...) exp) (values #'sel-id #f #'(dep-id ...) #'exp)]))
              
              (define-values (this-code 
                              this-lifts this-super-lifts this-partially-applied 
                              this-flat? this-can-be-optimized? this-stronger-ribs
                              this-chaperone?)
                (opt/i opt/info exp))
              
              (values (cond
                        [(and this-flat? (not lazy?) (not dep-vars))
                         s-chap-code]
                        [else
                         (with-syntax ([(strct cache) (generate-temporaries '(struct cache))]
                                       [proc-name (string->symbol
                                                   (format "~a-~a-chap/dep" 
                                                           (syntax-e #'struct-id)
                                                           (syntax-e sel-id)))])
                           (list* (cond
                                    [dep-vars
                                     (with-syntax ([(sel ...) (map (λ (var) (id->sel-id #'struct-id var)) 
                                                                   (syntax->list dep-vars))]
                                                   [(dep-var ...) dep-vars])
                                       (with-syntax ([this-code+lifts
                                                      #`(let ([dep-var (sel strct)] ...)
                                                          #,(bind-superlifts
                                                             this-super-lifts
                                                             (bind-lifts
                                                              this-lifts
                                                              (bind-lifts
                                                               this-partially-applied
                                                               this-code))))])
                                         (if lazy?
                                             #`(let ([cache unique])
                                                 (let ([proc-name
                                                        (λ (strct #,(opt/info-val opt/info)) 
                                                          (cond
                                                            [(eq? cache unique)
                                                             (set! cache this-code+lifts)
                                                             cache]
                                                            [else cache]))])
                                                   proc-name))
                                             #`(let ([proc-name
                                                      (λ (strct #,(opt/info-val opt/info)) 
                                                        this-code+lifts)])
                                                 proc-name))))]
                                    [else
                                     (if lazy?
                                         #`(let ([cache unique])
                                             (let ([proc-name
                                                    (λ (strct #,(opt/info-val opt/info))
                                                      (cond
                                                        [(eq? cache unique)
                                                         (set! cache #,this-code)
                                                         cache]
                                                        [else cache]))])
                                               proc-name))
                                         #`(let ([proc-name
                                                  (λ (strct #,(opt/info-val opt/info))
                                                    #,this-code)])
                                             proc-name))])
                                  (id->sel-id #'struct-id sel-id)
                                  s-chap-code))])
                      (cond
                        [lazy?
                         s-flat-code]
                        [dep-vars
                         (with-syntax ([(sel ...) (map (λ (var) (id->sel-id #'struct-id var)) 
                                                       (syntax->list dep-vars))]
                                       [(dep-var ...) dep-vars])
                           (cons #` (let ([dep-var (sel #,(opt/info-val opt/info))] ...)
                                      (let ([#,(opt/info-val opt/info) (#,(id->sel-id #'struct-id sel-id)
                                                                        #,(opt/info-val opt/info))])
                                        #,this-code))
                                 s-flat-code))]
                        [else
                         (cons #`(let ([#,(opt/info-val opt/info) (#,(id->sel-id #'struct-id sel-id)
                                                                   #,(opt/info-val opt/info))])
                                   #,this-code)
                               s-flat-code)])
                      (if dep-vars s-lifts (append this-lifts s-lifts))
                      (if dep-vars s-super-lifts (append this-super-lifts s-super-lifts))
                      (if dep-vars s-partially-applied (append this-partially-applied s-partially-applied))
                      (and this-can-be-optimized? can-be-optimized?)
                      (append this-stronger-ribs stronger-ribs)
                      (and this-chaperone? chaperone?))))
          (with-syntax ([(stronger-prop-desc stronger-prop-pred? stronger-prop-get)
                         (syntax-local-lift-values-expression
                          3
                          #'(make-impersonator-property 'struct/dc-stronger-prop))]
                        [(free-var ...) (opt/info-free-vars opt/info)]
                        [(index ...) (build-list (length (opt/info-free-vars opt/info)) values)]
                        [pred? (list-ref info 2)])
            (values #`(if (and (stronger-prop-pred? #,(opt/info-val opt/info))
                               (let ([v (stronger-prop-get #,(opt/info-val opt/info))])
                                 (and (eq? (vector-ref v index) free-var) ...)))
                          #,(opt/info-val opt/info)
                          (if (pred? #,(opt/info-val opt/info))
                              (begin
                                #,@(reverse s-flat-code) ;; built the last backwards, so reverse it here
                                (chaperone-struct
                                 #,(opt/info-val opt/info)
                                 #,@(reverse s-chap-code) ;; built the last backwards, so reverse it here
                                 stronger-prop-desc
                                 (vector free-var ...)))
                              (struct/dc-error blame #,(opt/info-val opt/info) 'struct-name)))
                    s-lifts
                    s-super-lifts
                    s-partially-applied
                    #f  ;; flat sexp
                    can-be-optimized?
                    stronger-ribs
                    #t  ;;chaperone?
                    ))]))]))

(define (struct/dc-error blame obj what)
  (raise-blame-error blame obj 
                     "expected a struct of type ~a"
                     what))
