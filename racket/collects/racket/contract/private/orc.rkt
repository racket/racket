#lang racket/base
(require "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "rand.rkt"
         "generate.rkt"
         "misc.rkt"
         (for-syntax racket/base))

(provide symbols or/c one-of/c
         blame-add-or-context
         (rename-out [_flat-rec-contract flat-rec-contract]))

(define/subexpression-pos-prop or/c
  (case-lambda 
    [() (make-none/c '(or/c))]
    [(x) (coerce-contract 'or/c x)]
    [raw-args
     (define args (coerce-contracts 'or/c raw-args))
     (define-values (ho-contracts flat-contracts)
       (let loop ([ho-contracts '()]
                  [flat-contracts '()]
                  [args args])
         (cond
           [(null? args) (values ho-contracts (reverse flat-contracts))]
           [else 
            (let ([arg (car args)])
              (cond
                [(flat-contract? arg)
                 (loop ho-contracts (cons arg flat-contracts) (cdr args))]
                [else
                 (loop (cons arg ho-contracts) flat-contracts (cdr args))]))])))
     (define pred 
       (cond
         [(null? flat-contracts) not]
         [else
          (let loop ([fst (car flat-contracts)]
                     [rst (cdr flat-contracts)])
            (let ([fst-pred (flat-contract-predicate fst)])
              (cond
                [(null? rst) fst-pred]
                [else 
                 (let ([r (loop (car rst) (cdr rst))])
                   (λ (x) (or (fst-pred x) (r x))))])))]))
     
     (cond
       [(null? ho-contracts)
        (make-flat-or/c pred flat-contracts)]
       [(null? (cdr ho-contracts))
        (define name (apply build-compound-type-name 'or/c args))
        (if (chaperone-contract? (car ho-contracts))
            (make-chaperone-single-or/c name pred flat-contracts (car ho-contracts))
            (make-impersonator-single-or/c name pred flat-contracts (car ho-contracts)))]
       [else
        (define name (apply build-compound-type-name 'or/c args))
        (if (andmap chaperone-contract? ho-contracts)
            (make-chaperone-multi-or/c name flat-contracts ho-contracts)
            (make-impersonator-multi-or/c name flat-contracts ho-contracts))])]))

(define (single-or/c-projection ctc)
  (let ([c-proc (contract-projection (single-or/c-ho-ctc ctc))]
        [pred (single-or/c-pred ctc)])
    (λ (blame)
      (define partial-contract
        (c-proc (blame-add-or-context blame)))
      (λ (val)
        (cond
          [(pred val) val]
          [else (partial-contract val)])))))

(define (single-or/c-late-neg-projection ctc)
  (define c-proj (get/build-late-neg-projection (single-or/c-ho-ctc ctc)))
  (define pred (single-or/c-pred ctc))
  (λ (blame)
    (define p-app (c-proj (blame-add-or-context blame)))
    (λ (val neg-party)
      (if (pred val)
          val
          (p-app val neg-party)))))

(define (blame-add-or-context blame)
  (blame-add-context blame "a part of the or/c of"))

(define (single-or/c-first-order ctc)
  (let ([pred (single-or/c-pred ctc)]
        [ho (contract-first-order (single-or/c-ho-ctc ctc))])
    (λ (x) (or (ho x) (pred x)))))

(define (single-or/c-stronger? this that)
  (or (and (single-or/c? that)
           (contract-stronger? (single-or/c-ho-ctc this)
                               (single-or/c-ho-ctc that))
           (let ([this-ctcs (single-or/c-flat-ctcs this)]
                 [that-ctcs (single-or/c-flat-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger?
                          this-ctcs
                          that-ctcs))))
      (generic-or/c-stronger? this that)))

(define (generic-or/c-stronger? this that)
  (define this-sub-ctcs (or/c-sub-contracts this))
  (define that-sub-ctcs (or/c-sub-contracts that))
  (and this-sub-ctcs
       that-sub-ctcs
       (for/and ([this-sub-ctc (in-list this-sub-ctcs)])
         (for/or ([that-sub-ctc (in-list that-sub-ctcs)])
           (contract-stronger? this-sub-ctc that-sub-ctc)))))

(define (or/c-sub-contracts ctc)
  (cond
    [(single-or/c? ctc)
     (cons (single-or/c-ho-ctc ctc)
           (single-or/c-flat-ctcs ctc))]
    [(multi-or/c? ctc)
     (append (multi-or/c-flat-ctcs ctc)
             (multi-or/c-ho-ctcs ctc))]
    [(flat-or/c? ctc)
     (flat-or/c-flat-ctcs ctc)]
    [else #f]))

(define (or/c-exercise ho-contracts)
  (λ (fuel)
    (define env (contract-random-generate-get-current-environment))
    (values (λ (val)
              (let loop ([ho-contracts ho-contracts])
                (unless (null? ho-contracts)
                  (define ctc (car ho-contracts))
                  (cond
                    [((contract-first-order ctc) val)
                     (define-values (exercise ctcs) ((contract-struct-exercise ctc) fuel))
                     (exercise val)
                     (contract-random-generate-stash env ctc val)]
                    [else
                     (loop (cdr ho-contracts))]))))
            '())))

(define ((or/c-generate or/c-ctc ctcs) fuel)
  (define directs 
    (filter
     values
     (for/list ([ctc (in-list ctcs)])
       ((contract-struct-generate ctc) fuel))))
  (define can-generate?
    (or (pair? directs)
        (for/or ([ctc (in-list ctcs)])
          (can-generate/env? ctc))))
  (cond
    [can-generate?
     ;; #f => try to use the entire or/c contract in the environment
     (define options (cons #f (append 
                               (map (λ (x) (cons 'direct x)) directs)
                               (map (λ (x) (cons 'env x)) ctcs))))
     (define env (contract-random-generate-get-current-environment))
     (λ ()
       (let loop ([options (permute options)])
         (cond
           [(null? options) contract-random-generate-fail]
           [else
            (define option (car options))
            (cond
              [(not option)
               (define candidate (try/env or/c-ctc env))
               (cond
                 [(contract-random-generate-fail? candidate)
                  (loop (cdr options))]
                 [else
                  candidate])]
              [(equal? (car option) 'env)
               (define candidate (try/env (cdr option) env))
               (cond
                 [(contract-random-generate-fail? candidate)
                  (loop (cdr options))]
                 [else
                  candidate])]
              [(equal? (car option) 'direct)
               (define-values (succ? val)
                 (let/ec k
                   (parameterize ([fail-escape (λ () (k #f #f))])
                     (k #t ((cdr option))))))
               (if (and succ? (not (contract-random-generate-fail? val)))
                   val
                   (loop (cdr options)))]
              [else (error 'racket/contract/orc.rkt "ack ~s" options)])])))]
    [else #f]))

(define (single-or/c-list-contract? c)
  (and (list-contract? (single-or/c-ho-ctc c))
       (for/and ([c (in-list (single-or/c-flat-ctcs c))])
         (list-contract? c))))

(define-struct single-or/c (name pred flat-ctcs ho-ctc)
  #:property prop:orc-contract
  (λ (this) (cons (single-or/c-ho-ctc this) 
                  (single-or/c-flat-ctcs this))))

(define-struct (chaperone-single-or/c single-or/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection single-or/c-projection
     #:late-neg-projection single-or/c-late-neg-projection
     #:name single-or/c-name
     #:first-order single-or/c-first-order
     #:stronger single-or/c-stronger?
     #:generate (λ (ctc) (or/c-generate ctc
                                        (cons (single-or/c-ho-ctc ctc)
                                              (single-or/c-flat-ctcs ctc))))
     #:exercise (λ (ctc) (or/c-exercise (list (single-or/c-ho-ctc ctc))))
     #:list-contract? single-or/c-list-contract?)))

(define-struct (impersonator-single-or/c single-or/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:projection single-or/c-projection
   #:late-neg-projection single-or/c-late-neg-projection
   #:name single-or/c-name
   #:first-order single-or/c-first-order
   #:stronger single-or/c-stronger?
   #:generate (λ (ctc) (or/c-generate ctc
                                      (cons (single-or/c-ho-ctc ctc)
                                            (single-or/c-flat-ctcs ctc))))
   #:exercise (λ (ctc) (or/c-exercise (list (single-or/c-ho-ctc ctc))))
   #:list-contract? single-or/c-list-contract?))

(define (multi-or/c-proj ctc)
  (let* ([ho-contracts (multi-or/c-ho-ctcs ctc)]
         [c-procs (map (λ (x) (contract-projection x)) ho-contracts)]
         [first-order-checks (map (λ (x) (contract-first-order x)) ho-contracts)]
         [predicates (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))])
    (λ (blame)
      (define disj-blame (blame-add-context blame "a part of the or/c of"))
      (define partial-contracts
        (for/list ([c-proc (in-list c-procs)])
          (c-proc disj-blame)))
      (λ (val)
        (cond
          [(ormap (λ (pred) (pred val)) predicates)
           val]
          [else
           (let loop ([checks first-order-checks]
                      [procs partial-contracts]
                      [contracts ho-contracts]
                      [candidate-proc #f]
                      [candidate-contract #f])
             (cond
               [(null? checks)
                (if candidate-proc
                    (candidate-proc val)
                    (raise-blame-error blame val 
                                       '("none of the branches of the or/c matched" given: "~e")
                                       val))]
               [((car checks) val)
                (if candidate-proc
                    (raise-blame-error blame val
                                       '("two of the clauses in the or/c might both match: ~s and ~s"
                                         given:
                                         "~e")
                                       (contract-name candidate-contract)
                                       (contract-name (car contracts))
                                       val)
                    (loop (cdr checks)
                          (cdr procs)
                          (cdr contracts)
                          (car procs)
                          (car contracts)))]
               [else
                (loop (cdr checks)
                      (cdr procs)
                      (cdr contracts)
                      candidate-proc
                      candidate-contract)]))])))))

(define (multi-or/c-late-neg-proj ctc)  
  (define ho-contracts (multi-or/c-ho-ctcs ctc))
  (define c-projs (map get/build-late-neg-projection ho-contracts))
  (define first-order-checks (map (λ (x) (contract-first-order x)) ho-contracts))
  (define predicates (map flat-contract-predicate (multi-or/c-flat-ctcs ctc)))
  (λ (blame)
    (define blame-w-context (blame-add-or-context blame))
    (define c-projs+blame (map (λ (c-proj) (c-proj blame-w-context)) c-projs))
    (λ (val neg-party)
      (cond
        [(for/or ([pred (in-list predicates)])
           (pred val))
         val]
        [else
         (let loop ([checks first-order-checks]
                    [c-projs c-projs+blame]
                    [contracts ho-contracts]
                    [candidate-c-proj #f]
                    [candidate-contract #f])
           (cond
             [(null? checks)
              (cond
                [candidate-c-proj
                 (candidate-c-proj val neg-party)]
                [else
                 (raise-blame-error blame val #:missing-party neg-party
                                    '("none of the branches of the or/c matched" given: "~e")
                                    val)])]
             [((car checks) val)
              (if candidate-c-proj
                  (raise-blame-error blame val #:missing-party neg-party
                                     '("two of the clauses in the or/c might both match: ~s and ~s"
                                       given:
                                       "~e")
                                     (contract-name candidate-contract)
                                     (contract-name (car contracts))
                                     val)
                  (loop (cdr checks)
                        (cdr c-projs)
                        (cdr contracts)
                        (car c-projs)
                        (car contracts)))]
             [else
              (loop (cdr checks)
                    (cdr c-projs)
                    (cdr contracts)
                    candidate-c-proj
                    candidate-contract)]))]))))

(define (multi-or/c-first-order ctc)
  (let ([flats (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))]
        [hos (map (λ (x) (contract-first-order x)) (multi-or/c-ho-ctcs ctc))])
    (λ (x)
      (or (ormap (λ (f) (f x)) hos)
          (ormap (λ (f) (f x)) flats)))))

(define (multi-or/c-stronger? this that)
  (or (and (multi-or/c? that)
           (let ([this-ctcs (multi-or/c-ho-ctcs this)]
                 [that-ctcs (multi-or/c-ho-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger? this-ctcs that-ctcs)))
           (let ([this-ctcs (multi-or/c-flat-ctcs this)]
                 [that-ctcs (multi-or/c-flat-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger? this-ctcs that-ctcs))))
      (generic-or/c-stronger? this that)))

(define (mult-or/c-list-contract? c)
  (and (for/and ([c (in-list (multi-or/c-flat-ctcs c))])
         (list-contract? c))
       (for/and ([c (in-list (multi-or/c-ho-ctcs c))])
         (list-contract? c))))

(define-struct multi-or/c (name flat-ctcs ho-ctcs)
  #:property prop:orc-contract
  (λ (this) (append (multi-or/c-ho-ctcs this) 
                    (multi-or/c-flat-ctcs this))))

(define-struct (chaperone-multi-or/c multi-or/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection multi-or/c-proj
     #:late-neg-projection multi-or/c-late-neg-proj
     #:name multi-or/c-name
     #:first-order multi-or/c-first-order
     #:stronger multi-or/c-stronger?
     #:generate (λ (ctc) (or/c-generate ctc
                                        (append (multi-or/c-ho-ctcs ctc)
                                                (multi-or/c-flat-ctcs ctc))))
     #:exercise (λ (ctc) (or/c-exercise (multi-or/c-ho-ctcs ctc)))
     #:list-contract? mult-or/c-list-contract?)))

(define-struct (impersonator-multi-or/c multi-or/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:projection multi-or/c-proj
   #:late-neg-projection multi-or/c-late-neg-proj
   #:name multi-or/c-name
   #:first-order multi-or/c-first-order
   #:stronger multi-or/c-stronger?
   #:generate (λ (ctc) (or/c-generate ctc
                                      (append (multi-or/c-ho-ctcs ctc)
                                              (multi-or/c-flat-ctcs ctc))))
   #:exercise (λ (ctc) (or/c-exercise (multi-or/c-ho-ctcs ctc)))
   #:list-contract? mult-or/c-list-contract?))

(define-struct flat-or/c (pred flat-ctcs)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:orc-contract
  (λ (this) (flat-or/c-flat-ctcs this))
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
      (apply build-compound-type-name 
             'or/c 
             (flat-or/c-flat-ctcs ctc)))
   #:stronger
   (λ (this that)
     (or (and (flat-or/c? that)
              (let ([this-ctcs (flat-or/c-flat-ctcs this)]
                    [that-ctcs (flat-or/c-flat-ctcs that)])
                (cond
                  [(and (<= (length this-ctcs) (length that-ctcs))
                        (for/and ([this-ctc (in-list this-ctcs)]
                                  [that-ctc (in-list that-ctcs)])
                          (contract-stronger? this-ctc that-ctc)))
                   #t]
                  [(and (andmap (λ (x) (or (eq-contract? x) (equal-contract? x))) this-ctcs)
                        (andmap (λ (x) (or (eq-contract? x) (equal-contract? x))) that-ctcs))
                   (define ht (make-hash))
                   (for ([x (in-list that-ctcs)])
                     (hash-set! ht
                                (if (equal-contract? x)
                                    (equal-contract-val x)
                                    (eq-contract-val x))
                                #t))
                   (for/and ([x (in-list this-ctcs)])
                     (hash-ref ht 
                               (if (equal-contract? x)
                                   (equal-contract-val x)
                                   (eq-contract-val x))
                               #f))]
                  [else #f])))
         (generic-or/c-stronger? this that)))
                    

   #:first-order
   (λ (ctc) (flat-or/c-pred ctc))
   #:generate (λ (ctc) (or/c-generate ctc (flat-or/c-flat-ctcs ctc)))
   #:list-contract? 
   (λ (ctc)
     (for/and ([c (in-list (flat-or/c-flat-ctcs ctc))])
       (list-contract? c)))))


(define/final-prop (symbols s1 . s2s)
  (define ss (cons s1 s2s))
  (for ([arg (in-list ss)]
        [i (in-naturals)])
    (unless (symbol? arg)
      (raise-argument-error 'symbols
                            "symbol?"
                            i
                            ss)))
  (apply or/c ss))



(define/final-prop (one-of/c . elems)
  (for ([arg (in-list elems)]
        [i (in-naturals)])
    (unless (atomic-value? arg)
      (raise-argument-error 'one-of/c
                            "char, symbol, boolean, null, keyword, number, or void"
                            i
                            elems)))
  (define or/c-args
    (map (λ (x)
           (cond
             [(void? x) void?]
             [else x]))
         elems))
  (apply or/c or/c-args))

(define atomic-value? 
  (λ (x)
     (or (char? x) (symbol? x) (boolean? x)
         (null? x) (keyword? x) (number? x)
         (void? x))))

(define (get-flat-rec-me ctc)
  (define ans (flat-rec-contract-me ctc))
  (unless ans (error 'flat-rec-contract "attempted to access the contract too early"))
  ans)

(struct flat-rec-contract ([me #:mutable] name)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc) (flat-rec-contract-name ctc))
   #:stronger
   (let ([recur? (make-parameter #t)])
     (λ (this that) 
       (cond
         [(equal? this that) #t]
         [(recur?) 
          (parameterize ([recur? #f])
            (contract-stronger? (get-flat-rec-me this) that))]
         [else #f])))
   #:first-order
   (λ (ctc) 
     (λ (v)
       ((contract-first-order (get-flat-rec-me ctc)) v)))
   #:generate 
   (λ (ctc) 
     (λ (fuel)
       (if (zero? fuel)
           #f
           (contract-random-generate/choose (get-flat-rec-me ctc) (- fuel 1)))))))

(define-syntax (_flat-rec-contract stx)
  (syntax-case stx  ()
    [(_ name ctc ...)
     (identifier? (syntax name))
     (syntax
      (let ([name (flat-rec-contract #f 'name)])
        (set-flat-rec-contract-me!
         name
         (or/c (coerce-flat-contract 'flat-rec-contract ctc) 
               ...))
        name))]
    [(_ name ctc ...)
     (raise-syntax-error 'flat-rec-contract
                         "expected first argument to be an identifier"
                         stx
                         (syntax name))]))
(define (flat-rec-contract/init x) 
  (error 'flat-rec-contract "applied too soon"))
