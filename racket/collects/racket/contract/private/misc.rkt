#lang racket/base

(require (for-syntax racket/base)
         racket/promise
         (only-in "../../private/promise.rkt" prop:force promise-forcer)
         "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "rand.rkt"
         "generate.rkt"
         "generate-base.rkt")

(provide flat-murec-contract
         and/c
         not/c
         =/c >=/c <=/c </c >/c between/c
         integer-in
         real-in
         natural-number/c
         string-len/c
         false/c
         printable/c
         listof non-empty-listof cons/c list/c
         promise/c
         syntax/c
         
         check-between/c
         check-unary-between/c
         parameter/c
         procedure-arity-includes/c
         
         any/c
         any
         none/c
         make-none/c

         prompt-tag/c
         continuation-mark-key/c

         channel/c
         evt/c

         chaperone-contract?
         impersonator-contract?
         flat-contract?
         contract?
         
         flat-contract
         flat-contract-predicate
         flat-named-contract
         
         contract-projection
         contract-val-first-projection  ;; might return #f (if none)
         get/build-val-first-projection ;; builds one if necc., using contract-projection
         contract-name
         n->th
         
         blame-add-car-context
         blame-add-cdr-context
         raise-not-cons-blame-error
         
         random-any/c)

(define-syntax (flat-murec-contract stx)
  (syntax-case stx  ()
    [(_ ([name ctc ...] ...) body1 body ...)
     (andmap identifier? (syntax->list (syntax (name ...))))
     (with-syntax ([((ctc-id ...) ...) (map generate-temporaries
                                            (syntax->list (syntax ((ctc ...) ...))))]
                   [(pred-id ...) (generate-temporaries (syntax (name ...)))]
                   [((pred-arm-id ...) ...) (map generate-temporaries
                                                 (syntax->list (syntax ((ctc ...) ...))))])
       (syntax 
        (let* ([pred-id flat-murec-contract/init] ...
               [name (flat-contract (let ([name (λ (x) (pred-id x))]) name))] ...)
          (let-values ([(ctc-id ...) (values (coerce-flat-contract 'flat-rec-contract ctc) ...)] ...)
            (set! pred-id
                  (let ([pred-arm-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-arm-id x) ...)))) ...
            body1
            body ...))))]
    [(_ ([name ctc ...] ...) body1 body ...)
     (for-each (λ (name)
                 (unless (identifier? name)
                   (raise-syntax-error 'flat-rec-contract
                                       "expected an identifier" stx name)))
               (syntax->list (syntax (name ...))))]
    [(_ ([name ctc ...] ...))
     (raise-syntax-error 'flat-rec-contract "expected at least one body expression" stx)]))

(define (flat-murec-contract/init x) (error 'flat-murec-contract "applied too soon"))


(define (and-name ctc)
  (apply build-compound-type-name 'and/c (base-and/c-ctcs ctc)))

(define (and-first-order ctc)
  (let ([tests (map contract-first-order (base-and/c-ctcs ctc))])
    (λ (x) (for/and ([test (in-list tests)]) (test x)))))

(define (and-proj ctc)
  (let ([mk-pos-projs (map contract-projection (base-and/c-ctcs ctc))])
    (lambda (blame)
      (define projs 
        (for/list ([c (in-list mk-pos-projs)]
                   [n (in-naturals 1)])
          (c (blame-add-context blame (format "the ~a conjunct of" (n->th n))))))
      (for/fold ([proj (car projs)])
        ([p (in-list (cdr projs))])
        (λ (v) (p (proj v)))))))

(define (val-first-and-proj ctc)
  (define mk-pos-projs (map get/build-val-first-projection (base-and/c-ctcs ctc)))
  (λ (blame)
    (define projs 
      (for/list ([c (in-list mk-pos-projs)]
                 [n (in-naturals 1)])
        (c (blame-add-context blame (format "the ~a conjunct of" (n->th n))))))
    (λ (val)
      (λ (neg-party)
        (let loop ([projs (cdr projs)]
                   [val (((car projs) val) neg-party)])
          (cond
            [(null? projs) val]
            [else
             (loop (cdr projs)
                   (((car projs) val) neg-party))]))))))

(define (first-order-and-proj ctc)
  (λ (blame)
    (λ (val)
      (let loop ([predicates (first-order-and/c-predicates ctc)]
                 [ctcs (base-and/c-ctcs ctc)])
        (cond
          [(null? predicates) val]
          [else
           (if ((car predicates) val)
               (loop (cdr predicates) (cdr ctcs))
               (raise-blame-error 
                blame
                val
                '(expected: "~s" given: "~e\n  which isn't: ~s")
                (contract-name ctc)
                val
                (contract-name (car ctcs))))])))))

(define (first-order-val-first-and-proj ctc)
  (define predicates (first-order-and/c-predicates ctc))
  (define ctcs (base-and/c-ctcs ctc))
  (λ (blame)
    (λ (val)
      (let loop ([predicates predicates]
                 [ctcs ctcs])
        (cond
          [(null? predicates) (λ (neg-party) val)]
          [else
           (if ((car predicates) val)
               (loop (cdr predicates) (cdr ctcs))
               (λ (neg-party)
                 (raise-blame-error
                  blame  #:missing-party neg-party
                  val
                  '(expected: "~s" given: "~e\n  which isn't: ~s")
                  (contract-name ctc)
                  val
                  (contract-name (car ctcs)))))])))))

(define (and-stronger? this that)
  (and (base-and/c? that)
       (let ([this-ctcs (base-and/c-ctcs this)]
             [that-ctcs (base-and/c-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger?
                      this-ctcs
                      that-ctcs)))))

(define (and/c-generate? ctc)
  (cond
    [(and/c-check-nonneg ctc real?) => values]
    [(and/c-check-nonneg ctc rational?) => values]
    [else (λ (fuel) #f)]))

(define (and/c-check-nonneg ctc pred)
  (define sub-contracts (base-and/c-ctcs ctc))
  (cond
    [(are-stronger-contracts? (list pred (not/c negative?))
                              sub-contracts)
     (define go (hash-ref predicate-generator-table pred))
     (λ (fuel)
       (λ ()
         (abs (go fuel))))]
    [else #f]))

(define (are-stronger-contracts? c1s c2s)
  (let loop ([c1s c1s]
             [c2s c2s])
    (cond
      [(and (null? c1s) (null? c2s)) #t]
      [(and (pair? c1s) (pair? c2s))
       (and (contract-stronger? (car c1s) (car c2s))
            (loop (cdr c1s) (cdr c2s)))]
      [else #f])))

(define-struct base-and/c (ctcs))
(define-struct (first-order-and/c base-and/c) (predicates)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection first-order-and-proj
   #:val-first-projection first-order-val-first-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))
(define-struct (chaperone-and/c base-and/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection and-proj
     #:val-first-projection val-first-and-proj
     #:name and-name
     #:first-order and-first-order
     #:stronger and-stronger?
     #:generate and/c-generate?)))
(define-struct (impersonator-and/c base-and/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:projection and-proj
   #:val-first-projection val-first-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))


(define/subexpression-pos-prop (and/c . raw-fs)
  (let ([contracts (coerce-contracts 'and/c raw-fs)])
    (cond
      [(null? contracts) any/c]
      [(andmap flat-contract? contracts)
       (let ([preds (map flat-contract-predicate contracts)])
         (make-first-order-and/c contracts preds))]
      [(andmap chaperone-contract? contracts)
       (make-chaperone-and/c contracts)]
      [else (make-impersonator-and/c contracts)])))


(define false/c #f)

(define/final-prop (string-len/c n)
  (unless (real? n)
    (raise-argument-error 'string-len/c "real?" n))
  (flat-named-contract 
   `(string-len/c ,n)
   (λ (x)
     (and (string? x)
          ((string-length x) . < . n)))))

(define-struct between/c (low high)
  #:property prop:custom-write custom-write-property-proc
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc) 
      (let ([n (between/c-low ctc)]
            [m (between/c-high ctc)])
        (cond
          [(and (= n -inf.0) (= m +inf.0))
           `(between/c ,n ,m)]
          [(= n -inf.0) `(<=/c ,m)]
          [(= m +inf.0) `(>=/c ,n)]
          [(= n m) `(=/c ,n)]
          [else `(between/c ,n ,m)])))

   #:stronger
   (λ (this that)
      (and (between/c? that)
           (<= (between/c-low that) (between/c-low this))
           (<= (between/c-high this) (between/c-high that))))

   #:first-order
   (λ (ctc) 
      (let ([n (between/c-low ctc)]
            [m (between/c-high ctc)])
        (λ (x) 
           (and (real? x)
                (<= n x m)))))
   #:generate
   (λ (ctc)
      (λ (fuel)
        (define n (between/c-low ctc))
        (define m (between/c-high ctc))
          (cond
            [(= n m)
             (λ () 
               (define choice (rand-choice
                               [1/2 n]
                               [else m]))
               (rand-choice 
                [1/2 (if (exact? choice)
                         (if (= (exact->inexact choice) choice)
                             (exact->inexact choice)
                             choice)
                         (if (= (* 1.0 choice) choice)
                             (* 1.0 choice)
                             choice))]
                [else choice]))]
            [else
             (λ ()
               (rand-choice
                [1/10 (if (<= n 0 m)
                          (rand-choice [1/3 0] [1/3 0.0] [else -0.0])
                          (rand-choice [1/2 n] [else m]))]
                [1/20 (if (<= n 1 m)
                          1
                          (rand-choice [1/2 n] [else m]))]
                [1/20 (if (<= n -1 m)
                          -1
                          (rand-choice [1/2 n] [else m]))]
                [1/10 m]
                [1/10 n]
                [1/10 (if (<= n 0 1 m)
                          (random)
                          (rand-choice [1/2 n] [else m]))]
                [else
                 (cond
                   [(or (= n -inf.0) (= m +inf.0))
                    (define c (random 4294967087))
                    (cond
                      [(and (= n -inf.0) (= m +inf.0)) c]
                      [(= m +inf.0) (+ n c)]
                      [(= n -inf.0) (- m c)])]
                   [else
                    (+ n (* (random) (- m n)))])]))])))))

(define (maybe-neg n) (rand-choice [1/2 n] [else (- n)]))

(define (check-unary-between/c sym x)
  (unless (real? x)
    (raise-argument-error sym "real?" x)))

(define/final-prop (=/c x) 
  (check-unary-between/c '=/c x)
  (make-between/c x x))
(define/final-prop (<=/c x) 
  (check-unary-between/c '<=/c x)
  (make-between/c -inf.0 x))
(define/final-prop (>=/c x)
  (check-unary-between/c '>=/c x)
  (make-between/c x +inf.0))
(define (check-between/c x y)
  (check-two-args 'between/c x y real? real?))
(define/final-prop (between/c x y)
  (check-between/c x y)
  (make-between/c x y))

(define (</c x)
  (flat-named-contract
   `(</c ,x)
   (λ (y) (and (real? y) (< y x)))
   (λ (fuel)
     (λ ()
       (rand-choice
        [1/10 -inf.0]
        [1/10 (- x 0.01)]
        [4/10 (- x (random))]
        [else (- x (random 4294967087))])))))

(define (>/c x)
  (flat-named-contract
    `(>/c ,x)
    (λ (y) (and (real? y) (> y x)))
    (λ (fuel) 
      (λ ()
        (rand-choice
         [1/10 +inf.0]
         [1/10 (+ x 0.01)]
         [4/10 (+ x (random))]
         [else (+ x (random 4294967087))])))))

(define (check-two-args name arg1 arg2 pred1? pred2?)
  (unless (pred1? arg1)
    (raise-argument-error name
                          (format "~a" (object-name pred1?))
                          0
                          arg1 arg2))
  (unless (pred2? arg2)
    (raise-argument-error name
                          (format "~a" (object-name pred2?))
                          1
                          arg1 arg2)))
(define/final-prop (integer-in start end)
  (check-two-args 'integer-in start end exact-integer? exact-integer?)
  (flat-named-contract 
   `(integer-in ,start ,end)
   (λ (x)
     (and (exact-integer? x)
          (<= start x end)))))

(define/final-prop (real-in start end)
  (check-two-args 'real start end real? real?)
  (between/c start end))

(define/final-prop (not/c f)
  (let* ([ctc (coerce-flat-contract 'not/c f)]
         [pred (flat-contract-predicate ctc)])
    (flat-named-contract
     (build-compound-type-name 'not/c ctc)
     (λ (x) (not (pred x))))))

(define (listof-generate elem-ctc)
  (λ (fuel)
    (define eg (generate/choose elem-ctc fuel))
    (if eg
        (λ ()
          (let loop ([so-far '()])
            (rand-choice
             [1/5 so-far]
             [else (loop (cons (eg) so-far))])))
        (λ () '()))))

(define (non-empty-listof-generate elem-ctc)
  (λ (fuel)
    (define eg (generate/choose elem-ctc fuel))
    (if eg
        (λ ()
          (let loop ([so-far (list (eg))])
            (rand-choice
             [1/5 so-far]
             [else (loop (cons (eg) so-far))])))
        #f)))

(define (non-empty-listof-exercise elem-ctc)
  (λ (fuel)
    (define env (generate-env))
    (values
     (λ (lst)
       (env-stash env elem-ctc (oneof lst)))
     (list elem-ctc))))

(define (*-listof predicate? name generate exercise)
  (λ (input)
    (define ctc (coerce-contract name input))
    (define ctc-name (build-compound-type-name name ctc))
    (define proj (contract-projection ctc))
    (define ((listof-*-ho-check check-all) blame)
      (let ([p-app (proj (blame-add-listof-*-context blame))])
        (λ (val)
          (unless (predicate? val)
            ((listof-*/fail blame val predicate?) #f))
          (check-all p-app val))))
    
    (define (fo-check x)
      (and (predicate? x) 
           (for/and ([v (in-list x)])
             (contract-first-order-passes? ctc v))))
    (cond
      [(flat-contract? ctc)
       (make-flat-contract
        #:name ctc-name
        #:first-order fo-check
        #:projection (listof-*-ho-check (λ (p v) (for-each p v) v))
        #:val-first-projection (listof-*-val-first-flat-proj predicate? ctc)
        #:generate (generate ctc)
        #:exercise (exercise ctc)
        #:list-contract? #t)]
      [(chaperone-contract? ctc)
       (make-chaperone-contract
        #:name ctc-name
        #:first-order fo-check
        #:projection (listof-*-ho-check (λ (p v) (map p v)))
        #:val-first-projection (listof-*-val-first-ho-proj predicate? ctc)
        #:generate (generate ctc)
        #:exercise (exercise ctc)
        #:list-contract? #t)]
      [else
       (make-contract
        #:name ctc-name
        #:first-order fo-check
        #:val-first-projection (listof-*-val-first-ho-proj predicate? ctc)
        #:projection (listof-*-ho-check (λ (p v) (map p v)))
        #:exercise (exercise ctc)
        #:list-contract? #t)])))

(define (listof-*-val-first-flat-proj predicate? ctc)
  (define vf-proj (get/build-val-first-projection ctc))
  (λ (blame)
    (define p-app (vf-proj (blame-add-listof-*-context blame)))
    (λ (val)
      (cond
        [(predicate? val)
         (λ (neg-party)
           (for ([ele (in-list val)])
             ((p-app ele) neg-party))
           val)]
        [else
         (listof-*/fail blame val predicate?)]))))

(define (listof-*-val-first-ho-proj predicate? ctc)
  (define vf-proj (get/build-val-first-projection ctc))
  (λ (blame)
    (define p-app (vf-proj (blame-add-listof-*-context blame)))
    (λ (val)
      (cond
        [(predicate? val)
         (λ (neg-party)
           (for/list ([ele (in-list val)])
             ((p-app ele) neg-party)))]
        [else
         (listof-*/fail blame val predicate?)]))))

(define (listof-*/fail blame val predicate?)
  (λ (neg-party)
    (raise-blame-error blame #:missing-party neg-party val
                       '(expected: "~s" given: "~e")
                       (object-name predicate?)
                       val)))

(define (blame-add-listof-*-context blame) (blame-add-context blame "an element of"))
(define (non-empty-list? x) (and (pair? x) (list? x)))

(define (no-exercise ctc) (λ (size) (values void '())))
(define listof-func (*-listof list? 'listof listof-generate no-exercise))
(define/subexpression-pos-prop (listof x) (listof-func x))

(define non-empty-listof-func (*-listof non-empty-list? 
                                        'non-empty-listof
                                        non-empty-listof-generate
                                        non-empty-listof-exercise))
(define/subexpression-pos-prop (non-empty-listof a) (non-empty-listof-func a))

(define (blame-add-car-context blame) (blame-add-context blame "the car of"))
(define (blame-add-cdr-context blame) (blame-add-context blame "the cdr of"))


(define ((cons/c-val-first-ho-check combine) ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (define car-val-first-proj (get/build-val-first-projection ctc-car))
  (define cdr-val-first-proj (get/build-val-first-projection ctc-cdr))
  (λ (blame)
    (define car-p (car-val-first-proj (blame-add-car-context blame)))
    (define cdr-p (cdr-val-first-proj (blame-add-cdr-context blame)))
    (λ (v)
      (λ (neg-party)
        (unless (pair? v)
          (raise-not-cons-blame-error blame #:missing-party neg-party v))
        (combine v 
                 ((car-p (car v)) neg-party)
                 ((cdr-p (cdr v)) neg-party))))))

(define ((cons/c-ho-check combine) ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (define car-proj (contract-projection ctc-car))
  (define cdr-proj (contract-projection ctc-cdr))
  (λ (blame)
    (let ([car-p (car-proj (blame-add-car-context blame))]
          [cdr-p (cdr-proj (blame-add-cdr-context blame))])
      (λ (v)
        (unless (pair? v)
          (raise-not-cons-blame-error blame v))
        (combine v (car-p (car v)) (cdr-p (cdr v)))))))

(define (cons/c-first-order ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (λ (v)
    (and (pair? v)
         (contract-first-order-passes? ctc-car (car v))
         (contract-first-order-passes? ctc-cdr (cdr v)))))

(define (cons/c-name ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (build-compound-type-name 'cons/c ctc-car ctc-cdr))

(define (cons/c-stronger? this that) 
  (and (the-cons/c? that)
       (contract-stronger? (the-cons/c-hd-ctc this)
                           (the-cons/c-hd-ctc that))
       (contract-stronger? (the-cons/c-tl-ctc this)
                           (the-cons/c-tl-ctc that))))

(define (cons/c-generate ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (λ (fuel)
    (define car-gen (generate/choose ctc-car fuel))
    (define cdr-gen (generate/choose ctc-cdr fuel))
    (and car-gen
         cdr-gen
         (λ () (cons (car-gen) (cdr-gen))))))

(define (cons/c-list-contract? c)
  (list-contract? (the-cons/c-tl-ctc c)))

(define-struct the-cons/c (hd-ctc tl-ctc))
(define-struct (flat-cons/c the-cons/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:val-first-projection (cons/c-val-first-ho-check (λ (v a d) v))
   #:projection (cons/c-ho-check (λ (v a d) v))
   #:name cons/c-name
   #:first-order cons/c-first-order
   #:stronger cons/c-stronger?
   #:generate cons/c-generate
   #:list-contract? cons/c-list-contract?))
(define-struct (chaperone-cons/c the-cons/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:val-first-projection (cons/c-val-first-ho-check (λ (v a d) (cons a d)))
     #:projection (cons/c-ho-check (λ (v a d) (cons a d)))
     #:name cons/c-name
     #:first-order cons/c-first-order
     #:stronger cons/c-stronger?
     #:generate cons/c-generate
     #:list-contract? cons/c-list-contract?)))
(define-struct (impersonator-cons/c the-cons/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:val-first-projection (cons/c-val-first-ho-check (λ (v a d) (cons a d)))
   #:projection (cons/c-ho-check (λ (v a d) (cons a d)))
   #:name cons/c-name
   #:first-order cons/c-first-order
   #:stronger cons/c-stronger?
   #:generate cons/c-generate
   #:list-contract? cons/c-list-contract?))

(define/subexpression-pos-prop (cons/c a b)
  (define ctc-car (coerce-contract 'cons/c a))
  (define ctc-cdr (coerce-contract 'cons/c b))
  (cond
    [(and (flat-contract? ctc-car) (flat-contract? ctc-cdr))
     (flat-cons/c ctc-car ctc-cdr)]
    [(and (chaperone-contract? ctc-car) (chaperone-contract? ctc-cdr))
     (chaperone-cons/c ctc-car ctc-cdr)]
    [else
     (impersonator-cons/c ctc-car ctc-cdr)]))

(define (raise-not-cons-blame-error blame val #:missing-party [missing-party #f])
  (raise-blame-error
   blame
   val #:missing-party missing-party
   '(expected: "pair?" given: "~e")
   val))

(define/subexpression-pos-prop (list/c . args)
  (define ctc-args (coerce-contracts 'list/c args))
  (cond
    [(andmap flat-contract? ctc-args)
     (flat-list/c ctc-args)]
    [(andmap chaperone-contract? ctc-args)
     (chaperone-list/c ctc-args)]
    [else
     (higher-order-list/c ctc-args)]))

(define (list/c-name-proc c) 
  (apply build-compound-type-name
         'list/c (generic-list/c-args c)))
(define ((list/c-first-order c) x)
  (and (list? x)
       (= (length x) (length (generic-list/c-args c)))
       (for/and ([arg/c (in-list (generic-list/c-args c))]
                 [v (in-list x)])
         ((contract-first-order arg/c) v))))

(define (list/c-generate ctc)
  (define elem-ctcs (generic-list/c-args ctc))
  (λ (fuel)
    (define gens (for/list ([elem-ctc (in-list elem-ctcs)])
                   (generate/choose elem-ctc fuel)))
    (cond
      [(andmap values gens)
       (λ ()
         (for/list ([gen (in-list gens)])
           (gen)))]
      [else
       #f])))

(define (list/c-exercise ctc)
  (multi-exercise (generic-list/c-args ctc)))

(struct generic-list/c (args))

(struct flat-list/c generic-list/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:generate list/c-generate
   #:exercise list/c-exercise
   #:val-first-projection
   (λ (c) 
     (λ (blame) 
       (define projs 
         (for/list ([ctc (in-list (generic-list/c-args c))]
                    [i (in-naturals 1)])
           ((get/build-val-first-projection ctc)
            (add-list-context blame i))))
       (define expected-length (length (generic-list/c-args c)))
       (λ (val) 
         (cond
           [(list? val)
            (define args (generic-list/c-args c))
            (define actual-length (length val))
            (cond
              [(= actual-length expected-length)
               (λ (neg-party)
                 (for ([proj (in-list projs)]
                       [ele (in-list val)])
                   ((proj ele) neg-party))
                 val)]
              [else
               (λ (neg-party)
                 (raise-blame-error
                  blame #:missing-party neg-party 
                  val
                  '(expected: "a list of ~a elements" given: "~a element~a\n  complete list: ~e")
                  expected-length 
                  actual-length
                  (if (= actual-length 1) "" "s")
                  val))])]
           [else
            (λ (neg-party)
              (raise-blame-error blame #:missing-party neg-party
                                 val
                                 '(expected "a list" given: "~e") 
                                 val))]))))
   #:projection
   (lambda (c)
     (lambda (blame)
       (lambda (x)
         (unless (list? x)
           (raise-blame-error blame x '(expected "a list" given: "~e") x))
         (let* ([args (generic-list/c-args c)]
                [expected (length args)]
                [actual (length x)])
           (unless (= actual expected)
             (raise-blame-error
              blame x
              '(expected: "a list of ~a elements" given: "~a element~a\n  complete list: ~e")
              expected 
              actual
              (if (= actual 1) "" "s")
              x))
           (for ([arg/c (in-list args)] [v (in-list x)] [i (in-naturals 1)])
             (((contract-projection arg/c) 
               (add-list-context blame i))
              v))
           x))))
   #:list-contract? (λ (c) #t)))

(define (list/c-chaperone/other-projection c)
  (define args (map contract-projection (generic-list/c-args c)))
  (define expected (length args))
  (λ (blame)
    (define projs (for/list ([arg/c (in-list args)]
                             [i (in-naturals 1)])
                    (arg/c (add-list-context blame i))))
    (λ (x)
      (unless (list? x) (expected-a-list x blame))
      (define actual (length x))
      (unless (= actual expected)
        (raise-blame-error
         blame x
         '(expected: "a list of ~a elements" given: "~a element~a\n complete list: ~e")
         expected
         actual
         (if (= actual 1) "" "s")
         x))
      (for/list ([item (in-list x)]
                 [proj (in-list projs)])
        (proj item)))))

(define (expected-a-list x blame #:missing-party [missing-party #f])
  (raise-blame-error blame #:missing-party missing-party
                     x '(expected: "a list" given: "~e") x))

(define (expected-a-list-of-len x actual expected blame #:missing-party [missing-party #f])
  (unless (= actual expected)
    (raise-blame-error
     blame #:missing-party missing-party x
     '(expected: "a list of ~a elements" given: "~a element~a\n complete list: ~e")
     expected
     actual
     (if (= actual 1) "" "s")
     x)))

(define (list/c-chaperone/other-val-first-projection c)
  (define projs (map get/build-val-first-projection (generic-list/c-args c)))
  (define expected (length projs))
  (λ (blame)
    (define p-apps (for/list ([proj (in-list projs)] 
                              [i (in-naturals 1)])
                     (proj (add-list-context blame i))))
    (λ (val)
      (cond
        [(list? val)
         (define actual (length val))
         (cond
           [(= actual expected)
            (λ (neg-party)
              (for/list ([item (in-list val)]
                         [p-app (in-list p-apps)])
                ((p-app item) neg-party)))]
           [else
            (λ (neg-party)
              (expected-a-list-of-len val actual expected blame
                                      #:missing-party neg-party))])]
        [else
         (λ (neg-party)
           (expected-a-list val blame #:missing-party neg-party))]))))

(define (add-list-context blame i)
  (blame-add-context blame (format "the ~a~a element of"
                                   i
                                   (case (modulo i 10)
                                     [(1) "st"]
                                     [(2) "nd"]
                                     [(3) "rd"]
                                     [else "th"]))))

(struct chaperone-list/c generic-list/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name list/c-name-proc
     #:first-order list/c-first-order
     #:generate list/c-generate
     #:exercise list/c-exercise
     #:projection list/c-chaperone/other-projection
     #:val-first-projection list/c-chaperone/other-val-first-projection
     #:list-contract? (λ (c) #t))))

(struct higher-order-list/c generic-list/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:generate list/c-generate
   #:exercise list/c-exercise
   #:projection list/c-chaperone/other-projection
   #:val-first-projection list/c-chaperone/other-val-first-projection
   #:list-contract? (λ (c) #t)))

(define/subexpression-pos-prop (syntax/c ctc-in)
  (let ([ctc (coerce-flat-contract 'syntax/c ctc-in)])
    (flat-named-contract
     (build-compound-type-name 'syntax/c ctc)
     (let ([pred (flat-contract-predicate ctc)])
       (λ (val)
         (and (syntax? val)
              (pred (syntax-e val))))))))

(define/subexpression-pos-prop promise/c
  (λ (ctc-in)
    (let* ([ctc (coerce-contract 'promise/c ctc-in)]
           [ctc-proc (contract-projection ctc)])
      (define chap? (chaperone-contract? ctc))
      (define c/i-struct (if chap? chaperone-struct impersonate-struct))
      (define c/i-procedure (if chap? chaperone-procedure impersonate-procedure))
      ((if chap? make-chaperone-contract make-contract)
       #:name (build-compound-type-name 'promise/c ctc)
       #:projection
       (λ (blame)
         (let ([p-app (ctc-proc (blame-add-context blame "the promise from"))])
           (λ (val)
             (unless (promise? val)
               (raise-blame-error
                blame
                val
                '(expected: "<promise>" given: "~e")
                val))
             (c/i-struct
              val
              promise-forcer (λ (_ proc) 
                               (c/i-procedure
                                proc
                                (λ (promise)
                                  (values p-app promise))))))))
       #:first-order promise?))))

;; (parameter/c in/out-ctc)
;; (parameter/c in-ctc out-ctc)
(define/subexpression-pos-prop parameter/c
  (case-lambda
    [(in-ctc)
     (define ctc (coerce-contract 'parameter/c in-ctc))
     (make-parameter/c ctc ctc #f)]
    [(in-ctc out-ctc)
     (make-parameter/c (coerce-contract 'parameter/c in-ctc)
                       (coerce-contract 'parameter/c out-ctc)
                       #t)]))

;; in - negative contract
;; out - positive contract
;; both-supplied? - for backwards compat printing
(define-struct parameter/c (in out both-supplied?)
  #:property prop:custom-write custom-write-property-proc
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
      (let* ([in-proc (contract-projection (parameter/c-in ctc))]
             [out-proc (contract-projection (parameter/c-out ctc))])
        (λ (blame)
          (define blame/c (blame-add-context blame "the parameter of"))
          (define partial-neg-contract (in-proc (blame-swap blame/c)))
          (define partial-pos-contract (out-proc blame/c))
          (λ (val)
            (cond
              [(parameter? val)
               (make-derived-parameter 
                val 
                partial-neg-contract
                partial-pos-contract)]
              [else
               (raise-blame-error blame val '(expected "a parameter"))])))))
   #:val-first-projection
   (λ (ctc)
     (define in-proc (contract-projection (parameter/c-in ctc)))
     (define out-proc (contract-projection (parameter/c-out ctc)))
     (λ (blame)
       (define blame/c (blame-add-context blame "the parameter of"))
       (define swapped (blame-swap blame/c))
       (λ (val)
         (cond
           [(parameter? val)
            (λ (neg-party)
              (make-derived-parameter
               val
               ;; unfortunately expensive
               (in-proc (blame-add-missing-party swapped neg-party))
               (out-proc (blame-add-missing-party blame/c neg-party))))]
           [else
            (λ (neg-party)
              (raise-blame-error blame #:missing-party neg-party
                                 val '(expected "a parameter")))]))))

   #:name
   (λ (ctc) (apply build-compound-type-name
                   `(parameter/c ,(parameter/c-in ctc)
                                 ,@(if (parameter/c-both-supplied? ctc)
                                       (list (parameter/c-out ctc))
                                       (list)))))
   #:first-order
   (λ (ctc)
      (let ([tst (contract-first-order (parameter/c-out ctc))])
        (λ (x)
           (and (parameter? x)
                (tst (x))))))

   #:stronger
   (λ (this that)
      (and (parameter/c? that)
           (and (contract-stronger? (parameter/c-out this)
                                    (parameter/c-out that))
                (contract-stronger? (parameter/c-in that)
                                    (parameter/c-in this)))))))

(define-struct procedure-arity-includes/c (n)
  #:property prop:custom-write custom-write-property-proc
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:stronger (λ (this that) (and (procedure-arity-includes/c? that)
                                  (= (procedure-arity-includes/c-n this)
                                     (procedure-arity-includes/c-n that))))
   #:name (λ (ctc) `(procedure-arity-includes/c ,(procedure-arity-includes/c-n ctc)))
   #:first-order (λ (ctc)
                   (define n (procedure-arity-includes/c-n ctc))
                   (λ (x)
                     (and (procedure? x)
                          (procedure-arity-includes? x n))))))

(define/subexpression-pos-prop (procedure-arity-includes/c n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'procedure-arity-includes/c
                          "exact-nonnegative-integer?"
                          n))
  (make-procedure-arity-includes/c n))

(define (get-any-projection c) any-projection)
(define (any-projection b) any-function)
(define (any-function x) x)

(define (get-any? c) any?)
(define (any? x) #t)
(define any/c-neg-party-fn (λ (val) (λ (neg-party) val)))

(define (random-any/c env fuel)
  (cond
    [(zero? (hash-count env))
     (rand-choice
      [1/3 (any/c-simple-value)]
      [1/3 (any/c-procedure env fuel)]
      [else (any/c-from-predicate-generator env fuel)])]
    [else
     (rand-choice
      [1/4 (oneof (hash-ref env (oneof (hash-keys env))))]
      [1/4 (any/c-simple-value)]
      [1/4 (any/c-procedure env fuel)]
      [else (any/c-from-predicate-generator env fuel)])]))

(define (any/c-simple-value)
  (oneof '(0 #f "" () #() -1 1 #t elephant)))
(define (any/c-from-predicate-generator env fuel)
  ((hash-ref predicate-generator-table
             (oneof (hash-keys predicate-generator-table)))
   fuel))
(define (any/c-procedure env fuel)
  (procedure-rename
   (procedure-reduce-arity
    (λ args
      (apply
       values
       (for/list ([i (in-range (rand-nat))])
         (random-any/c env fuel))))
    (rand-nat))
   'random-any/c-generated-procedure))

(define-struct any/c ()
  #:property prop:custom-write custom-write-property-proc
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection get-any-projection
   #:val-first-projection (λ (ctc) (λ (blame) any/c-neg-party-fn))
   #:stronger (λ (this that) (any/c? that))
   #:name (λ (ctc) 'any/c)
   #:generate (λ (ctc) 
                (λ (fuel) 
                  (define env (generate-env))
                  (λ () (random-any/c env fuel))))
   #:first-order get-any?))

(define/final-prop any/c (make-any/c))

(define-syntax (any stx)
  (raise-syntax-error 'any "use of 'any' outside the range of an arrow contract" stx))

(define (none-curried-proj ctc)
  (λ (blame)
    (λ (val) 
      (raise-blame-error
       blame
       val
       '("~s accepts no values" given: "~e")
       (none/c-name ctc)
       val))))

(define ((((none-curried-val-first-proj ctc) blame) val) neg-party)
  (raise-blame-error
   blame #:missing-party neg-party
   val
   '("~s accepts no values" given: "~e")
   (none/c-name ctc)
   val))

(define-struct none/c (name)
  #:property prop:custom-write custom-write-property-proc
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection none-curried-proj
   #:val-first-projection none-curried-val-first-proj
   #:stronger (λ (this that) #t)
   #:name (λ (ctc) (none/c-name ctc))
   #:first-order (λ (ctc) (λ (val) #f))))

(define/final-prop none/c (make-none/c 'none/c))

;; prompt-tag/c
(define-syntax prompt-tag/c
  (syntax-rules (values)
    [(_ ?ctc ... #:call/cc (values ?call/cc ...))
     (-prompt-tag/c (list ?ctc ...) (list ?call/cc ...))]
    [(_ ?ctc ... #:call/cc ?call/cc)
     (-prompt-tag/c (list ?ctc ...) (list ?call/cc))]
    [(_ ?ctc ...) (-prompt-tag/c (list ?ctc ...) (list))]))

;; procedural part of the contract
;; takes two lists of contracts (abort & call/cc contracts)
(define/subexpression-pos-prop (-prompt-tag/c ctc-args call/ccs)
  (define ctcs (coerce-contracts 'prompt-tag/c ctc-args))
  (define call/cc-ctcs (coerce-contracts 'prompt-tag/c call/ccs))
  (cond [(and (andmap chaperone-contract? ctcs)
              (andmap chaperone-contract? call/cc-ctcs))
         (chaperone-prompt-tag/c ctcs call/cc-ctcs)]
        [else
         (impersonator-prompt-tag/c ctcs call/cc-ctcs)]))

(define (prompt-tag/c-name ctc)
  (apply build-compound-type-name
         (append (list 'prompt-tag/c) (base-prompt-tag/c-ctcs ctc)
                 (list '#:call/cc) (base-prompt-tag/c-call/ccs ctc))))

;; build a projection for prompt tags
(define ((prompt-tag/c-proj chaperone?) ctc)
  (define proxy (if chaperone? chaperone-prompt-tag impersonate-prompt-tag))
  (define proc-proxy (if chaperone? chaperone-procedure impersonate-procedure))
  (define ho-projs
    (map contract-projection (base-prompt-tag/c-ctcs ctc)))
  (define call/cc-projs
    (map contract-projection (base-prompt-tag/c-call/ccs ctc)))
  (λ (blame)
    (define (make-proj projs swap?)
      (λ vs
         (define vs2 (for/list ([proj projs] [v vs])
                       ((proj (if swap? (blame-swap blame) blame)) v)))
         (apply values vs2)))
    ;; prompt/abort projections
    (define proj1 (make-proj ho-projs #f))
    (define proj2 (make-proj ho-projs #t))
    ;; call/cc projections
    (define call/cc-guard (make-proj call/cc-projs #f))
    (define call/cc-proxy
      (λ (f)
        (proc-proxy
         f
         (λ args
           (apply values (make-proj call/cc-projs #t) args)))))
    ;; now do the actual wrapping
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name ctc)
         val))
      (proxy val proj1 proj2 call/cc-guard call/cc-proxy
             impersonator-prop:contracted ctc
             impersonator-prop:blame blame))))

(define ((prompt-tag/c-val-first-proj chaperone?) ctc)
  (define proxy (if chaperone? chaperone-prompt-tag impersonate-prompt-tag))
  (define proc-proxy (if chaperone? chaperone-procedure impersonate-procedure))
  (define ho-projs
    (map get/build-val-first-projection (base-prompt-tag/c-ctcs ctc)))
  (define call/cc-projs
    (map get/build-val-first-projection (base-prompt-tag/c-call/ccs ctc)))
  (λ (blame)
    (define swapped (blame-swap blame))
    (define ho-neg-projs (for/list ([proj (in-list ho-projs)]) (proj swapped)))
    (define ho-pos-projs (for/list ([proj (in-list ho-projs)]) (proj blame)))
    (define cc-neg-projs (for/list ([proj (in-list call/cc-projs)]) (proj swapped)))
    (define cc-pos-projs (for/list ([proj (in-list call/cc-projs)]) (proj blame)))
    (λ (val)
      (define (make-proj projs neg-party)
        (λ vs
          (define vs2 (for/list ([proj projs] [v vs])
                        ((proj v) neg-party)))
          (apply values vs2)))
      ;; now do the actual wrapping
      (cond
        [(continuation-prompt-tag? val)
         (λ (neg-party)
           ;; prompt/abort projections
           (define proj1 (make-proj ho-pos-projs neg-party))
           (define proj2 (make-proj ho-neg-projs neg-party))
           ;; call/cc projections
           (define call/cc-guard (make-proj cc-pos-projs neg-party))
           (define call/cc-proxy
             (λ (f)
               (proc-proxy
                f
                (λ args
                  (apply values (make-proj cc-neg-projs neg-party) args)))))
           (proxy val
                  proj1 proj2
                  call/cc-guard call/cc-proxy
                  impersonator-prop:contracted ctc
                  impersonator-prop:blame (blame-add-missing-party blame neg-party)))]
        [else
         (λ (neg-party)
           (raise-blame-error
            blame #:missing-party neg-party val
            '(expected: "~s" given: "~e")
            (contract-name ctc)
            val))]))))

(define (prompt-tag/c-stronger? this that)
  (and (base-prompt-tag/c? that)
       (andmap (λ (this that) (contract-stronger? this that))
               (base-prompt-tag/c-ctcs this)
               (base-prompt-tag/c-ctcs that))
       (andmap (λ (this that) (contract-stronger? this that))
               (base-prompt-tag/c-call/ccs this)
               (base-prompt-tag/c-call/ccs that))))

;; (listof contract) (listof contract)
(define-struct base-prompt-tag/c (ctcs call/ccs))

(define-struct (chaperone-prompt-tag/c base-prompt-tag/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:val-first-projection (prompt-tag/c-val-first-proj #t)
   #:projection (prompt-tag/c-proj #t)
   #:first-order (λ (ctc) continuation-prompt-tag?)
   #:stronger prompt-tag/c-stronger?
   #:name prompt-tag/c-name))

(define-struct (impersonator-prompt-tag/c base-prompt-tag/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:val-first-projection (prompt-tag/c-val-first-proj #f)
   #:projection (prompt-tag/c-proj #f)
   #:first-order (λ (ctc) continuation-prompt-tag?)
   #:stronger prompt-tag/c-stronger?
   #:name prompt-tag/c-name))


;; continuation-mark-key/c
(define/subexpression-pos-prop (continuation-mark-key/c ctc-arg)
  (define ctc (coerce-contract 'continuation-mark-key/c ctc-arg))
  (cond [(chaperone-contract? ctc)
         (chaperone-continuation-mark-key/c ctc)]
        [else
         (impersonator-continuation-mark-key/c ctc)]))

(define (continuation-mark-key/c-name ctc)
  (build-compound-type-name
   'continuation-mark-key/c
   (base-continuation-mark-key/c-ctc ctc)))

(define ((continuation-mark-key/c-proj proxy) ctc)
  (define ho-proj
    (contract-projection (base-continuation-mark-key/c-ctc ctc)))
  (λ (blame)
    (define proj1 (ho-proj blame))
    (define proj2 (ho-proj (blame-swap blame)))
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name ctc)
         val))
      (proxy val proj1 proj2
             impersonator-prop:contracted ctc
             impersonator-prop:blame blame))))

(define ((continuation-mark-key/c-val-first-proj proxy) ctc)
  (define ho-proj
    (get/build-val-first-projection (base-continuation-mark-key/c-ctc ctc)))
  (λ (blame)
    (define swapped (blame-swap blame))
    (define proj1 (ho-proj blame))
    (define proj2 (ho-proj (blame-swap blame)))
    (λ (val)
      (cond
        [(continuation-mark-key? val)
         (λ (neg-party)
           (proxy val 
                  (λ (v) ((proj1 v) neg-party))
                  (λ (v) ((proj2 v) neg-party))
                  impersonator-prop:contracted ctc
                  impersonator-prop:blame blame))]
        [else 
         (λ (neg-party)
           (unless (contract-first-order-passes? ctc val)
             (raise-blame-error
              blame #:missing-party neg-party
              val
              '(expected: "~s" given: "~e")
              (contract-name ctc)
              val)))]))))

(define (continuation-mark-key/c-stronger? this that)
  (and (base-continuation-mark-key/c? that)
       (contract-stronger?
        (base-continuation-mark-key/c-ctc this)
        (base-continuation-mark-key/c-ctc that))))

(define-struct base-continuation-mark-key/c (ctc))

(define-struct (chaperone-continuation-mark-key/c
                base-continuation-mark-key/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:val-first-projection (continuation-mark-key/c-val-first-proj chaperone-continuation-mark-key)
   #:projection (continuation-mark-key/c-proj chaperone-continuation-mark-key)
   #:first-order (λ (ctc) continuation-mark-key?)
   #:stronger continuation-mark-key/c-stronger?
   #:name continuation-mark-key/c-name))

(define-struct (impersonator-continuation-mark-key/c
                base-continuation-mark-key/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:val-first-projection (continuation-mark-key/c-val-first-proj impersonate-continuation-mark-key)
   #:projection (continuation-mark-key/c-proj impersonate-continuation-mark-key)
   #:first-order (λ (ctc) continuation-mark-key?)
   #:stronger continuation-mark-key/c-stronger?
   #:name continuation-mark-key/c-name))

;; evt/c : Contract * -> Contract
;; Contract combinator for synchronizable events
(define (evt/c . maybe-ctcs)
  (define ctcs (coerce-contracts 'evt/c maybe-ctcs))
  (for ([ctc ctcs])
    (unless (chaperone-contract? ctc)
      (raise-argument-error 'evt/c "chaperone-contract?" ctc)))
  (make-chaperone-evt/c ctcs))

;; evt/c-proj : Contract -> (Blame -> Any -> Any)
;; Constructs the projection for evt/c
(define (evt/c-proj evt-ctc)
  (define ctcs (chaperone-evt/c-ctcs evt-ctc))
  (define projs (map contract-projection ctcs))
  (λ (blame)
    (define ((checker val) . args)
      (define expected-num (length ctcs))
      (unless (= (length args) expected-num)
        (raise-blame-error
         blame val
         `(expected: "event that produces ~a values"
           given: "event that produces ~a values")
         expected-num
         (length args)))
      (apply
       values
       (for/list ([proj projs] [val args])
         ((proj blame) val))))
    (define (generator evt)
      (values evt (checker evt)))
    (λ (val)
      (unless (contract-first-order-passes? evt-ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name evt-ctc)
         val))
      (chaperone-evt val generator))))

;; evt/c-first-order : Contract -> Any -> Boolean
;; First order check for evt/c
(define ((evt/c-first-order ctc) v) (evt? v))

;; evt/c-name : Contract -> Sexp
;; Construct the name of the contract
(define (evt/c-name ctc)
  (apply build-compound-type-name
         (cons 'evt/c (chaperone-evt/c-ctcs ctc))))

;; evt/c-stronger? : Contract Contract -> Boolean
(define (evt/c-stronger? this that)
  (define this-ctcs (chaperone-evt/c-ctcs this))
  (define that-ctcs (chaperone-evt/c-ctcs that))
  (and (= (length this-ctcs) (that-ctcs))
       (for/and ([this this-ctcs] [that that-ctcs])
         (contract-stronger? this that))))

;; ctcs - Listof<Contract>
(define-struct chaperone-evt/c (ctcs)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection evt/c-proj
   #:first-order evt/c-first-order
   #:stronger evt/c-stronger?
   #:name evt/c-name))

;; channel/c
(define/subexpression-pos-prop (channel/c ctc-arg)
  (define ctc (coerce-contract 'channel/c ctc-arg))
  (cond [(chaperone-contract? ctc)
         (chaperone-channel/c ctc)]
        [else
         (impersonator-channel/c ctc)]))

(define (channel/c-name ctc)
  (build-compound-type-name
   'channel/c
   (base-channel/c-ctc ctc)))

(define ((channel/c-proj proxy) ctc)
  (define ho-proj
    (contract-projection (base-channel/c-ctc ctc)))
  (λ (blame)
    (define proj1 (λ (ch) (values ch (λ (v) ((ho-proj blame) v)))))
    (define proj2 (λ (ch v) ((ho-proj (blame-swap blame)) v)))
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name ctc)
         val))
      (proxy val proj1 proj2
             impersonator-prop:contracted ctc
             impersonator-prop:blame blame))))

(define ((channel/c-val-first-proj proxy) ctc)
  (define ho-proj
    (get/build-val-first-projection (base-channel/c-ctc ctc)))
  (λ (blame)
    (define pos-proj (ho-proj blame))
    (define neg-proj (ho-proj (blame-swap blame)))
    (define (proj1 neg-party) (λ (ch) (values ch (λ (v) ((pos-proj v) neg-party)))))
    (define (proj2 neg-party) (λ (ch v) ((neg-proj v) neg-party)))
    (λ (val)
      (cond
        [(channel? val)
         (λ (neg-party)
           (proxy val 
                  (proj1 neg-party)
                  (proj2 neg-party)
                  impersonator-prop:contracted ctc
                  impersonator-prop:blame blame))]
        [else
         (λ (neg-party)
           (raise-blame-error
            blame #:missing-party neg-party
            val '(expected: "~s" given: "~e")
            (contract-name ctc)
            val))]))))

(define (channel/c-first-order ctc) channel?)

(define (channel/c-stronger? this that)
  (and (base-channel/c? that)
       (contract-stronger?
        (base-channel/c-ctc this)
        (base-channel/c-ctc that))))

(define-struct base-channel/c (ctc))

(define-struct (chaperone-channel/c base-channel/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:val-first-projection (channel/c-val-first-proj chaperone-channel)
   #:projection (channel/c-proj chaperone-channel)
   #:first-order channel/c-first-order
   #:stronger channel/c-stronger?
   #:name channel/c-name))

(define-struct (impersonator-channel/c base-channel/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:val-first-projection (channel/c-val-first-proj impersonate-channel)
   #:projection (channel/c-proj impersonate-channel)
   #:first-order channel/c-first-order
   #:stronger channel/c-stronger?
   #:name channel/c-name))


(define (flat-contract-predicate x)
  (contract-struct-first-order
   (coerce-flat-contract 'flat-contract-predicate x)))

(define (flat-contract? x) 
  (let ([c (coerce-contract/f x)])
    (and c
         (flat-contract-struct? c))))

(define (chaperone-contract? x)
  (let ([c (coerce-contract/f x)])
    (and c
         (or (chaperone-contract-struct? c)
             (and (prop:opt-chaperone-contract? c)
                  ((prop:opt-chaperone-contract-get-test c) c))))))

(define (impersonator-contract? x)
  (let ([c (coerce-contract/f x)])
    (and c
         (not (flat-contract-struct? c))
         (not (chaperone-contract-struct? c)))))

(define (contract-name ctc)
  (contract-struct-name
   (coerce-contract 'contract-name ctc)))

(define (contract? x) (and (coerce-contract/f x) #t))
(define (contract-projection ctc)
  (contract-struct-projection
   (coerce-contract 'contract-projection ctc)))
(define (contract-val-first-projection ctc)
  (contract-struct-val-first-projection
   (coerce-contract 'contract-projection ctc)))

(define (get/build-val-first-projection ctc)
  (or (contract-struct-val-first-projection ctc)
      (let ([p (contract-projection ctc)])
        (λ (blme)
          (procedure-rename
           (λ (val)
             (λ (neg-party)
               ((p (blame-add-missing-party blme neg-party)) val)))
           (string->symbol (format "val-first: ~s" (contract-name ctc))))))))


(define (flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (flat-named-contract name predicate [generate #f])
  (cond
    [(and (procedure? predicate)
          (procedure-arity-includes? predicate 1))
     (make-predicate-contract name predicate generate #f)]
    [(flat-contract? predicate)
     (make-predicate-contract name (flat-contract-predicate predicate) generate #f)]
    [else
     (raise-argument-error 'flat-named-contract
                           (format "~s" `(or/c flat-contract?
                                               (and/c procedure?
                                                      (λ (x) (procedure-arity-include? x 1)))))
                           predicate)]))

(define printable/c
  (flat-named-contract
   'printable/c
   (λ (x)
     (let printable? ([x x])
       (or (symbol? x)
           (string? x)
           (bytes? x)
           (boolean? x)
           (char? x)
           (null? x)
           (number? x)
           (regexp? x)
           (prefab-struct-key x) ;; this cannot be last, since it doesn't return just #t
           (and (pair? x)
                (printable? (car x))
                (printable? (cdr x)))
           (and (vector? x)
                (andmap printable? (vector->list x)))
           (and (box? x)
                (printable? (unbox x)))
           (and (hash? x)
                (immutable? x)
                (for/and ([(k v) (in-hash x)])
                  (and (printable? k)
                       (printable? v)))))))))


(define natural-number/c
  (flat-named-contract
   'natural-number/c
   (λ (x)
     (and (number? x)
          (integer? x)
          (exact? x)
          (x . >= . 0)))
   (λ (fuel) (λ () (exact-nonnegative-integer-gen fuel)))))

(define (n->th n)
  (string-append 
   (number->string n)
   (case (modulo n 10)
     [(1) "st"]
     [(2) "nd"]
     [(3) "rd"]
     [else "th"])))
