#lang racket/base

(require (for-syntax racket/base
                     "arr-util.rkt")
         racket/promise
         syntax/location
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
         char-in
         real-in
         natural-number/c
         string-len/c
         false/c
         printable/c
         listof list*of non-empty-listof cons/c list/c cons/dc
         promise/c
         syntax/c
         
         check-between/c
         check-unary-between/c
         parameter/c
         procedure-arity-includes/c
         
         any/c  any/c?
         any
         none/c
         make-none/c

         prompt-tag/c
         continuation-mark-key/c

         channel/c
         evt/c

         flat-contract
         flat-contract-predicate
         flat-named-contract
         
         n->th
         
         blame-add-car-context
         blame-add-cdr-context
         raise-not-cons-blame-error
         
         random-any/c

         rename-contract
         if/c

         pairwise-stronger-contracts?)

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

(define (late-neg-and-proj ctc)
  (define mk-pos-projs (map get/build-late-neg-projection (base-and/c-ctcs ctc)))
  (λ (blame)
    (define projs
      (for/list ([c (in-list mk-pos-projs)]
                 [n (in-naturals 1)])
        (c (blame-add-context blame (format "the ~a conjunct of" (n->th n))))))
    (λ (val neg-party)
      (let loop ([projs (cdr projs)]
                 [val ((car projs) val neg-party)])
        (cond
          [(null? projs) val]
          [else
           (loop (cdr projs)
                 ((car projs) val neg-party))])))))

(define (first-order-late-neg-and-proj ctc)
  (define predicates (first-order-and/c-predicates ctc))
  (define blame-accepters (map get/build-late-neg-projection (base-and/c-ctcs ctc)))
  (λ (blame)
    (define new-blame (blame-add-context blame "an and/c case of"))
    (define projs (map (λ (f) (f new-blame)) blame-accepters))
    (λ (val neg-party)
      (let loop ([predicates predicates]
                 [projs projs])
        (cond
          [(null? predicates) val]
          [else
           (cond
             [((car predicates) val)
              (loop (cdr predicates) (cdr projs))]
             [else
              ((car projs) val neg-party)])])))))

(define (and-stronger? this that)
  (and (base-and/c? that)
       (pairwise-stronger-contracts? (base-and/c-ctcs this)
                                     (base-and/c-ctcs that))))

(define (and/c-generate? ctc)
  (cond
    [(and/c-check-nonneg ctc real?) => values]
    [(and/c-check-nonneg ctc rational?) => values]
    [(null? (base-and/c-ctcs ctc)) => (λ (fuel) #f)]
    [else
     (define flat (filter flat-contract? (base-and/c-ctcs ctc)))
     (define ho (filter (λ (x) (not (flat-contract? x))) (base-and/c-ctcs ctc)))
     (cond
       [(null? ho)
        (λ (fuel)
          (define candidates
            (let loop ([sub-contracts-after (cdr (base-and/c-ctcs ctc))]
                       [sub-contract (car (base-and/c-ctcs ctc))]
                       [sub-contracts-before '()]
                       [candidates '()])
              (define sub-gen (contract-random-generate/choose sub-contract fuel))
              (define new-candidates
                (cond
                  [sub-gen
                   (cons (cons sub-gen (append (reverse sub-contracts-before) sub-contracts-after))
                         candidates)]
                  [else candidates]))
              (cond
                [(null? sub-contracts-after) new-candidates]
                [else (loop (cdr sub-contracts-after)
                            (car sub-contracts-after)
                            (cons sub-contract sub-contracts-before)
                            new-candidates)])))
          (cond
            [(null? candidates) #f]
            [else
             (λ ()
               (let loop ([attempts 10])
                 (cond
                   [(zero? attempts) contract-random-generate-fail]
                   [else
                    (define which (oneof candidates))
                    (define val ((car which)))
                    (cond
                      [(andmap (λ (p?) (p? val)) (cdr which))
                       val]
                      [else
                       (loop (- attempts 1))])])))]))]
       [(null? (cdr ho))
        (λ (fuel)
          (define ho-gen (contract-random-generate/choose (car ho) fuel))
          (cond
            [ho-gen 
             (λ ()
               (let loop ([attempts 10])
                 (cond
                   [(zero? attempts) contract-random-generate-fail]
                   [else
                    (define val (ho-gen))
                    (cond
                      [(andmap (λ (p?) (p? val)) flat)
                       val]
                      [else
                       (loop (- attempts 1))])])))]
            [else #f]))]
       [else
        (λ (fuel) #f)])]))

(define (and/c-check-nonneg ctc pred)
  (define sub-contracts (base-and/c-ctcs ctc))
  (cond
    [(pairwise-stronger-contracts?
      (list (coerce-contract 'and/c-check-nonneg pred) (not/c negative?))
      sub-contracts)
     (define go (hash-ref predicate-generator-table pred))
     (λ (fuel)
       (λ ()
         (abs (go fuel))))]
    [else #f]))

(define (pairwise-stronger-contracts? c1s c2s)
  (let loop ([c1s c1s]
             [c2s c2s])
    (cond
      [(and (null? c1s) (null? c2s)) #t]
      [(and (pair? c1s) (pair? c2s))
       (and (contract-struct-stronger? (car c1s) (car c2s))
            (loop (cdr c1s) (cdr c2s)))]
      [else #f])))

(define-struct base-and/c (ctcs))
(define-struct (first-order-and/c base-and/c) (predicates)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:late-neg-projection first-order-late-neg-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))
(define-struct (chaperone-and/c base-and/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection late-neg-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))
(define-struct (impersonator-and/c base-and/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection late-neg-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate?))

(define-syntax (and/c stx)
  (syntax-case stx (pair? listof)
    [(_ pair? (listof e))
     #'(non-empty-listof e)]
    [(_ (listof e) pair?)
     #'(non-empty-listof e)]
    [(_ . args)
     #'(real-and/c . args)]
    [x
     (identifier? #'x)
     #'real-and/c]))

(define/subexpression-pos-prop/name real-and/c-name (real-and/c . raw-fs)
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

(define (between/c-stronger this that)
  (define this-low (between/c-s-low this))
  (define this-high (between/c-s-high this))
  (cond
    [(between/c-s? that)
     (and (<= (between/c-s-low that) this-low)
          (<= this-high (between/c-s-high that)))]
    [(</>-ctc? that)
     (define that-x (</>-ctc-x that))
     (cond
       [(<-ctc? that)
        (and (= this-low -inf.0)
             (< this-high that-x))]
       [(>-ctc? that)
        (and (= this-high +inf.0)
             (< that-x this-low))])]
    [else #f]))

(define (between/c-first-order ctc)
  (define n (between/c-s-low ctc))
  (define m (between/c-s-high ctc))
  (λ (x) 
    (and (real? x)
         (<= n x m))))

(define ((between/c-generate ctc) fuel)
  (define n (between/c-s-low ctc))
  (define m (between/c-s-high ctc))
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
            (+ n (* (random) (- m n)))])]))]))

(define-struct between/c-s (low high)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
     (define n (between/c-s-low ctc))
     (define m (between/c-s-high ctc))
     (define name (if (real-in-s? ctc) 'real-in 'between/c))
     (cond
       [(and (= n -inf.0) (= m +inf.0))
        `(,name ,n ,m)]
       [(= n -inf.0) `(<=/c ,m)]
       [(= m +inf.0) `(>=/c ,n)]
       [(= n m) `(=/c ,n)]
       [else `(,name ,n ,m)]))
   #:stronger between/c-stronger
   #:first-order between/c-first-order
   #:generate between/c-generate))
(define-struct (real-in-s between/c-s) ())

(define (maybe-neg n) (rand-choice [1/2 n] [else (- n)]))

(define (check-unary-between/c sym x)
  (unless (real? x)
    (raise-argument-error sym "real?" x)))

(define/final-prop (=/c x) 
  (check-unary-between/c '=/c x)
  (make-between/c-s x x))
(define/final-prop (<=/c x) 
  (check-unary-between/c '<=/c x)
  (make-between/c-s -inf.0 x))
(define/final-prop (>=/c x)
  (check-unary-between/c '>=/c x)
  (make-between/c-s x +inf.0))
(define (check-between/c x y)
  (check-two-args 'between/c x y real? real?))
(define/final-prop (between/c x y)
  (check-between/c x y)
  (if (= x y)
      (coerce-contract 'between/c x)
      (make-between/c-s x y)))

(define (make-</c->/c-contract-property name </> -/+ less/greater)
  (build-flat-contract-property
   #:name (λ (c) `(,name ,(</>-ctc-x c)))
   #:first-order (λ (ctc) (define x (</>-ctc-x ctc)) (λ (y) (and (real? y) (</> y x))))
   #:late-neg-projection
   (λ (ctc)
     (define x (</>-ctc-x ctc))
     (λ (blame)
       (λ (val neg-party)
         (if (and (real? val) (</> val x))
             val
             (raise-blame-error
              blame val #:missing-party neg-party
              '(expected:
                "a number strictly ~a than ~v"
                given: "~v")
              less/greater
              x
              val)))))
   #:generate
   (λ (ctc)
     (define x (</>-ctc-x ctc))
     (λ (fuel)
       (λ ()
         (rand-choice
          [1/10 (-/+ +inf.0)]
          [1/10 (-/+ x 0.01)]
          [4/10 (-/+ x (random))]
          [else (-/+ x (random 4294967087))]))))
   #:stronger </>-ctc-stronger))

(define (</>-ctc-stronger this that)
  (define this-x (</>-ctc-x this))
  (cond
    [(</>-ctc? that)
     (cond
       [(and (<-ctc? this) (<-ctc? that))
        (<= this-x (</>-ctc-x that))]
       [(and (>-ctc? this) (>-ctc? that))
        (>= this-x (</>-ctc-x that))])]
    [(between/c-s? that)
     (cond
       [(<-ctc? this)
        (and (= (between/c-s-low that) -inf.0)
             (<= this-x (between/c-s-high that)))]
       [else
        (and (= (between/c-s-high that) +inf.0)
             (<= (between/c-s-low that) this-x))])]))


(struct </>-ctc (x))
(struct <-ctc </>-ctc ()
  #:property prop:flat-contract
  (make-</c->/c-contract-property '</c < - "less")
  #:property prop:custom-write custom-write-property-proc)
(define (</c x) (<-ctc x))
(struct >-ctc </>-ctc ()
  #:property prop:flat-contract
  (make-</c->/c-contract-property '>/c > + "greater")
  #:property prop:custom-write custom-write-property-proc)
(define (>/c x) (>-ctc x))

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
(struct integer-in-ctc (start end)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (ctc) 
            `(integer-in ,(integer-in-ctc-start ctc)
                         ,(integer-in-ctc-end ctc)))
   #:first-order (λ (ctc)
                   (define start (integer-in-ctc-start ctc))
                   (define end (integer-in-ctc-end ctc))
                   (λ (x) (and (exact-integer? x)
                               (<= start x end))))
   #:stronger (λ (this that)
                (define this-start (integer-in-ctc-start this))
                (define this-end (integer-in-ctc-end that))
                (cond
                  [(integer-in-ctc? that)
                   (define that-start (integer-in-ctc-start that))
                   (define that-end (integer-in-ctc-end that))
                   (<= that-start this-start this-end that-end)]
                  [else #f]))
   #:generate (λ (ctc)
                (define start (integer-in-ctc-start ctc))
                (define end (integer-in-ctc-end ctc))
                (λ (fuel)
                  (λ ()
                    (+ start (random (min 4294967087 (+ (- end start) 1)))))))))

(define/final-prop (integer-in start end)
  (check-two-args 'integer-in start end exact-integer? exact-integer?)
  (if (= start end)
      (and/c start exact?)
      (integer-in-ctc start end)))

(define (char-in a b)
  (check-two-args 'char-in a b char? char?)
  (char-in/c a b))

(define/final-prop (real-in start end)
  (check-two-args 'real-in start end real? real?)
  (make-real-in-s start end))

(define/final-prop (not/c f)
  (let* ([ctc (coerce-flat-contract 'not/c f)]
         [pred (flat-contract-predicate ctc)])
    (flat-named-contract
     (build-compound-type-name 'not/c ctc)
     (λ (x) (not (pred x))))))

(define (listof-generate ctc)
  (λ (fuel)
    (define eg (contract-random-generate/choose (listof-ctc-elem-c ctc) fuel))
    (if eg
        (λ ()
          (let loop ([so-far (cond
                               [(pe-listof-ctc? ctc)
                                '()]
                               [(ne-listof-ctc? ctc)
                                (list (eg))]
                               [else
                                ;; improper list
                                (eg)])])
            (rand-choice
             [1/5 so-far]
             [else (loop (cons (eg) so-far))])))
        (if (pe-listof-ctc? ctc)
            (λ () '())
            #f))))

(define (listof-exercise ctc) 
  (cond
    [(pe-listof-ctc? ctc)
     (λ (fuel) (values void '()))]
    [else
     (define elem-ctc (listof-ctc-elem-c ctc))
     (λ (fuel)
       (define env (contract-random-generate-get-current-environment))
       (values
        (λ (lst)
          (contract-random-generate-stash
           env elem-ctc 
           (oneof
            (if (im-listof-ctc? ctc)
                (improper-list->list lst)
                lst))))
        (list elem-ctc)))]))

(define (improper-list->list l)
  (cond
    [(pair? l) (cons (car l) (improper-list->list (cdr l)))]
    [else (list l)]))

(define (listof-stronger this that)
  (define this-elem (listof-ctc-elem-c this))
  (cond
    [(listof-ctc? that)
     (define that-elem (listof-ctc-elem-c that))
     (and (cond
            [(pe-listof-ctc? this) (pe-listof-ctc? that)]
            [(im-listof-ctc? this) (im-listof-ctc? that)]
            [else #t])
          (contract-struct-stronger? this-elem that-elem))]
    [(the-cons/c? that)
     (define hd-ctc (the-cons/c-hd-ctc that))
     (define tl-ctc (the-cons/c-tl-ctc that))
     (and (ne-listof-ctc? this)
          (contract-struct-stronger? this-elem hd-ctc)
          (contract-struct-stronger? (ne->pe-ctc this) tl-ctc))]
    [else #f]))
           
(define (raise-listof-blame-error blame val empty-ok? neg-party)
  (raise-blame-error blame #:missing-party neg-party val
                     '(expected: "~s" given: "~e")
                     (if empty-ok?
                         'list?
                         (format "~s" `(and/c list? pair?)))
                     val))

(define (blame-add-listof-context blame) (blame-add-context blame "an element of"))
(define (non-empty-list? x) (and (pair? x) (list? x)))

(define (list-name ctc)
  (build-compound-type-name (cond
                              [(pe-listof-ctc? ctc)
                               'listof]
                              [(ne-listof-ctc? ctc)
                               'non-empty-listof]
                              [(im-listof-ctc? ctc)
                               'list*of])
                            (listof-ctc-elem-c ctc)))

(define (list-fo-check ctc)
  (define elem-fo? (contract-first-order (listof-ctc-elem-c ctc)))
  (cond
    [(pe-listof-ctc? ctc)
     (λ (v)
       (and (list? v)
            (for/and ([e (in-list v)])
              (elem-fo? e))))]
    [(ne-listof-ctc? ctc)
     (λ (v)
       (and (list? v)
            (pair? v)
            (for/and ([e (in-list v)])
              (elem-fo? e))))]
    [(im-listof-ctc? ctc)
     (λ (v)
       (let loop ([v v])
         (cond
           [(pair? v) 
            (and (elem-fo? (car v))
                 (loop (cdr v)))]
           [else
            (elem-fo? v)])))]))

(define (listof-late-neg-projection ctc)
  (define elem-proj (get/build-late-neg-projection (listof-ctc-elem-c ctc)))
  (define pred? (if (pe-listof-ctc? ctc)
                    list?
                    non-empty-list?))
  (λ (blame)
    (define elem-proj+blame (elem-proj (blame-add-listof-context blame)))
    (cond
      [(flat-listof-ctc? ctc)
       (if (im-listof-ctc? ctc)
           (λ (val neg-party)
             (let loop ([val val])
               (cond
                 [(pair? val)
                  (elem-proj+blame (car val) neg-party)
                  (loop (cdr val))]
                 [else 
                  (elem-proj+blame val neg-party)]))
             val)
           (λ (val neg-party)
             (cond
               [(pred? val)
                (for ([x (in-list val)])
                  (elem-proj+blame x neg-party))
                val]
               [else
                (raise-listof-blame-error blame val (pe-listof-ctc? ctc) neg-party)])))]
      [else
       (if (im-listof-ctc? ctc)
           (λ (val neg-party)
             (let loop ([val val])
               (cond
                 [(pair? val)
                  (cons (elem-proj+blame (car val) neg-party)
                        (loop (cdr val)))]
                 [else 
                  (elem-proj+blame val neg-party)])))
           (λ (val neg-party)
             (if (pred? val)
                 (for/list ([x (in-list val)])
                   (elem-proj+blame x neg-party))
                 (raise-listof-blame-error blame val (pe-listof-ctc? ctc) neg-party))))])))

(define flat-prop
  (build-flat-contract-property
   #:name list-name
   #:first-order list-fo-check
   #:late-neg-projection listof-late-neg-projection
   #:generate listof-generate
   #:exercise listof-exercise
   #:stronger listof-stronger
   #:list-contract? (λ (c) (not (im-listof-ctc? c)))))
(define chap-prop
  (build-chaperone-contract-property
   #:name list-name
   #:first-order list-fo-check
   #:late-neg-projection listof-late-neg-projection
   #:generate listof-generate
   #:exercise listof-exercise
   #:stronger listof-stronger
   #:list-contract? (λ (c) (not (im-listof-ctc? c)))))
(define full-prop
  (build-contract-property
   #:name list-name
   #:first-order list-fo-check
   #:late-neg-projection listof-late-neg-projection
   #:generate listof-generate
   #:exercise listof-exercise
   #:stronger listof-stronger
   #:list-contract? (λ (c) (not (im-listof-ctc? c)))))

(struct listof-ctc (elem-c))

;; possibly-empty lists
(struct pe-listof-ctc listof-ctc ())

;; possibly-empty, flat
(struct pef-listof-ctc pe-listof-ctc ()
  #:property prop:flat-contract flat-prop
  #:property prop:custom-write custom-write-property-proc)
;; possibly-empty, chaperone
(struct pec-listof-ctc pe-listof-ctc ()
  #:property prop:chaperone-contract chap-prop
  #:property prop:custom-write custom-write-property-proc)
;; possibly-empty, impersonator
(struct pei-listof-ctc pe-listof-ctc ()
  #:property prop:contract full-prop
  #:property prop:custom-write custom-write-property-proc)

;; non-empty lists
(struct ne-listof-ctc listof-ctc ())

;; non-empty, flat
(struct nef-listof-ctc ne-listof-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract flat-prop)
;; non-empty, chaperone
(struct nec-listof-ctc ne-listof-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract chap-prop)
;; non-empty, impersonator
(struct nei-listof-ctc ne-listof-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract full-prop)

;; improper lists
(struct im-listof-ctc listof-ctc ())

;; improper, flat
(struct imf-listof-ctc im-listof-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract flat-prop)
;; improper, chaperone
(struct imc-listof-ctc im-listof-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract chap-prop)
;; improper, impersonator
(struct imi-listof-ctc im-listof-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract full-prop)

(define (flat-listof-ctc? x)
  (or (pef-listof-ctc? x)
      (nef-listof-ctc? x)
      (imf-listof-ctc? x)))

(define (ne->pe-ctc ne-ctc)
  (define elem-ctc (listof-ctc-elem-c ne-ctc))
  (cond
    [(nef-listof-ctc? ne-ctc)
     (pef-listof-ctc elem-ctc)]
    [(nef-listof-ctc? ne-ctc)
     (pef-listof-ctc elem-ctc)]
    [(nei-listof-ctc? ne-ctc)
     (pei-listof-ctc elem-ctc)]))

(define/subexpression-pos-prop (non-empty-listof raw-c)
  (define c (coerce-contract 'non-empty-listof raw-c))
  (cond
    [(flat-contract? c) (nef-listof-ctc c)]
    [(chaperone-contract? c) (nec-listof-ctc c)]
    [else (nei-listof-ctc c)]))
(define/subexpression-pos-prop (listof raw-c)
  (define c (coerce-contract 'listof raw-c))
  (cond
    [(flat-contract? c) (pef-listof-ctc c)]
    [(chaperone-contract? c) (pec-listof-ctc c)]
    [else (pei-listof-ctc c)]))
(define/subexpression-pos-prop (list*of raw-c)
  (define c (coerce-contract 'list*of raw-c))
  (cond
    [(flat-contract? c) (imf-listof-ctc c)]
    [(chaperone-contract? c) (imc-listof-ctc c)]
    [else (imi-listof-ctc c)]))


(define (blame-add-car-context blame) (blame-add-context blame "the car of"))
(define (blame-add-cdr-context blame) (blame-add-context blame "the cdr of"))

(define ((cons/c-late-neg-ho-check combine) ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (define car-late-neg-proj (get/build-late-neg-projection ctc-car))
  (define cdr-late-neg-proj (get/build-late-neg-projection ctc-cdr))
  (λ (blame)
    (define car-p (car-late-neg-proj (blame-add-car-context blame)))
    (define cdr-p (cdr-late-neg-proj (blame-add-cdr-context blame)))
    (λ (v neg-party)
      (unless (pair? v)
        (raise-not-cons-blame-error blame #:missing-party neg-party v))
      (combine v
               (car-p (car v) neg-party)
               (cdr-p (cdr v) neg-party)))))

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
  (cond
    [(and (any/c? ctc-car) (any/c? ctc-cdr))
     'pair?]
    [else
     (build-compound-type-name 'cons/c ctc-car ctc-cdr)]))

(define (cons/c-stronger? this that)
  (define this-hd (the-cons/c-hd-ctc this))
  (define this-tl (the-cons/c-tl-ctc this))
  (cond
    [(the-cons/c? that)
     (define that-hd (the-cons/c-hd-ctc that))
     (define that-tl (the-cons/c-tl-ctc that))
     (and (contract-struct-stronger? this-hd that-hd)
          (contract-struct-stronger? this-tl that-tl))]
    [(ne-listof-ctc? that)
     (define elem-ctc (listof-ctc-elem-c that))
     (and (contract-struct-stronger? this-hd elem-ctc)
          (contract-struct-stronger? this-tl (ne->pe-ctc that)))]
    [(pe-listof-ctc? that)
     (define elem-ctc (listof-ctc-elem-c that))
     (and (contract-struct-stronger? this-hd elem-ctc)
          (contract-struct-stronger? this-tl that))]
    [else #f]))


(define (cons/c-generate ctc)
  (define ctc-car (the-cons/c-hd-ctc ctc))
  (define ctc-cdr (the-cons/c-tl-ctc ctc))
  (λ (fuel)
    (define car-gen (contract-random-generate/choose ctc-car fuel))
    (define cdr-gen (contract-random-generate/choose ctc-cdr fuel))
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
   #:late-neg-projection (cons/c-late-neg-ho-check (λ (v a d) v))
   #:name cons/c-name
   #:first-order cons/c-first-order
   #:stronger cons/c-stronger?
   #:generate cons/c-generate
   #:list-contract? cons/c-list-contract?))
(define-struct (chaperone-cons/c the-cons/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection (cons/c-late-neg-ho-check (λ (v a d) (cons a d)))
   #:name cons/c-name
   #:first-order cons/c-first-order
   #:stronger cons/c-stronger?
   #:generate cons/c-generate
   #:list-contract? cons/c-list-contract?))
(define-struct (impersonator-cons/c the-cons/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (cons/c-late-neg-ho-check (λ (v a d) (cons a d)))
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

(define (cons/dc-late-neg-projection ctc)
  (define undep-proj (get/build-late-neg-projection (the-cons/dc-undep ctc)))
  (define dep-proc (the-cons/dc-dep ctc))
  (define forwards? (the-cons/dc-forwards? ctc))
  (λ (blame)
    (define car-blame (blame-add-car-context blame))
    (define cdr-blame (blame-add-cdr-context blame))
    (define undep-proj+blame (undep-proj (if forwards? car-blame cdr-blame)))
    (define undep-proj+indy-blame
      (undep-proj (blame-replace-negative
                   (if forwards? cdr-blame car-blame)
                   (the-cons/dc-here ctc))))
    (λ (val neg-party)
      (cond
        [(pair? val)
         (define-values (orig-undep orig-dep)
           (if forwards?
               (values (car val) (cdr val))
               (values (cdr val) (car val))))
         (define new-undep (undep-proj+blame orig-undep neg-party))
         (define new-dep-ctc (coerce-contract
                              'cons/dc
                              (dep-proc (undep-proj+indy-blame orig-undep neg-party))))
         (define new-dep (((get/build-late-neg-projection new-dep-ctc)
                           (if forwards? cdr-blame car-blame))
                          orig-dep
                          neg-party))
         (if forwards?
             (cons new-undep new-dep)
             (cons new-dep new-undep))]
        [else
         (raise-not-cons-blame-error blame val #:missing-party neg-party)]))))

(define (cons/dc-name ctc)
  (define info (the-cons/dc-name-info ctc))
  (if (the-cons/dc-forwards? ctc)
      `(cons/dc [,(vector-ref info 0) ,(contract-name (the-cons/dc-undep ctc))]
                [,(vector-ref info 1) (,(vector-ref info 0))
                                      ,(vector-ref info 2)])
      `(cons/dc [,(vector-ref info 0) (,(vector-ref info 1))
                                      ,(vector-ref info 2)]
                [,(vector-ref info 1) ,(contract-name (the-cons/dc-undep ctc))])))
(define (cons/dc-first-order ctc)
  (λ (val)
    (and (pair? val)
         (contract-first-order-passes?
          (the-cons/dc-undep ctc)
          (if (the-cons/dc-forwards? ctc) (car val) (cdr val))))))

(define (cons/dc-stronger? this that) #f)

(define (cons/dc-generate ctc)
  (define undep-ctc (the-cons/dc-undep ctc))
  (define dep-mk-ctc (the-cons/dc-dep ctc))
  (define forwards? (the-cons/dc-forwards? ctc))
  (λ (fuel)
    (define undep-gen (contract-random-generate/choose undep-ctc fuel))
    (define pair-gens
      (for*/list ([i (in-range 5)]
                  [v (in-value (undep-gen))]
                  [g (in-value (contract-random-generate/choose (dep-mk-ctc v) fuel))]
                  #:when g)
        (if forwards?
            (λ () (cons v (g)))
            (λ () (cons (g) v)))))
    (define howmany (length pair-gens))
    (and (not (zero? howmany))
         (λ ()
           ((list-ref pair-gens (random howmany)))))))

(struct the-cons/dc (forwards? undep dep here name-info))

(struct flat-cons/dc the-cons/dc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:late-neg-projection cons/dc-late-neg-projection
   #:name cons/dc-name
   #:first-order cons/dc-first-order
   #:stronger cons/dc-stronger?
   #:generate cons/dc-generate))

(struct chaperone-cons/dc the-cons/dc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection cons/dc-late-neg-projection
   #:name cons/dc-name
   #:first-order cons/dc-first-order
   #:stronger cons/dc-stronger?
   #:generate cons/dc-generate))

(struct impersonator-cons/dc the-cons/dc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection cons/dc-late-neg-projection
   #:name cons/dc-name
   #:first-order cons/dc-first-order
   #:stronger cons/dc-stronger?
   #:generate cons/dc-generate))

(define-syntax (cons/dc stx)
  (define (kwds->constructor stx)
    (syntax-case stx ()
      [() #'chaperone-cons/dc]
      [(#:chaperone) #'chaperone-cons/dc]
      [(#:flat) #'flat-cons/dc]
      [(#:impersonator) #'impersonator-cons/dc]
      [(x . y) (raise-syntax-error
                'cons/dc
                "expected a keyword, either #:chaperone, #:flat, or #:impersonator"
                stx
                #'x)]))
  (define this-one (gensym 'ctc))
  (syntax-property
   (syntax-case stx ()
     [(_ [hd e1] [tl (hd2) e2] . kwds)
      (begin
        (unless (free-identifier=? #'hd2 #'hd)
          (raise-syntax-error 'cons/dc
                              "expected matching identifiers"
                              stx
                              #'hd
                              (list #'hd2)))
        #`(#,(kwds->constructor #'kwds)
           #t
           (coerce-contract 'cons/dc #,(syntax-property
                                        #'e1
                                        'racket/contract:positive-position
                                        this-one))
           (λ (hd2) #,(syntax-property
                       #'e2
                       'racket/contract:positive-position
                       this-one))
           (quote-module-name)
           '#(hd tl #,(compute-quoted-src-expression #'e2))))]
     [(_ [hd (tl2) e1] [tl e2] . kwds)
      (begin
        (unless (free-identifier=? #'tl2 #'tl)
          (raise-syntax-error 'cons/dc
                              "expected matching identifiers"
                              stx
                              #'tl
                              (list #'tl2)))
        #`(#,(kwds->constructor #'kwds)
           #f
           (coerce-contract 'cons/dc #,(syntax-property
                                        #'e2
                                        'racket/contract:positive-position
                                        this-one))
           (λ (tl2) #,(syntax-property
                       #'e1
                       'racket/contract:positive-position
                       this-one))
           (quote-module-name)
           '#(hd tl #,(compute-quoted-src-expression #'e1))))])
   'racket/contract:contract
   (vector this-one
           (list (car (syntax-e stx)))
           '())))


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
  (define args (generic-list/c-args c))
  (cond
    [(null? args) ''()]
    [else (apply build-compound-type-name 'list/c args)]))

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
                   (contract-random-generate/choose elem-ctc fuel)))
    (cond
      [(andmap values gens)
       (λ ()
         (for/list ([gen (in-list gens)])
           (gen)))]
      [else
       #f])))

(define (list/c-exercise ctc)
  (multi-exercise (generic-list/c-args ctc)))

(define (list/c-stronger this that)
  (cond
    [(generic-list/c? that)
     (pairwise-stronger-contracts? (generic-list/c-args this)
                                   (generic-list/c-args that))]
    [(listof-ctc? that)
     (define that-elem-ctc (listof-ctc-elem-c that))
     (define this-elem-ctcs (generic-list/c-args this))
     (and (or (pair? this-elem-ctcs)
              (pe-listof-ctc? that))
          (for/and ([this-s (in-list this-elem-ctcs)])
            (contract-struct-stronger? this-s that-elem-ctc)))]
    [else #f]))

(struct generic-list/c (args))

(struct flat-list/c generic-list/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:generate list/c-generate
   #:exercise list/c-exercise
   #:stronger list/c-stronger
   #:late-neg-projection
   (λ (c) 
     (λ (blame)
       (define projs 
         (for/list ([ctc (in-list (generic-list/c-args c))]
                    [i (in-naturals 1)])
           ((get/build-late-neg-projection ctc)
            (add-list-context blame i))))
       (define args (generic-list/c-args c))
       (define expected-length (length args))
       (λ (val neg-party) 
         (cond
           [(list? val)
            (define actual-length (length val))
            (cond
              [(= actual-length expected-length)
               (for ([proj (in-list projs)]
                     [ele (in-list val)])
                 (proj ele neg-party))
               val]
              [else
               (expected-a-list-of-len val actual-length expected-length blame 
                                       #:missing-party neg-party)])]
           [else
            (raise-blame-error blame #:missing-party neg-party
                               val
                               '(expected "a list" given: "~e") 
                               val)]))))
   #:list-contract? (λ (c) #t)))

(define (expected-a-list x blame #:missing-party [missing-party #f])
  (raise-blame-error blame #:missing-party missing-party
                     x '(expected: "a list" given: "~e") x))

(define (expected-a-list-of-len x actual expected blame #:missing-party [missing-party #f])
  (unless (= actual expected)
    (cond
      [(null? x)
       (raise-blame-error
        blame #:missing-party missing-party x
        '(expected: "a list of ~a element~a" given: "~e")
        expected
        (if (= expected 1) "" "s")
        x)]
      [else
       (raise-blame-error
        blame #:missing-party missing-party x
        '(expected: "a list of ~a element~a" given: "~a element~a\n  complete list: ~e")
        expected
        (if (= expected 1) "" "s")
        actual
        (if (= actual 1) "" "s")
        x)])))

(define (list/c-chaperone/other-late-neg-projection c)
  (define projs (map get/build-late-neg-projection (generic-list/c-args c)))
  (define expected (length projs))
  (λ (blame)
    (define p-apps (for/list ([proj (in-list projs)] 
                              [i (in-naturals 1)])
                     (proj (add-list-context blame i))))
    (λ (val neg-party)
      (cond
        [(list? val)
         (define actual (length val))
         (cond
           [(= actual expected)
            (for/list ([item (in-list val)]
                       [p-app (in-list p-apps)])
              (p-app item neg-party))]
           [else
            (expected-a-list-of-len val actual expected blame
                                    #:missing-party neg-party)])]
        [else
         (expected-a-list val blame #:missing-party neg-party)]))))

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
  (build-chaperone-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:generate list/c-generate
   #:exercise list/c-exercise
   #:stronger list/c-stronger
   #:late-neg-projection list/c-chaperone/other-late-neg-projection
   #:list-contract? (λ (c) #t)))

(struct higher-order-list/c generic-list/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:generate list/c-generate
   #:exercise list/c-exercise
   #:stronger list/c-stronger
   #:late-neg-projection list/c-chaperone/other-late-neg-projection
   #:list-contract? (λ (c) #t)))

(struct syntax-ctc (ctc)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (ctc) (build-compound-type-name 'syntax/c (syntax-ctc-ctc ctc)))
   #:stronger (λ (this that)
                (and (syntax-ctc? that)
                     (contract-struct-stronger? (syntax-ctc-ctc this)
                                                (syntax-ctc-ctc that))))
   #:first-order (λ (ctc) 
                   (define ? (flat-contract-predicate (syntax-ctc-ctc ctc)))
                   (λ (v)
                     (and (syntax? v)
                          (? (syntax-e v)))))))

(define/subexpression-pos-prop (syntax/c ctc-in)
  (define ctc (coerce-flat-contract 'syntax/c ctc-in))
  (syntax-ctc ctc))

(define/subexpression-pos-prop promise/c
  (λ (ctc-in)
    (define ctc (coerce-contract 'promise/c ctc-in))
    (cond
      [(chaperone-contract? ctc)
       (chaperone-promise-ctc ctc)]
      [else
       (promise-ctc ctc)])))

(define (promise-contract-late-neg-proj ctc)
  (define chap? (chaperone-promise-ctc? ctc))
  (define c/i-struct (if chap? chaperone-struct impersonate-struct))
  (define c/i-procedure (if chap? chaperone-procedure impersonate-procedure))
  (define ctc-proc (get/build-late-neg-projection (promise-base-ctc-ctc ctc)))
  (λ (blame)
    (define p-app (ctc-proc (blame-add-context blame "the promise from")))
    (λ (val neg-party)
      (if (promise? val)
          (c/i-struct
           val
           promise-forcer
           (λ (_ proc)
             (c/i-procedure
              proc
              (λ (promise)
                (values (λ (val) (p-app val neg-party)) promise)))))
          (raise-blame-error
           blame #:missing-party neg-party
           val
           '(expected: "<promise>" given: "~e")
           val)))))

(define (promise-contract-name ctc)
  (build-compound-type-name 'promise/c (promise-base-ctc-ctc ctc)))

(define (promise-ctc-stronger? this that)
  (and (promise-base-ctc? that)
       (contract-struct-stronger? (promise-base-ctc-ctc this)
                                  (promise-base-ctc-ctc that))))

(struct promise-base-ctc (ctc))
(struct chaperone-promise-ctc promise-base-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name promise-contract-name
   #:late-neg-projection promise-contract-late-neg-proj
   #:stronger promise-ctc-stronger?
   #:first-order (λ (ctc) promise?)))
(struct promise-ctc promise-base-ctc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name promise-contract-name
   #:late-neg-projection promise-contract-late-neg-proj
   #:stronger promise-ctc-stronger?
   #:first-order (λ (ctc) promise?)))

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
   #:late-neg-projection
   (λ (ctc)
     (define in-proc (get/build-late-neg-projection (parameter/c-in ctc)))
     (define out-proc (get/build-late-neg-projection (parameter/c-out ctc)))
     (λ (blame)
       (define blame/c (blame-add-context blame "the parameter of"))
       (define in-proj (in-proc (blame-swap blame/c)))
       (define out-proj (out-proc blame/c))
       (λ (val neg-party)
         (cond
           [(parameter? val)
            (define (add-profiling f)
              (λ (x)
                (with-contract-continuation-mark
                 (cons blame/c neg-party)
                 (f x neg-party))))
            (make-derived-parameter
             val
             (add-profiling in-proj)
             (add-profiling out-proj))]
           [else
            (raise-blame-error blame #:missing-party neg-party
                               val '(expected "a parameter"))]))))

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
           (and (contract-struct-stronger? (parameter/c-out this)
                                           (parameter/c-out that))
                (contract-struct-stronger? (parameter/c-in that)
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

(define (get-any? c) any?)
(define (any? x) #t)
(define any/c-blame->neg-party-fn (λ (blame) any/c-neg-party-fn))
(define any/c-neg-party-fn (λ (val neg-party) val))

(define (random-any/c env fuel)
  (define env-hash (contract-random-generate-env-hash env))
  (cond
    [(zero? (hash-count env-hash))
     (rand-choice
      [1/3 (any/c-simple-value)]
      [1/3 (any/c-procedure env-hash fuel)]
      [else (any/c-from-predicate-generator env-hash fuel)])]
    [else
     (rand-choice
      [1/4 (oneof (hash-ref env-hash (oneof (hash-keys env-hash))))]
      [1/4 (any/c-simple-value)]
      [1/4 (any/c-procedure env-hash fuel)]
      [else (any/c-from-predicate-generator env-hash fuel)])]))

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
  #:property prop:any/c #f
  #:property prop:flat-contract
  (build-flat-contract-property
   #:late-neg-projection (λ (ctc) any/c-blame->neg-party-fn)
   #:stronger (λ (this that) (any/c? that))
   #:name (λ (ctc) 'any/c)
   #:generate (λ (ctc) 
                (λ (fuel) 
                  (define env (contract-random-generate-get-current-environment))
                  (λ () (random-any/c env fuel))))
   #:first-order get-any?))

(define/final-prop any/c (make-any/c))

(define-syntax (any stx)
  (raise-syntax-error 'any "use of 'any' outside the range of an arrow contract" stx))

(define (((none-curried-late-neg-proj ctc) blame) val neg-party)
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
   #:late-neg-projection none-curried-late-neg-proj
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
(define ((prompt-tag/c-late-neg-proj chaperone?) ctc)
  (define proxy (if chaperone? chaperone-prompt-tag impersonate-prompt-tag))
  (define proc-proxy (if chaperone? chaperone-procedure impersonate-procedure))
  (define ho-projs
    (map get/build-late-neg-projection (base-prompt-tag/c-ctcs ctc)))
  (define call/cc-projs
    (map get/build-late-neg-projection (base-prompt-tag/c-call/ccs ctc)))
  (λ (blame)
    (define swapped (blame-swap blame))
    (define ho-neg-projs (for/list ([proj (in-list ho-projs)]) (proj swapped)))
    (define ho-pos-projs (for/list ([proj (in-list ho-projs)]) (proj blame)))
    (define cc-neg-projs (for/list ([proj (in-list call/cc-projs)]) (proj swapped)))
    (define cc-pos-projs (for/list ([proj (in-list call/cc-projs)]) (proj blame)))
    (define (make-proj projs neg-party)
      (λ vs
        (apply values
               (for/list ([proj (in-list projs)]
                          [v (in-list vs)])
                 (proj v neg-party)))))
    (λ (val neg-party)
      ;; now do the actual wrapping
      (cond
        [(continuation-prompt-tag? val)
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
                impersonator-prop:blame (blame-add-missing-party blame neg-party))]
        [else
         (raise-blame-error
          blame #:missing-party neg-party val
          '(expected: "~s" given: "~e")
          (contract-name ctc)
          val)]))))

(define (prompt-tag/c-stronger? this that)
  (and (base-prompt-tag/c? that)
       (andmap (λ (this that) (contract-struct-stronger? this that))
               (base-prompt-tag/c-ctcs this)
               (base-prompt-tag/c-ctcs that))
       (andmap (λ (this that) (contract-struct-stronger? this that))
               (base-prompt-tag/c-call/ccs this)
               (base-prompt-tag/c-call/ccs that))))

;; (listof contract) (listof contract)
(define-struct base-prompt-tag/c (ctcs call/ccs))

(define-struct (chaperone-prompt-tag/c base-prompt-tag/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection (prompt-tag/c-late-neg-proj #t)
   #:first-order (λ (ctc) continuation-prompt-tag?)
   #:stronger prompt-tag/c-stronger?
   #:name prompt-tag/c-name))

(define-struct (impersonator-prompt-tag/c base-prompt-tag/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (prompt-tag/c-late-neg-proj #f)
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

(define ((continuation-mark-key/c-late-neg-proj proxy) ctc)
  (define ho-proj
    (get/build-late-neg-projection (base-continuation-mark-key/c-ctc ctc)))
  (λ (blame)
    (define swapped (blame-swap blame))
    (define proj1 (ho-proj blame))
    (define proj2 (ho-proj (blame-swap blame)))
    (λ (val neg-party)
      (cond
        [(continuation-mark-key? val)
         (proxy val 
                (λ (v) (proj1 v neg-party))
                (λ (v) (proj2 v neg-party))
                impersonator-prop:contracted ctc
                impersonator-prop:blame blame)]
        [else 
         (unless (contract-first-order-passes? ctc val)
           (raise-blame-error
            blame #:missing-party neg-party
            val
            '(expected: "~s" given: "~e")
            (contract-name ctc)
            val))]))))

(define (continuation-mark-key/c-stronger? this that)
  (and (base-continuation-mark-key/c? that)
       (contract-struct-stronger?
        (base-continuation-mark-key/c-ctc this)
        (base-continuation-mark-key/c-ctc that))))

(define-struct base-continuation-mark-key/c (ctc))

(define-struct (chaperone-continuation-mark-key/c
                base-continuation-mark-key/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection (continuation-mark-key/c-late-neg-proj chaperone-continuation-mark-key)
   #:first-order (λ (ctc) continuation-mark-key?)
   #:stronger continuation-mark-key/c-stronger?
   #:name continuation-mark-key/c-name))

(define-struct (impersonator-continuation-mark-key/c
                base-continuation-mark-key/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (continuation-mark-key/c-late-neg-proj impersonate-continuation-mark-key)
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
    (λ (val neg-party)
      (unless (contract-first-order-passes? evt-ctc val)
        (raise-blame-error
         blame val #:missing-party neg-party
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
  (pairwise-stronger-contracts? this-ctcs that-ctcs))

;; ctcs - Listof<Contract>
(define-struct chaperone-evt/c (ctcs)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection evt/c-proj
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

(define ((channel/c-late-neg-proj proxy) ctc)
  (define ho-proj
    (get/build-late-neg-projection (base-channel/c-ctc ctc)))
  (λ (blame)
    (define pos-proj (ho-proj blame))
    (define neg-proj (ho-proj (blame-swap blame)))
    (define (proj1 neg-party) (λ (ch) (values ch (λ (v) (pos-proj v neg-party)))))
    (define (proj2 neg-party) (λ (ch v) (neg-proj v neg-party)))
    (λ (val neg-party)
      (cond
        [(channel? val)
         (proxy val 
                (proj1 neg-party)
                (proj2 neg-party)
                impersonator-prop:contracted ctc
                impersonator-prop:blame blame)]
        [else
         (raise-blame-error
          blame #:missing-party neg-party
          val '(expected: "~s" given: "~e")
          (contract-name ctc)
          val)]))))

(define (channel/c-first-order ctc) channel?)

(define (channel/c-stronger? this that)
  (and (base-channel/c? that)
       (contract-struct-stronger?
        (base-channel/c-ctc this)
        (base-channel/c-ctc that))))

(define-struct base-channel/c (ctc))

(define-struct (chaperone-channel/c base-channel/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection (channel/c-late-neg-proj chaperone-channel)
   #:first-order channel/c-first-order
   #:stronger channel/c-stronger?
   #:name channel/c-name))

(define-struct (impersonator-channel/c base-channel/c)
  ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (channel/c-late-neg-proj impersonate-channel)
   #:first-order channel/c-first-order
   #:stronger channel/c-stronger?
   #:name channel/c-name))


(define (flat-contract-predicate x)
  (contract-struct-first-order
   (coerce-flat-contract 'flat-contract-predicate x)))

(define (flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (flat-named-contract name pre-contract [generate #f])
  (cond
    [(and (not generate)
          (coerce-contract/f pre-contract name))
     =>
     values]
    [(flat-contract? pre-contract)
     (make-predicate-contract name (flat-contract-predicate pre-contract) generate #f)]
    [else
     (raise-argument-error 'flat-named-contract
                           "flat-contract?"
                           pre-contract)]))

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

;; this is a hack to work around cyclic linking issues;
;; see definition of set-some-basic-contracts!
(set-some-basic-contracts!
 (listof any/c)
 (cons/c any/c any/c)
 (list/c))

;; rename-contract : contract any/c -> contract
;; If the argument is a flat contract, so is the result.
(define (rename-contract ctc name)
  (unless (contract? ctc)
    (raise-type-error 'rename-contract "contract?" ctc))
  (let ([ctc (coerce-contract 'rename-contract ctc)])
    (if (flat-contract? ctc)
        (flat-named-contract name (flat-contract-predicate ctc))
        (let* ([make-contract (if (chaperone-contract? ctc) make-chaperone-contract make-contract)])
          (define (stronger? this other)
            (contract-struct-stronger? ctc other))
          (make-contract #:name name
                         #:late-neg-projection (get/build-late-neg-projection ctc)
                         #:first-order (contract-first-order ctc)
                         #:stronger stronger?
                         #:list-contract? (list-contract? ctc))))))

(define (if/c predicate then/c else/c)
  (unless (procedure? predicate)
    (raise-type-error 'if/c "procedure?" predicate))
  (unless (procedure-arity-includes? predicate 1)
    (raise-type-error 'if/c "procedure that accepts 1 argument" predicate))
  (define then-ctc (coerce-contract 'if/c then/c))
  (define else-ctc (coerce-contract 'if/c else/c))
  (cond
    [(and (flat-contract? then-ctc)
          (flat-contract? else-ctc))
     (define then-pred (flat-contract-predicate then-ctc))
     (define else-pred (flat-contract-predicate else-ctc))
     (define name `(if/c ,(object-name predicate)
                         ,(contract-name then-pred)
                         ,(contract-name else-pred)))
     (define (pred x)
       (if (predicate x) (then-pred x) (else-pred x)))
     (flat-named-contract name pred)]
    [(and (chaperone-contract? then-ctc)
          (chaperone-contract? else-ctc))
     (chaperone-if/c predicate then-ctc else-ctc)]
    [else
     (impersonator-if/c predicate then-ctc else-ctc)]))

(define (if/c-first-order ctc)
  (define predicate (base-if/c-predicate ctc))
  (define thn (contract-first-order (base-if/c-thn ctc)))
  (define els (contract-first-order (base-if/c-els ctc)))
  (λ (x) (if (predicate x) (thn x) (els x))))

(define (if/c-name ctc)
  (define predicate (base-if/c-predicate ctc))
  (define thn (contract-name (base-if/c-thn ctc)))
  (define els (contract-name (base-if/c-els ctc)))
  `(if/c ,(object-name predicate) ,thn ,els))

(define (if/c-late-neg-proj ctc)
  (define predicate (base-if/c-predicate ctc))
  (define thn (get/build-late-neg-projection (base-if/c-thn ctc)))
  (define els (get/build-late-neg-projection (base-if/c-els ctc)))
  (λ (blame)
    (define thn-proj (thn blame))
    (define els-proj (els blame))
    (λ (val neg-party)
      (if (predicate val)
          (thn-proj val neg-party)
          (els-proj val neg-party)))))

(define-struct base-if/c (predicate thn els)
  #:property prop:custom-write custom-write-property-proc)
(define-struct (chaperone-if/c base-if/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection if/c-late-neg-proj
   #:first-order if/c-first-order
   #:name if/c-name))

(define-struct (impersonator-if/c base-if/c) ()
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection if/c-late-neg-proj
   #:first-order if/c-first-order
   #:name if/c-name))
