#lang racket/base
(require (for-syntax racket/base)
         racket/treelist
         racket/mutable-treelist
         syntax/location
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "rand.rkt"
         "generate.rkt")

(provide treelist/c mutable-treelist/c)

(struct base-treelist/c (ctc))

(define/subexpression-pos-prop (treelist/c
                                ctc
                                #:flat? [flat? (flat-contract? ctc)]
                                #:lazy? [lazy?
                                         (cond
                                           [(flat-contract? ctc) #f]
                                           [(chaperone-contract? ctc) #t]
                                           [else #f])])
  (when (and flat? lazy?)
    (raise-arguments-error
     'treelist/c
     "#:flat? and #:lazy? cannot both be true"
     "ctc" ctc
     "#:flat?" flat?
     "#:lazy?" lazy?))
  (cond
    [flat?
     (flat-treelist/c (coerce-flat-contract 'treelist/c ctc))]
    [lazy?
     (lazy-treelist/c (coerce-chaperone-contract 'treelist/c ctc))]
    [else
     (define c-ctc (coerce-contract 'treelist/c ctc))
     (if (chaperone-contract? c-ctc)
         (chaperone-copying-treelist/c c-ctc)
         (copying-treelist/c c-ctc))]))


(define (treelist/c-name tl)
  (build-compound-type-name 'treelist/c (base-treelist/c-ctc tl)))

(define (treelist/c-first-order tl)
  (define sub-fo? (contract-struct-first-order (base-treelist/c-ctc tl)))
  (λ (x)
    (and (treelist? x)
         (for/and ([ele (in-treelist x)])
           (sub-fo? ele)))))

(define (treelist/c-chaperone-first-order tl) treelist?)

(define (do-treelist-generate ctc-inside adjust)
  (λ (fuel)
    (define eg (contract-random-generate/choose ctc-inside fuel))
     (if eg
         (λ ()
           (adjust
            (let loop ([so-far '()])
              (rand-choice
               [1/5 (apply treelist so-far)]
               [else (loop (cons (eg) so-far))]))))
         (λ () (adjust (treelist))))))

(define (treelist/c-generate ctc)
  (do-treelist-generate (base-treelist/c-ctc ctc) values))

(define (do-treelist/c-exercise elem-ctc adjust)
  (λ (fuel)
    (define env (contract-random-generate-get-current-environment))
    (values
     (λ (lst)
       (contract-random-generate-stash
        env elem-ctc
        (oneof (for/list ([e (in-treelist (adjust lst))])
                 e))))
     (list elem-ctc))))

(define (treelist/c-exercise ctc)
  (define elem-ctc (base-treelist/c-ctc ctc))
  (do-treelist/c-exercise elem-ctc values))

(define (treelist/c-stronger this that)
  (cond
    [(base-treelist/c? that)
     (define this-sub (base-treelist/c-ctc this))
     (define that-sub (base-treelist/c-ctc that))
     (contract-struct-stronger? this-sub that-sub)]
    [else #f]))

(define (treelist/c-equivalent this that)
  (cond
    [(base-treelist/c? that)
     (define this-sub (base-treelist/c-ctc this))
     (define that-sub (base-treelist/c-ctc that))
     (contract-struct-equivalent? this-sub that-sub)]
    [else #f]))

(define (treelist/c-flat-late-neg-projection ctc)
  (define late-neg (contract-late-neg-projection (base-treelist/c-ctc ctc)))
  (λ (blame)
    (define tl-blame (blame-add-context blame "the elements of"))
    (define ln+blame (late-neg tl-blame))
    (λ (val neg-party)
      (unless (treelist? val)
        (raise-blame-error 
         blame val #:missing-party neg-party
         '(expected "a treelist" given: "~e") val))
      (for ([ele (in-treelist val)])
        (ln+blame ele neg-party))
      val)))

(define (treelist/c-lazy-late-neg-projection ctc)
  (define late-neg (contract-late-neg-projection (base-treelist/c-ctc ctc)))
  (λ (blame)
    (define tl-blame (blame-add-context blame "the elements of"))
    (define ln+blame (late-neg tl-blame))
    (λ (val neg-party)
      (unless (treelist? val)
        (raise-blame-error 
         blame val #:missing-party neg-party
         '(expected "a treelist" given: "~e") val))
      (define blame+neg-party (cons blame neg-party))
      (chaperone-treelist
       val
       #:state (make-treelist (treelist-length val) #t)
       #:ref (λ (tl i val state)
               (if (treelist-ref state i)
                   (with-contract-continuation-mark
                       blame+neg-party
                     (ln+blame val neg-party))
                   val))
       #:set (λ (tl i val state) (values val (treelist-set state i #f)))
       #:insert (λ (tl i val state) (values val (treelist-insert state i #f)))
       #:append (λ (tl1 tl2 state) (values tl2 (treelist-append state (make-treelist (treelist-length tl2) #f))))
       #:prepend (λ (tl1 tl2 state) (values tl1 (treelist-append (make-treelist (treelist-length tl2) #f) state)))
       #:delete (λ (tl i state) (treelist-delete state i))
       #:take (λ (tl n state) (treelist-take state n))
       #:drop (λ (tl n state) (treelist-drop state n))
       impersonator-prop:contracted ctc
       impersonator-prop:blame blame))))

(define (treelist/c-copying-late-neg-projection ctc)
  (define late-neg (contract-late-neg-projection (base-treelist/c-ctc ctc)))
  (λ (blame)
    (define tl-blame (blame-add-context blame "the elements of"))
    (define ln+blame (late-neg tl-blame))
    (λ (val neg-party)
      (unless (treelist? val)
        (raise-blame-error 
         blame val #:missing-party neg-party
         '(expected "a treelist" given: "~e") val))
      (for/treelist ([ele (in-treelist val)])
        (ln+blame ele neg-party)))))

(struct flat-treelist/c base-treelist/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:name treelist/c-name
   #:first-order treelist/c-first-order
   #:generate treelist/c-generate
   #:exercise treelist/c-exercise
   #:stronger treelist/c-stronger
   #:equivalent treelist/c-equivalent
   #:late-neg-projection treelist/c-flat-late-neg-projection))

(struct lazy-treelist/c base-treelist/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name treelist/c-name
   #:first-order treelist/c-chaperone-first-order
   #:generate treelist/c-generate
   #:exercise treelist/c-exercise
   #:stronger treelist/c-stronger
   #:equivalent treelist/c-equivalent
   #:late-neg-projection treelist/c-lazy-late-neg-projection))

(struct chaperone-copying-treelist/c base-treelist/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name treelist/c-name
   #:first-order treelist/c-first-order
   #:generate treelist/c-generate
   #:exercise treelist/c-exercise
   #:stronger treelist/c-stronger
   #:equivalent treelist/c-equivalent
   #:late-neg-projection treelist/c-copying-late-neg-projection))

(struct copying-treelist/c base-treelist/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name treelist/c-name
   #:first-order treelist/c-first-order
   #:generate treelist/c-generate
   #:exercise treelist/c-exercise
   #:stronger treelist/c-stronger
   #:equivalent treelist/c-equivalent
   #:late-neg-projection treelist/c-copying-late-neg-projection))


(define (mutable-treelist/c-name ctc)
  (build-compound-type-name 'mutable-treelist/c (base-mutable-treelist/c-ctc ctc)))

(define (mutable-treelist/c-first-order tl)
  mutable-treelist?)

(define (mutable-treelist/c-generate ctc)
  (do-treelist-generate (base-mutable-treelist/c-ctc ctc)
                        treelist-copy))

(define (mutable-treelist/c-exercise ctc)
  (define elem-ctc (base-mutable-treelist/c-ctc ctc))
  (do-treelist/c-exercise elem-ctc mutable-treelist-snapshot))

(define (mutable-treelist/c-stronger this that)
  (cond
    [(base-mutable-treelist/c? that)
     (define this-sub (base-mutable-treelist/c-ctc this))
     (define that-sub (base-mutable-treelist/c-ctc that))
     (contract-struct-equivalent? this-sub that-sub)]
    [else #f]))

(define (mutable-treelist/c-equivalent this that)
  (cond
    [(base-mutable-treelist/c? that)
     (define this-sub (base-mutable-treelist/c-ctc this))
     (define that-sub (base-mutable-treelist/c-ctc that))
     (contract-struct-equivalent? this-sub that-sub)]
    [else #f]))

(define ((mutable-treelist/c-late-neg-projection c-or-i-mutable-treelist) ctc)
  (define late-neg (contract-late-neg-projection (base-mutable-treelist/c-ctc ctc)))
  (λ (blame)
    (define tl-blame (blame-add-context blame "the elements of"))
    (define tl-swap-blame (blame-add-context blame "the elements of" #:swap? #t))
    (define-values (filled? maybe-ln+blame maybe-ln+swap-blame)
      (contract-pos/neg-doubling (late-neg tl-blame) (late-neg tl-swap-blame)))
    (define (make-val-np/proc ln+blame ln+swap-blame)
      (λ (val neg-party)
        (unless (mutable-treelist? val)
          (raise-blame-error
           blame val #:missing-party neg-party
           '(expected "a mutable treelist" given: "~e") val))
        (define blame+neg-party (cons blame neg-party))
        (c-or-i-mutable-treelist
         val
         #:ref (λ (tl i val)
                 (with-contract-continuation-mark
                     blame+neg-party
                   (ln+blame val neg-party)))
         #:set (λ (tl i val)
                 (with-contract-continuation-mark
                     blame+neg-party
                   (ln+swap-blame val neg-party)))
         #:insert (λ (tl i val)
                    (with-contract-continuation-mark
                        blame+neg-party
                      (ln+swap-blame val neg-party)))
         #:append (λ (tl1 tl2)
                    (chaperone-treelist
                     tl2
                     #:state #f
                     #:ref (λ (tl i val state) (ln+swap-blame val neg-party))
                     #:set (λ (tl i val state) (values val state))
                     #:insert (λ (tl i val state) (values val state))
                     #:append (λ (tl1 tl2 state) (values tl2 state))
                     #:prepend (λ (tl1 tl2 state) (values tl1 state))
                     #:delete (λ (tl i state) state)
                     #:take (λ (tl i state) state)
                     #:drop (λ (tl i state) state))
                    ;; alternatively, we could just copy the to-be-appended
                    ;; treelist but the chaperone above has better asymptotic
                    ;; complexity and seems to be working out
                    #;
                    (for/treelist ([ele (in-treelist tl2)])
                      (ln+swap-blame ele neg-party)))
         impersonator-prop:contracted ctc
         impersonator-prop:blame blame)))
    (cond
      [filled? (make-val-np/proc maybe-ln+blame maybe-ln+swap-blame)]
      [else
       (define tc (make-thread-cell #f))
       (λ (val neg-party)
         (cond
           [(thread-cell-ref tc)
            =>
            (λ (f) (f val neg-party))]
           [else
            (define proc (make-val-np/proc (maybe-ln+blame) (maybe-ln+swap-blame)))
            (thread-cell-set! tc proc)
            (proc val neg-party)]))])))

(struct base-mutable-treelist/c (ctc))
(struct chaperone-mutable-treelist/c base-mutable-treelist/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name mutable-treelist/c-name
   #:first-order mutable-treelist/c-first-order
   #:generate mutable-treelist/c-generate
   #:exercise mutable-treelist/c-exercise
   #:stronger mutable-treelist/c-stronger
   #:equivalent mutable-treelist/c-equivalent
   #:late-neg-projection (mutable-treelist/c-late-neg-projection chaperone-mutable-treelist)))
(struct impersonator-mutable-treelist/c base-mutable-treelist/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name mutable-treelist/c-name
   #:first-order mutable-treelist/c-first-order
   #:generate mutable-treelist/c-generate
   #:exercise mutable-treelist/c-exercise
   #:stronger mutable-treelist/c-stronger
   #:equivalent mutable-treelist/c-equivalent
   #:late-neg-projection (mutable-treelist/c-late-neg-projection impersonate-mutable-treelist)))

(define/subexpression-pos-prop (mutable-treelist/c _ctc)
  (define ctc (coerce-contract 'mutable-treelist/c _ctc))
  (cond
    [(chaperone-contract? ctc)
     (chaperone-mutable-treelist/c ctc)]
    [else
     (impersonator-mutable-treelist/c ctc)]))
