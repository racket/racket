#lang racket/base
(require (for-syntax racket/base)
         racket/treelist
         syntax/location
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "rand.rkt"
         "generate.rkt")

(provide treelist/c)

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

(define (treelist/c-generate ctc)
  (λ (fuel)
    (define eg (contract-random-generate/choose (base-treelist/c-ctc ctc) fuel))
    (if eg
        (λ ()
          (let loop ([so-far '()])
            (rand-choice
             [1/5 (apply treelist so-far)]
             [else (loop (cons (eg) so-far))])))
        (λ () (treelist)))))

(define (treelist/c-exercise ctc)
  (define elem-ctc (base-treelist/c-ctc ctc))
  (λ (fuel)
    (printf "fuel: ~s\n" fuel)
    (define env (contract-random-generate-get-current-environment))
    (values
     (λ (lst)
       (contract-random-generate-stash
        env elem-ctc
        (oneof (for/list ([e (in-treelist lst)])
                 e))))
     (list elem-ctc))))

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
