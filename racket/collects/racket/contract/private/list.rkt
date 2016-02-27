#lang racket/base
(require (for-syntax racket/base
                     "arr-util.rkt")
         syntax/location
         (only-in "../../private/promise.rkt" prop:force promise-forcer)
         "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "rand.rkt"
         "generate.rkt"
         "generate-base.rkt"
         "misc.rkt")

(provide listof list*of non-empty-listof cons/c list/c cons/dc
         blame-add-car-context
         blame-add-cdr-context
         raise-not-cons-blame-error
         *list/c ellipsis-rest-arg
         (struct-out ellipsis-rest-arg-ctc)
         (struct-out *list-ctc))

(define (listof-generate ctc)
  (cond
    [(im-listof-ctc? ctc)
     (λ (fuel)
       (define middle-eg (contract-random-generate/choose (listof-ctc-elem-c ctc) fuel))
       (define last-eg (contract-random-generate/choose (im-listof-ctc-last-c ctc) fuel))
       (cond
         [(and last-eg middle-eg)
          (λ ()
            (let loop ([so-far (last-eg)])
              (rand-choice
               [1/5 so-far]
               [else (loop (cons (middle-eg) so-far))])))]
         [last-eg
          (λ ()
            (last-eg))]
         [else #f]))]
    [else
     (λ (fuel)
       (define eg (contract-random-generate/choose (listof-ctc-elem-c ctc) fuel))
       (if eg
           (λ ()
             (let loop ([so-far (cond
                                  [(pe-listof-ctc? ctc)
                                   '()]
                                  [(ne-listof-ctc? ctc)
                                   (list (eg))])])
               (rand-choice
                [1/5 so-far]
                [else (loop (cons (eg) so-far))])))
           (if (pe-listof-ctc? ctc)
               (λ () '())
               #f)))]))

(define (listof-exercise ctc) 
  (cond
    [(pe-listof-ctc? ctc)
     (λ (fuel) (values void '()))]
    [(im-listof-ctc? ctc)
     (define last-ctc (im-listof-ctc-last-c ctc))
     (λ (fuel)
       (define env (contract-random-generate-get-current-environment))
       (values
        (λ (lst)
          (contract-random-generate-stash
           env last-ctc 
           (let loop ([lst lst])
             (cond
               [(pair? lst) (loop (cdr lst))]
               [else lst]))))
        (list last-ctc)))]
    [else
     (define elem-ctc (listof-ctc-elem-c ctc))
     (λ (fuel)
       (define env (contract-random-generate-get-current-environment))
       (values
        (λ (lst)
          (contract-random-generate-stash
           env elem-ctc 
           (oneof lst)))
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
     (cond
       [(pe-listof-ctc? this) (and (pe-listof-ctc? that)
                                   (contract-struct-stronger? this-elem that-elem))]
       [(im-listof-ctc? this)
        (and (im-listof-ctc? that)
             (contract-struct-stronger? this-elem that-elem)
             (contract-struct-stronger? (im-listof-ctc-last-c this)
                                        (im-listof-ctc-last-c that)))]
       [else (contract-struct-stronger? this-elem that-elem)])]
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
  (cond
    [(pe-listof-ctc? ctc)
     (build-compound-type-name 'listof (listof-ctc-elem-c ctc))]
    [(ne-listof-ctc? ctc)
     (build-compound-type-name 'non-empty-listof (listof-ctc-elem-c ctc))]
    [(im-listof-ctc? ctc)
     (define elem-name (contract-name (listof-ctc-elem-c ctc)))
     (define last-name (contract-name (im-listof-ctc-last-c ctc)))
     (cond
       [(equal? elem-name last-name)
        `(list*of ,elem-name)]
       [else
        `(list*of ,elem-name ,last-name)])]))

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
     (define last-fo? (contract-first-order (im-listof-ctc-last-c ctc)))
     (λ (v)
       (let loop ([v v])
         (cond
           [(pair? v) 
            (and (elem-fo? (car v))
                 (loop (cdr v)))]
           [else
            (last-fo? v)])))]))

(define (listof-late-neg-projection ctc)
  (define elem-proj (get/build-late-neg-projection (listof-ctc-elem-c ctc)))
  (define pred? (if (pe-listof-ctc? ctc)
                    list?
                    non-empty-list?))
  (define last-proj (and (im-listof-ctc? ctc)
                         (get/build-late-neg-projection (im-listof-ctc-last-c ctc))))
  (λ (blame)
    (define lo-blame (blame-add-listof-context blame))
    (define elem-proj+blame (elem-proj lo-blame))
    (cond
      [(flat-listof-ctc? ctc)
       (cond
         [(im-listof-ctc? ctc)
          (define last-elem-proj+blame (last-proj lo-blame))
          (λ (val neg-party)
            (let loop ([val val])
              (cond
                [(pair? val)
                 (elem-proj+blame (car val) neg-party)
                 (loop (cdr val))]
                [else 
                 (last-elem-proj+blame val neg-party)]))
            val)]
         [else
          (λ (val neg-party)
            (cond
              [(pred? val)
               (for ([x (in-list val)])
                 (elem-proj+blame x neg-party))
               val]
              [else
               (raise-listof-blame-error blame val (pe-listof-ctc? ctc) neg-party)]))])]
      [else
       (cond
         [(im-listof-ctc? ctc)
          (define last-elem-proj+blame (last-proj lo-blame))
          (λ (val neg-party)
            (let loop ([val val])
              (cond
                [(pair? val)
                 (cons (elem-proj+blame (car val) neg-party)
                       (loop (cdr val)))]
                [else 
                 (last-elem-proj+blame val neg-party)])))]
         [else
          (λ (val neg-party)
            (if (pred? val)
                (for/list ([x (in-list val)])
                  (elem-proj+blame x neg-party))
                (raise-listof-blame-error blame val (pe-listof-ctc? ctc) neg-party)))])])))

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
(struct im-listof-ctc listof-ctc (last-c))

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
(define/subexpression-pos-prop (list*of raw-ele-c [raw-last-c raw-ele-c])
  (define ele-c (coerce-contract 'list*of raw-ele-c))
  (define last-c (coerce-contract 'list*of raw-last-c))
  (cond
    [(and (generic-list/c? last-c)
          (null? (generic-list/c-args last-c)))
     (listof ele-c)]
    [(and (flat-contract? ele-c) (flat-contract? last-c)) (imf-listof-ctc ele-c last-c)]
    [(and (chaperone-contract? ele-c) (chaperone-contract? last-c)) (imc-listof-ctc ele-c last-c)]
    [else (imi-listof-ctc ele-c last-c)]))


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

(define (*list/c-name-proc ctc)
  `(*list/c ,(contract-name (*list-ctc-prefix ctc))
            ,@(map contract-name (*list-ctc-suffix ctc))))

(define (*list/c-first-order ctc)
  (define prefix? (contract-first-order (*list-ctc-prefix ctc)))
  (define suffix?s (map contract-first-order (*list-ctc-suffix ctc)))
  (define suffix?s-len (length suffix?s))
  (λ (val)
    (cond
      [(list? val)
       (define-values (long-enough? end) (get-end val suffix?s-len))
       (cond
         [long-enough?
          (let loop ([val val]
                     [end end])
            (cond
              [(null? end)
               (for/and ([ele (in-list val)]
                         [suffix? (in-list suffix?s)])
                 (suffix? ele))]
              [else
               (and (prefix? (car val))
                    (loop (cdr val) (cdr end)))]))]
         [else #f])]
      [else #f])))

(define (get-end val suffix?s-len)
  (let loop ([val val]
             [i suffix?s-len])
    (cond
      [(zero? i) (values #t val)]
      [(null? val) (values #f #f)]
      [else (loop (cdr val) (- i 1))])))

(define (*list/c-generate ctc)
  (λ (fuel)
    (define prefix-gen (contract-random-generate/choose (*list-ctc-prefix ctc) fuel))
    (define suffix-gens
      (for/list ([suf (*list-ctc-suffix ctc)])
        (contract-random-generate/choose suf fuel)))
    (and prefix-gen
         (for/and ([i (in-list suffix-gens)]) i)
         (λ ()
           (let loop ()
             (rand-choice
              [1/5 (for/list ([suffix-gen (in-list suffix-gens)])
                     (suffix-gen))]
              [else (cons (prefix-gen) (loop))]))))))

(define (*list/c-exercise ctc)
  (define suffix (reverse (*list-ctc-suffix ctc)))
  (λ (fuel)
    (define env (contract-random-generate-get-current-environment))
    (values
     (λ (lst)
       (let loop ([lst (reverse lst)]
                  [suffix suffix])
         (cond
           [(null? suffix) (void)]
           [else
            (contract-random-generate-stash env (car lst) (car suffix))
            (loop (cdr lst) (cdr suffix))])))
     suffix)))

(define (*list/c-stronger this that)
  (define this-prefix (*list-ctc-prefix this))
  (define this-suffix (*list-ctc-suffix this))
  (cond
    [(*list-ctc? that)
     (define that-prefix (*list-ctc-prefix that))
     (define that-suffix (*list-ctc-suffix that))
     (define (pad-to a-suf b-suf a-prefix)
       (define a-len (length a-suf))
       (define b-len (length b-suf))
       (cond
         [(< a-len b-len)
          (append (build-list (- b-len a-len) (λ (x) a-prefix))
                  a-suf)]
         [else a-suf]))
     (define padded-this (pad-to this-suffix that-suffix this-prefix))
     (define padded-that (pad-to that-suffix this-suffix that-prefix))
     (and (contract-struct-stronger? this-prefix that-prefix)
          (for/and ([this (in-list padded-this)]
                    [that (in-list padded-that)])
            (contract-struct-stronger? this that)))]
    [(listof-ctc? that)
     (define that-elem (listof-ctc-elem-c that))
     (and (contract-struct-stronger? this-prefix that-elem)
          (for/and ([suf (in-list this-suffix)])
            (contract-struct-stronger? suf that-elem)))]
    [else #f]))

(define (*list/c-late-neg-projection ctc start-index flat?)
  (define prefix-lnp (contract-late-neg-projection (*list-ctc-prefix ctc)))
  (define suffix-lnps (map contract-late-neg-projection (*list-ctc-suffix ctc)))
  (define suffix?s-len (length suffix-lnps))
  (λ (blame)
    (define prefix-val-acceptor
      (prefix-lnp (if start-index
                      (blame-add-context blame "the repeated argument of")
                      (blame-add-context blame "the prefix of"))))
    (define suffix-val-acceptors
      (for/list ([i (in-naturals)]
                 [suffix-lnp (in-list suffix-lnps)])
        (define which (- suffix?s-len i))
        (define msg
          (if (= 1 which)
              (if start-index "the last argument of" "the last element of")
              (format (if start-index
                          "the ~a to the last argument of"
                          "the ~a to the last element of")
                      (n->th which))))
        (suffix-lnp (blame-add-context blame msg))))
    (λ (val neg-party)
      (cond
        [(list? val)
         (define-values (long-enough? end) (get-end val suffix?s-len))
         (cond
           [long-enough?
            (let loop ([remainder-to-process val]
                       [end end])
              (cond
                [(null? end)
                 (cond
                   [flat?
                    (for ([ele (in-list remainder-to-process)]
                          [suffix-val-acceptor (in-list suffix-val-acceptors)])
                      (suffix-val-acceptor ele neg-party))
                    val]
                   [else
                    (for/list ([ele (in-list remainder-to-process)]
                               [suffix-val-acceptor (in-list suffix-val-acceptors)])
                      (suffix-val-acceptor ele neg-party))])]
                [else
                 (define fst (prefix-val-acceptor (car remainder-to-process) neg-party))
                 (if flat?
                     (loop (cdr remainder-to-process) (cdr end))
                     (cons fst (loop (cdr remainder-to-process) (cdr end))))]))]
           [else
            (if start-index
                (raise-blame-error
                 blame
                 val
                 '(expected: "at least ~a arguments"
                             (+ start-index suffix?s-len)))
                (raise-blame-error
                 blame
                 val
                 '(expected: "list? with at least ~a elements" given: "~e")
                 suffix?s-len
                 val))])]
        [else (raise-blame-error
               blame
               val
               '(expected: "list?" given: "~e") val)]))))
  
;; prefix : contract
;; suffix : (listof contract)
(struct *list-ctc (prefix suffix)
    #:property prop:custom-write custom-write-property-proc)

(struct flat-*list/c *list-ctc ()
  #:property prop:contract
  (build-contract-property
   #:name *list/c-name-proc
   #:first-order *list/c-first-order
   #:generate *list/c-generate
   #:exercise *list/c-exercise
   #:stronger *list/c-stronger
   #:late-neg-projection (λ (ctc) (*list/c-late-neg-projection ctc #f #t))
   #:list-contract? (λ (c) #t)))
(struct chaperone-*list/c *list-ctc ()
  #:property prop:contract
  (build-contract-property
   #:name *list/c-name-proc
   #:first-order *list/c-first-order
   #:generate *list/c-generate
   #:exercise *list/c-exercise
   #:stronger *list/c-stronger
   #:late-neg-projection (λ (ctc) (*list/c-late-neg-projection ctc #f #f))
   #:list-contract? (λ (c) #t)))
(struct impersonator-*list/c *list-ctc ()
  #:property prop:contract
  (build-contract-property
   #:name *list/c-name-proc
   #:first-order *list/c-first-order
   #:generate *list/c-generate
   #:exercise *list/c-exercise
   #:stronger *list/c-stronger
   #:late-neg-projection (λ (ctc) (*list/c-late-neg-projection ctc #f #f))
   #:list-contract? (λ (c) #t)))

(define (*list/c ele . rest)
  (define ctcs (coerce-contracts '*list/c (cons ele rest)))
  (cond
    [(null? rest) (listof ele)]
    [(andmap flat-contract? ctcs)
     (flat-*list/c (car ctcs) (cdr ctcs))]
    [(andmap chaperone-contract? ctcs)
     (chaperone-*list/c (car ctcs) (cdr ctcs))]
    [else
     (impersonator-*list/c (car ctcs) (cdr ctcs))]))
     
;; this is a hack to work around cyclic linking issues;
;; see definition of set-some-basic-contracts!
(set-some-basic-contracts!
 (listof any/c)
 (cons/c any/c any/c)
 (list/c))

;; used by -> when it gets an ellipsis. This
;; contract turns into the equivalent of the #:rest
;; argument (if the same contract had been an ->*)
(define (ellipsis-rest-arg start-index . eles)
  (define ctcs (coerce-contracts '-> eles))
  (cond
    [(andmap flat-contract? ctcs)
     (flat-ellipsis-rest-arg (car ctcs) (cdr ctcs) start-index)]
    [(andmap chaperone-contract? ctcs)
     (chaperone-ellipsis-rest-arg (car ctcs) (cdr ctcs) start-index)]
    [else
     (impersonator-ellipsis-rest-arg (car ctcs) (cdr ctcs) start-index)]))

(struct ellipsis-rest-arg-ctc *list-ctc (start-index))
(struct flat-ellipsis-rest-arg ellipsis-rest-arg-ctc ()
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (error 'flat-ellipsis-rest-arg "the name property shouldn't be called!"))
   #:first-order *list/c-first-order
   #:generate *list/c-generate
   #:exercise *list/c-exercise
   #:stronger *list/c-stronger
   #:late-neg-projection
   (λ (ctc) (*list/c-late-neg-projection ctc (ellipsis-rest-arg-ctc-start-index ctc) #t))
   #:list-contract? (λ (c) #t)))
(struct chaperone-ellipsis-rest-arg ellipsis-rest-arg-ctc ()
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (error 'flat-ellipsis-rest-arg "the name property shouldn't be called!"))
   #:first-order *list/c-first-order
   #:generate *list/c-generate
   #:exercise *list/c-exercise
   #:stronger *list/c-stronger
   #:late-neg-projection
   (λ (ctc) (*list/c-late-neg-projection ctc (ellipsis-rest-arg-ctc-start-index ctc) #f))
   #:list-contract? (λ (c) #t)))
(struct impersonator-ellipsis-rest-arg ellipsis-rest-arg-ctc ()
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (error 'flat-ellipsis-rest-arg "the name property shouldn't be called!"))
   #:first-order *list/c-first-order
   #:generate *list/c-generate
   #:exercise *list/c-exercise
   #:stronger *list/c-stronger
   #:late-neg-projection
   (λ (ctc) (*list/c-late-neg-projection ctc (ellipsis-rest-arg-ctc-start-index ctc) #f))
   #:list-contract? (λ (c) #t)))
