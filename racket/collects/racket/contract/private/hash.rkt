#lang racket/base

(require (for-syntax racket/base "arr-util.rkt")
         syntax/location
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "rand.rkt"
         "generate.rkt"
         "generate-base.rkt")

(provide (rename-out [wrap-hash/c hash/c])
         hash/dc)

(define-syntax (wrap-hash/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx hash/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list stx) null))]
    [(h/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'ctc)])
       (define (convert-args args)
         (let loop ([args args]
                    [new-args null]
                    [neg-ctc? #t])
           (cond
             [(null? args) (reverse new-args)]
             [(keyword? (syntax-e (car args)))
              (if (null? (cdr args))
                  (reverse (cons (car args) new-args))
                  (loop (cddr args)
                        (list* (cadr args) (car args) new-args)
                        neg-ctc?))]
             [neg-ctc?
              (loop (cdr args)
                    (cons (syntax-property 
                           (car args)
                           'racket/contract:negative-position
                           this-one)
                          new-args)
                    #f)]
             [else
              (append (reverse new-args)
                      (cons (syntax-property
                             (car args)
                             'racket/contract:positive-position
                             this-one)
                            (cdr args)))])))
       (with-syntax ([(new-arg ...) (convert-args args)]
                     [app (datum->syntax stx '#%app)])
         (syntax-property
          (syntax/loc stx
            (app hash/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'h/c) null))))]))

(define (hash/c dom rng #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (unless (member immutable '(#t #f dont-care))
    (raise-argument-error 'hash/c
                          "(or/c #t #f 'dont-care) for the #:immutable argument"
                          immutable))
  (define dom-ctc (if flat?
                      (coerce-flat-contract 'hash/c dom)
                      (coerce-chaperone-contract 'hash/c dom)))
  (define rng-ctc (if flat?
                      (coerce-flat-contract 'hash/c rng)
                      (coerce-contract 'hash/c rng)))
  (cond
    [(or flat?
         (and (eq? immutable #t)
              (flat-contract? dom-ctc)
              (flat-contract? rng-ctc)))
     (make-flat-hash/c dom-ctc rng-ctc immutable)]
    [(chaperone-contract? rng-ctc)
     (make-chaperone-hash/c dom-ctc rng-ctc immutable)]
    [else
     (make-impersonator-hash/c dom-ctc rng-ctc immutable)]))


;; ... --> boolean
;;  returns #t when it called raise-blame-error, #f otherwise
(define (check-hash/c dom-ctc immutable flat? val blame neg-party) 
  (cond
    [(hash? val)
     (cond
       [(and (not flat?)
             (not (flat-contract? dom-ctc))
             (not (hash-equal? val)))
        (raise-blame-error
         blame val #:missing-party neg-party
         '(expected "equal?-based hash table due to higher-order domain contract" given: "~e")
         val)
        #t]
       [else
        (case immutable
          [(#t) 
           (cond
             [(immutable? val) 
              #f]
             [else
              (raise-blame-error 
               blame val #:missing-party neg-party
               '(expected "an immutable hash" given: "~e") val)
              #t])]
          [(#f)
           (cond
             [(immutable? val)
              (raise-blame-error 
               blame val #:missing-party neg-party
               '(expected "a mutable hash" given: "~e") val)
              #t]
             [else #f])]
          [(dont-care) #f])])]
    [else 
     (raise-blame-error blame val #:missing-party neg-party
                        '(expected "a hash" given: "~e") val)
     #t]))

(define (hash/c-first-order ctc)
  (define dom-ctc (base-hash/c-dom ctc))
  (define rng-ctc (base-hash/c-rng ctc))
  (define immutable (base-hash/c-immutable ctc))
  (define flat? (flat-hash/c? ctc))
  (λ (val)
    (and (hash? val)
         (or flat?
             (flat-contract? dom-ctc)
             (hash-equal? val))
         (case immutable
           [(#t) (immutable? val)]
           [(#f) (not (immutable? val))]
           [else #t])
         (for/and ([(k v) (in-hash val)])
           (and (contract-first-order-passes? dom-ctc k)
                (contract-first-order-passes? rng-ctc v))))))

(define (hash/c-name ctc)
  (apply 
   build-compound-type-name
   'hash/c (base-hash/c-dom ctc) (base-hash/c-rng ctc)
   (append
    (if (and (flat-hash/c? ctc)
             (not (eq? (base-hash/c-immutable ctc) #t)))
        (list '#:flat? #t)
        null)
    (case (base-hash/c-immutable ctc)
      [(dont-care) null]
      [(#t)
       (list '#:immutable #t)]
      [(#f)
       (list '#:immutable #f)]))))

(define-struct base-hash/c (dom rng immutable))

(define (hash/c-stronger this that)
  (define this-dom (base-hash/c-dom this))
  (define this-rng (base-hash/c-rng this))
  (define this-immutable (base-hash/c-immutable this))
  (cond
    [(base-hash/c? that)
     (define that-dom (base-hash/c-dom that))
     (define that-rng (base-hash/c-rng that))
     (define that-immutable (base-hash/c-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (and (contract-struct-stronger? this-dom that-dom)
             (contract-struct-stronger? this-rng that-rng))]
       [(or (equal? that-immutable 'dont-care)
            (equal? this-immutable that-immutable))
        (and (contract-struct-equivalent? this-dom that-dom)
             (contract-struct-equivalent? this-rng that-rng))]
       [else #f])]
    [else #f]))

(define (hash/c-equivalent this that)
  (cond
    [(base-hash/c? that)
     (define this-dom (base-hash/c-dom this))
     (define this-rng (base-hash/c-rng this))
     (define this-immutable (base-hash/c-immutable this))
     (define that-dom (base-hash/c-dom that))
     (define that-rng (base-hash/c-rng that))
     (define that-immutable (base-hash/c-immutable that))
     (and (equal? this-immutable that-immutable)
          (contract-struct-equivalent? this-dom that-dom)
          (contract-struct-equivalent? this-rng that-rng))]
    [else #f]))

;; Will periodically generate empty hashes and hashes with multiple elements
(define (hash/c-generate ctc)
  (define this-dom (base-hash/c-dom ctc))
  (define this-rng (base-hash/c-rng ctc))
  (define this-immutable (base-hash/c-immutable ctc))
  (λ (fuel)
    (define rnd (if (zero? fuel) 0 (random fuel))) ;; used to return empty hashes from time to time
    (define gen-key (contract-random-generate/choose this-dom fuel))
    (define gen-val (contract-random-generate/choose this-rng fuel))
    (λ ()
      (cond [(or (zero? rnd) (not gen-key) (not gen-val))
             (if this-immutable
                 (hash)
                 (make-hash))]
            [else
             (let ([pair-list
                    (let loop ([so-far (list (cons (gen-key) (gen-val)))])
                      (rand-choice
                       [1/5 so-far]
                       [else
                        (loop
                         (cons (cons (gen-key) (gen-val)) so-far))]))])
               (if this-immutable
                   (make-immutable-hash pair-list)
                   (make-hash pair-list)))]))))

(define (hash/c-exercise ctc)
  (define env (contract-random-generate-get-current-environment))
  (define dom (base-hash/c-dom ctc))
  (define rng (base-hash/c-rng ctc))
  (λ (fuel)
    ;; passing (list dom rng) to multi-exercise will produce
    ;; a function that exercises values of form (list/c dom rng)
    ;; and a list of newly available contracts.
    (define-values (exercise-list-dom-rng available-ctcs)
      ((multi-exercise (list dom rng)) fuel))
    (values
     (λ (h)
       ;; iterate over key-value pairs, exercise and stash
       (for ([(k v) (in-hash h)])
         (exercise-list-dom-rng (list k v))
         (contract-random-generate-stash env dom k)
         (contract-random-generate-stash env dom v)))
     (cons dom (cons rng available-ctcs)))))

(define-struct (flat-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:generate hash/c-generate
   #:exercise hash/c-exercise
   #:stronger hash/c-stronger
   #:equivalent hash/c-equivalent
   #:late-neg-projection
   (λ (ctc)
     (define dom-ctc (base-hash/c-dom ctc))
     (define immutable (base-hash/c-immutable ctc))
     (define flat? (flat-hash/c? ctc))
     (λ (blame)
       (define dom-proj ((get/build-late-neg-projection (base-hash/c-dom ctc))
                         (blame-add-key-context blame #f)))
       (define rng-proj ((get/build-late-neg-projection (base-hash/c-rng ctc))
                         (blame-add-value-context blame #f)))
       (λ (val neg-party)
         (cond
           [(check-hash/c dom-ctc immutable flat? val blame neg-party)
            val]
           [else
            (for ([(k v) (in-hash val)])
              (dom-proj k neg-party)
              (rng-proj v neg-party))
            val]))))))

(define (ho-projection chaperone-or-impersonate-hash)
  (λ (ctc)
    (define immutable (base-hash/c-immutable ctc))
    (define dom-ctc (base-hash/c-dom ctc))
    (define flat? (flat-hash/c? ctc))
    (define dom-proc (get/build-late-neg-projection dom-ctc))
    (define rng-proc (get/build-late-neg-projection (base-hash/c-rng ctc)))
    (λ (blame)
      (define-values (dom-filled? maybe-pos-dom-proj maybe-neg-dom-proj)
        (contract-pos/neg-doubling (dom-proc (blame-add-key-context blame #f))
                                   (dom-proc (blame-add-key-context blame #t))))
      (define-values (rng-filled? maybe-pos-rng-proj maybe-neg-rng-proj)
        (contract-pos/neg-doubling (rng-proc (blame-add-value-context blame #f))
                                   (rng-proc (blame-add-value-context blame #t))))
      (cond
        [(and dom-filled? rng-filled?)
         (λ (val neg-party)
           (cond
             [(check-hash/c dom-ctc immutable flat? val blame neg-party)
              val]
             [else
              (handle-the-hash val neg-party
                               maybe-pos-dom-proj maybe-neg-dom-proj
                               (λ (v) maybe-pos-rng-proj) (λ (v) maybe-neg-rng-proj)
                               chaperone-or-impersonate-hash ctc blame)]))]
        [else
         (define tc (make-thread-cell #f))
         (λ (val neg-party)
           (define-values (pos-dom-proj neg-dom-proj pos-rng-proj neg-rng-proj)
             (cond
               [(thread-cell-ref tc)
                =>
                (λ (v) (values (vector-ref v 1) (vector-ref v 2) (vector-ref v 3) (vector-ref v 4)))]
               [else
                (define pos-dom-proj (maybe-pos-dom-proj))
                (define neg-dom-proj (maybe-neg-dom-proj))
                (define pos-rng-proj (maybe-pos-rng-proj))
                (define neg-rng-proj (maybe-neg-rng-proj))
                (thread-cell-set! tc (vector pos-dom-proj neg-dom-proj pos-rng-proj neg-rng-proj))
                (values pos-dom-proj neg-dom-proj pos-rng-proj neg-rng-proj)]))
           (cond
             [(check-hash/c dom-ctc immutable flat? val blame neg-party)
              val]
             [else
              (handle-the-hash val neg-party
                               pos-dom-proj neg-dom-proj
                               (λ (v) pos-rng-proj) (λ (v) neg-rng-proj)
                               chaperone-or-impersonate-hash ctc blame)]))]))))

(define (blame-add-key-context blame swap?) (blame-add-context blame "the keys of" #:swap? swap?))
(define (blame-add-value-context blame swap?) (blame-add-context blame "the values of" #:swap? swap?))

(define (handle-the-hash val neg-party
                         pos-dom-proj neg-dom-proj mk-pos-rng-proj mk-neg-rng-proj
                         chaperone-or-impersonate-hash ctc blame)
  (define blame+neg-party (cons blame neg-party))
  (if (immutable? val) 
      (for/fold ([h val]) ([(k v) (in-hash val)])
        (hash-set h
                  (pos-dom-proj k neg-party)
                  ((mk-pos-rng-proj k) v neg-party)))
      (chaperone-or-impersonate-hash
       val
       (λ (h k)
         (values (with-contract-continuation-mark
                   blame+neg-party
                   (neg-dom-proj k neg-party))
                 (λ (h k v)
                   (with-contract-continuation-mark
                     blame+neg-party
                     ((mk-pos-rng-proj k) v neg-party)))))
       (λ (h k v)
         (with-contract-continuation-mark
           blame+neg-party
           (values (neg-dom-proj k neg-party)
                   ((mk-neg-rng-proj k) v neg-party))))
       (λ (h k)
         (with-contract-continuation-mark
           blame+neg-party
           (neg-dom-proj k neg-party)))
       (λ (h k)
         (with-contract-continuation-mark
           blame+neg-party
           (pos-dom-proj k neg-party)))
       impersonator-prop:contracted ctc
       impersonator-prop:blame blame)))

(define-struct (chaperone-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:generate hash/c-generate
   #:exercise hash/c-exercise
   #:stronger hash/c-stronger
   #:equivalent hash/c-equivalent
   #:late-neg-projection (ho-projection chaperone-hash)))

(define-struct (impersonator-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:stronger hash/c-stronger
   #:equivalent hash/c-equivalent
   #:late-neg-projection (ho-projection impersonate-hash)))


(define (hash/dc-name a-hash-dc)
  (define info (base-hash/dc-name-info a-hash-dc))
  (define immutable (base-hash/dc-immutable a-hash-dc))
  `(hash/dc [,(vector-ref info 0) ,(contract-name (base-hash/dc-dom a-hash-dc))]
            [,(vector-ref info 1) (,(vector-ref info 0)) ,(vector-ref info 2)]
            ,@(if (equal? immutable 'dont-care)
                  '()
                  `(#:immutable ,immutable))
            ,@(cond
                [(flat-hash/dc? a-hash-dc)
                 '(#:kind 'flat)]
                [(chaperone-hash/dc? a-hash-dc)
                 '()]
                [else '(#:kind 'impersonator)])))

(define (hash/dc-first-order a-hash-dc)
  (define dom (base-hash/dc-dom a-hash-dc))
  (define rng-f (base-hash/dc-dep-rng a-hash-dc))
  (λ (val)
    (and (hash? val)
         (for/and ([(k v) (in-hash val)])
           (and (contract-first-order-passes? dom k)
                (contract-first-order-passes? (rng-f k) v))))))

(define (hash/dc-stronger this that) #f)
(define (hash/dc-equivalent this that) #f)

(define ((hash/dc-late-neg-projection chaperone-or-impersonate-hash) ctc)
  (define dom-ctc (base-hash/dc-dom ctc))
  (define immutable (base-hash/dc-immutable ctc))
  (define flat? (flat-hash/dc? ctc))
  (define dom-proc (get/build-late-neg-projection dom-ctc))
  (define dep-rng-proc (base-hash/dc-dep-rng ctc))
  (λ (blame)
    (define pos-dom-proj (dom-proc (blame-add-key-context blame #f)))
    (define neg-dom-proj (dom-proc (blame-add-key-context blame #t)))
    (define indy-dom-proj (dom-proc 
                           (blame-replace-negative (blame-add-key-context blame #f)
                                                   (base-hash/dc-here ctc))))
    (define pos-value-blame (blame-add-value-context blame #f))
    (define neg-value-blame (blame-add-value-context blame #t))
    (cond
      [chaperone-or-impersonate-hash
       (λ (val neg-party)
         (cond
           [(check-hash/c dom-ctc immutable flat? val blame neg-party) val]
           [else
            (define ((mk-rng-proj x-value-blame) key)
              ((get/build-late-neg-projection (dep-rng-proc (indy-dom-proj key neg-party)))
               x-value-blame))
            (handle-the-hash val neg-party
                             pos-dom-proj neg-dom-proj
                             (mk-rng-proj pos-value-blame) (mk-rng-proj neg-value-blame)
                             chaperone-or-impersonate-hash ctc blame)]))]
      [else
       (λ (val neg-party)
         (check-hash/c dom-ctc immutable flat? val blame neg-party)
         (define ((mk-rng-proj x-value-blame) key)
           ((get/build-late-neg-projection (dep-rng-proc (indy-dom-proj key neg-party)))
            x-value-blame))
         (define mk-pos-rng-proj (mk-rng-proj pos-value-blame))
         (define mk-neg-rng-proj (mk-rng-proj neg-value-blame))
         (with-contract-continuation-mark (cons blame neg-party)
           (for ([(k v) (in-hash val)])
             (pos-dom-proj k neg-party)
             ((mk-pos-rng-proj k) v neg-party)))
         val)])))
        
(struct base-hash/dc (dom dep-rng here name-info immutable))
(struct flat-hash/dc base-hash/dc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:name hash/dc-name
   #:first-order hash/dc-first-order
   #:equivalent hash/dc-equivalent
   #:stronger hash/dc-stronger
   #:late-neg-projection (hash/dc-late-neg-projection #f)))

(struct chaperone-hash/dc base-hash/dc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name hash/dc-name
   #:first-order hash/dc-first-order
   #:stronger hash/dc-stronger
   #:equivalent hash/dc-equivalent
   #:late-neg-projection (hash/dc-late-neg-projection chaperone-hash)))
(struct impersonator-hash/dc base-hash/dc ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name hash/dc-name
   #:first-order hash/dc-first-order
   #:stronger hash/dc-stronger
   #:equivalent hash/dc-equivalent
   #:late-neg-projection (hash/dc-late-neg-projection impersonate-hash)))

(define (build-hash/dc dom dep-rng here name-info immutable kind)
  (unless (member kind '(flat chaperone impersonator))
    (error 'hash/dc 
           "expected (or/c 'flat 'chaperone 'impersonator) for the #:kind argument, got ~s"
           kind))
  (cond
    [(equal? kind 'flat) 
     (flat-hash/dc (coerce-flat-contract 'hash/dc dom)
                   (λ (v) (coerce-flat-contract 'hash/dc (dep-rng v)))
                   here name-info immutable)]
    [(equal? kind 'chaperone)
     (chaperone-hash/dc (coerce-chaperone-contract 'hash/dc dom)
                        (λ (v) (coerce-chaperone-contract 'hash/dc (dep-rng v)))
                        here name-info immutable)]
    [else
     (chaperone-hash/dc (coerce-contract 'hash/dc dom)
                        (λ (v) (coerce-contract 'hash/dc (dep-rng v)))
                        here name-info immutable)]))

(define-syntax (hash/dc stx)
  (syntax-case stx ()
    [(_ [dom-id dom-ctc-expr] [rng-id (dom-id2) rng-ctc-expr] . more)
     (begin
       (unless (free-identifier=? #'dom-id2 #'dom-id)
         (raise-syntax-error
          'hash/dc
          "expected the same identifier for the domain and the dependency"
          stx
          #'dom-id
          (list #'dom-id2)))
       (define immutable-expression #f)
       (define kind-expression #f)
       (let loop ([kwd-stx #'more])
         (syntax-case kwd-stx ()
           [() (void)]
           [(#:immutable immutable . more)
            (begin
              (when immutable-expression
                (raise-syntax-error 'hash/dc "multiple #:immutable arguments"
                                    stx
                                    immutable-expression
                                    (list #'immutable)))
              (set! immutable-expression #'immutable)
              (loop #'more))]
           [(#:kind kind . more)
            (begin
              (when kind-expression
                (raise-syntax-error 'hash/dc "multiple #:kind arguments"
                                    stx
                                    kind-expression
                                    (list #'kind)))
              (set! kind-expression #'kind)
              (loop #'more))]
           [(x . y)
            (raise-syntax-error 'hash/dc
                                "expected either the keyword #:kind or #:immutable"
                                stx
                                #'x)]))
       #`(build-hash/dc dom-ctc-expr
                        (λ (dom-id2) rng-ctc-expr)
                        (quote-module-name)
                        '#(dom-id rng-id #,(compute-quoted-src-expression #'rng-ctc-expr))
                        #,(or immutable-expression #''dont-care)
                        #,(or kind-expression #''chaperone)))]))
