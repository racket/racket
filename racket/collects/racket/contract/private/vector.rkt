#lang racket/base

(require (for-syntax racket/base)
         "guts.rkt"
         "prop.rkt"
         "blame.rkt"
         "misc.rkt")

(provide (rename-out [wrap-vectorof vectorof]
                     [wrap-vector/c vector/c])
         vector-immutable/c vector-immutableof)

(define-struct base-vectorof (elem immutable))

(define-for-syntax (convert-args args this-one)
  (let loop ([args args]
             [new-args null])
    (cond
      [(null? args) (reverse new-args)]
      [(keyword? (syntax-e (car args)))
       (if (null? (cdr args))
           (reverse (cons (car args) new-args))
           (loop (cddr args)
                 (list* (cadr args) (car args) new-args)))]
      [else
       (loop (cdr args)
             (cons (syntax-property
                    (syntax-property
                     (car args)
                     'racket/contract:positive-position
                     this-one)
                    'racket/contract:negative-position
                    this-one)
                   new-args))])))

(define (vectorof-name c)
  (let ([immutable (base-vectorof-immutable c)])
    (apply build-compound-type-name 'vectorof 
           (contract-name (base-vectorof-elem c))
           (append
            (if (and (flat-vectorof? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (check-vectorof c) 
  (let ([elem-ctc (base-vectorof-elem c)]
        [immutable (base-vectorof-immutable c)]
        [flat? (flat-vectorof? c)])
    (λ (val fail first-order?)
      (unless (vector? val)
        (fail val '(expected "a vector," given: "~e") val))
      (cond
        [(eq? immutable #t)
         (unless (immutable? val)
           (fail val '(expected "an immutable vector" given: "~e") val))]
        [(eq? immutable #f)
         (when (immutable? val)
           (fail val '(expected "an mutable vector" given: "~e" val)))]
        [else (void)])
      (when first-order?
        (for ([e (in-vector val)]
              [n (in-naturals)])
          (unless (contract-first-order-passes? elem-ctc e)
            (fail val '(expected: "~s for element ~s" given: "~e") (contract-name elem-ctc) n e))))
      #t)))

(define (check-late-neg-vectorof c)
  (define immutable (base-vectorof-immutable c))
  (λ (val blame neg-party)
    (cond
      [(vector? val) 
       (cond
         [(eq? immutable #t)
          (cond
            [(immutable? val) #f]
            [else
             (raise-blame-error blame #:missing-party neg-party
                                val '(expected "an immutable vector" given: "~e") val)])]
         [(eq? immutable #f)
          (cond
            [(immutable? val) 
             (raise-blame-error blame #:missing-party neg-party
                                val '(expected "an mutable vector" given: "~e" val))]
            [else #f])]
         [else #f])]
      [else
       (raise-blame-error blame #:missing-party neg-party
                          val
                          '(expected "a vector," given: "~e") 
                          val)])))

(define (vectorof-first-order ctc)
  (let ([check (check-vectorof ctc)])
    (λ (val)
      (let/ec return
        (check val (λ _ (return #f)) #t)))))

(define (vectorof-stronger this that)
  (define this-elem (base-vectorof-elem this))
  (define this-immutable (base-vectorof-immutable this))
  (cond
    [(base-vectorof? that)
     (define that-elem (base-vectorof-elem that))
     (define that-immutable (base-vectorof-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (contract-stronger? this-elem that-elem)]
       [else
        (and (or (equal? that-immutable 'dont-care)
                 (equal? this-immutable that-immutable))
             (contract-stronger? this-elem that-elem)
             (contract-stronger? that-elem this-elem))])]
    [else #f]))

(define-struct (flat-vectorof base-vectorof) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:late-neg-projection (λ (ctc) 
                           (define check (check-late-neg-vectorof ctc))
                           (define vfp (get/build-late-neg-projection (base-vectorof-elem ctc)))
                           (λ (blame)
                             (define ele-blame (blame-add-element-of-context blame))
                             (define vfp+blame (vfp ele-blame))
                             (λ (val neg-party)
                               (check val blame neg-party)
                               (for ([x (in-vector val)])
                                 (vfp+blame x neg-party))
                               val)))
   #:stronger vectorof-stronger
   #:projection 
   (λ (ctc) 
     (define check (check-vectorof ctc))
     (λ (blame) 
       (define raise-blame (λ (val . args) (apply raise-blame-error blame val args)))
       (define ele-blame (blame-add-element-of-context blame))
       (λ (val)
         (check val raise-blame #f)
         (let* ([elem-ctc (base-vectorof-elem ctc)]
                [p ((contract-projection elem-ctc) ele-blame)])
           (for ([e (in-vector val)])
             (p e)))
         val)))))

(define (blame-add-element-of-context blame #:swap? [swap? #f])
  (blame-add-context blame "an element of" #:swap? swap?))

(define (vectorof-late-neg-ho-projection chaperone-or-impersonate-vector)
  (λ (ctc)
    (define elem-ctc (base-vectorof-elem ctc))
    (define immutable (base-vectorof-immutable ctc))
    (define check (check-vectorof ctc))
    (λ (blame)
      (define pos-blame (blame-add-element-of-context blame))
      (define neg-blame (blame-add-element-of-context blame #:swap? #t))
      (define vfp (get/build-late-neg-projection elem-ctc))
      (define elem-pos-proj (vfp pos-blame))
      (define elem-neg-proj (vfp neg-blame))
      (define checked-ref (λ (neg-party)
                            (λ (vec i val)
                              (with-continuation-mark contract-continuation-mark-key
                                (cons pos-blame neg-party)
                                (elem-pos-proj val neg-party)))))
      (define checked-set (λ (neg-party)
                            (λ (vec i val)
                              (with-continuation-mark contract-continuation-mark-key
                                (cons neg-blame neg-party)
                                (elem-neg-proj val neg-party)))))
      
      (λ (val neg-party)
        (define (raise-blame val . args) 
          (apply raise-blame-error blame #:missing-party neg-party val args))
        (check val raise-blame #f)
        (if (and (immutable? val) (not (chaperone? val)))
            (apply vector-immutable
                   (for/list ([e (in-vector val)])
                     (elem-pos-proj e neg-party)))
            (chaperone-or-impersonate-vector
             val
             (checked-ref neg-party)
             (checked-set neg-party)
             impersonator-prop:contracted ctc
             impersonator-prop:blame (blame-add-missing-party blame neg-party)))))))

(define-values (prop:neg-blame-party prop:neg-blame-party? prop:neg-blame-party-get)
  (make-impersonator-property 'prop:neg-blame-party))

(define (vectorof-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctc (base-vectorof-elem ctc)]
           [immutable (base-vectorof-immutable ctc)]
           [check (check-vectorof ctc)])
       (λ (blame)
         (let ([elem-pos-proj ((contract-projection elem-ctc) 
                               (blame-add-element-of-context blame))]
               [elem-neg-proj ((contract-projection elem-ctc)
                               (blame-add-element-of-context blame #:swap? #t))])
           (define checked-ref (λ (vec i val)
                                 (with-continuation-mark
                                  contract-continuation-mark-key blame
                                  (elem-pos-proj val))))
           (define checked-set (λ (vec i val)
                                 (with-continuation-mark
                                  contract-continuation-mark-key blame
                                  (elem-neg-proj val))))
           (define raise-blame (λ (val . args)
                                  (apply raise-blame-error blame val args)))
           (λ (val)
             (check val raise-blame #f)
             (if (and (immutable? val) (not (chaperone? val)))
                 (apply vector-immutable
                        (for/list ([e (in-vector val)])
                          (elem-pos-proj e)))
                 (vector-wrapper
                  val
                  checked-ref
                  checked-set
                  impersonator-prop:contracted ctc
                  impersonator-prop:blame blame))))))))

(define-struct (chaperone-vectorof base-vectorof) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:stronger vectorof-stronger
   #:late-neg-projection (vectorof-late-neg-ho-projection chaperone-vector)
   #:projection (vectorof-ho-projection chaperone-vector)))

(define-struct (impersonator-vectorof base-vectorof) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:stronger vectorof-stronger
   #:late-neg-projection (vectorof-late-neg-ho-projection chaperone-vector)
   #:projection (vectorof-ho-projection impersonate-vector)))

(define-syntax (wrap-vectorof stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx vectorof)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vecof arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'vectorof-ctc)])
       (with-syntax ([(new-arg ...) (convert-args args this-one)])
         (syntax-property
          (syntax/loc stx
            (vectorof new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vecof) null))))]))

(define/subexpression-pos-prop (vectorof c #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (define ctc
    (if flat?
        (coerce-flat-contract 'vectorof c)
        (coerce-contract 'vectorof c)))
  (cond
    [(or flat?
         (and (equal? immutable #t)
              (flat-contract? ctc)))
     (make-flat-vectorof ctc immutable)]
    [(chaperone-contract? ctc)
     (make-chaperone-vectorof ctc immutable)]
    [else
     (make-impersonator-vectorof ctc immutable)]))

(define/subexpression-pos-prop (vector-immutableof c)
  (vectorof c #:immutable #t))

(define-struct base-vector/c (elems immutable))

(define (vector/c-name c)
  (let ([immutable (base-vector/c-immutable c)])
    (apply build-compound-type-name 'vector/c
           (append
            (map contract-name (base-vector/c-elems c))
            (if (and (flat-vector/c? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (check-vector/c ctc val blame)
  (define elem-ctcs (base-vector/c-elems ctc))
  (define immutable (base-vector/c-immutable ctc))
  (unless (vector? val)
    (raise-blame-error blame val '(expected: "a vector" given: "~e") val))
  (cond
    [(eq? immutable #t)
     (unless (immutable? val)
       (raise-blame-error blame val 
                          '(expected: "an immutable vector" given: "~e")
                          val))]
    [(eq? immutable #f)
     (when (immutable? val)
       (raise-blame-error blame val 
                          '(expected: "a mutable vector" given: "~e")
                          val))]
    [else (void)])
  (define elem-count (length elem-ctcs))
  (unless (= (vector-length val) elem-count)
    (raise-blame-error blame val '(expected: "a vector of ~a element~a" given: "~e")
                       elem-count
                       (if (= elem-count 1) "" "s") 
                       val)))

(define (vector/c-first-order ctc)
  (define elem-ctcs (base-vector/c-elems ctc))
  (define immutable (base-vector/c-immutable ctc))
  (λ (val)
    (and (vector? val)
         (cond
          [(eq? immutable #t) (immutable? val)]
          [(eq? immutable #f) (not (immutable? val))]
          [else #t])
         (= (vector-length val) (length elem-ctcs))
         (for/and ([e (in-vector val)]
                   [c (in-list elem-ctcs)])
           (contract-first-order-passes? c e)))))

(define (vector/c-stronger this that)
  ;(define-struct base-vector/c (elems immutable))
  (define this-elems (base-vector/c-elems this))
  (define this-immutable (base-vector/c-immutable this))
  (cond
    [(base-vector/c? that)
     (define that-elems (base-vector/c-elems that))
     (define that-immutable (base-vector/c-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (and (= (length this-elems) (length that-elems))
             (for/and ([this-elem (in-list this-elems)]
                       [that-elem (in-list that-elems)])
               (contract-stronger? this-elem that-elem)))]
       [(or (equal? that-immutable 'dont-care)
            (equal? this-immutable that-immutable))
        (and (= (length this-elems) (length that-elems))
             (for/and ([this-elem (in-list this-elems)]
                       [that-elem (in-list that-elems)])
               (and (contract-stronger? this-elem that-elem)
                    (contract-stronger? that-elem this-elem))))]
       [else #f])]
    [(base-vectorof? that)
     (define that-elem (base-vectorof-elem that))
     (define that-immutable (base-vectorof-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (for/and ([this-elem (in-list this-elems)])
          (contract-stronger? this-elem that-elem))]
       [(or (equal? that-immutable 'dont-care)
            (equal? this-immutable that-immutable))
        (for/and ([this-elem (in-list this-elems)])
          (and (contract-stronger? this-elem that-elem)
               (contract-stronger? that-elem this-elem)))]
       [else #f])]
    [else #f]))

(define-struct (flat-vector/c base-vector/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:stronger vector/c-stronger
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (define blame+ctxt (blame-add-element-of-context blame))
       (λ (val)
         (with-continuation-mark
          contract-continuation-mark-key blame
          (begin
            (check-vector/c ctc val blame)
            (for ([e (in-vector val)]
                  [c (in-list (base-vector/c-elems ctc))])
              (((contract-projection c) blame+ctxt) e))
            val)))))))

(define (vector/c-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctcs (base-vector/c-elems ctc)]
           [immutable (base-vector/c-immutable ctc)])
       (λ (blame)
         (let ([elem-pos-projs (for/vector #:length (length elem-ctcs)
                                 ([c (in-list elem-ctcs)]
                                  [i (in-naturals)])
                                 ((contract-projection c) 
                                  (blame-add-context blame (format "the ~a element of" (n->th i)))))]
               [elem-neg-projs (for/vector #:length (length elem-ctcs)
                                 ([c (in-list elem-ctcs)]
                                  [i (in-naturals)])
                                 ((contract-projection c)
                                  (blame-add-context blame (format "the ~a element of" (n->th i))
                                                     #:swap? #t)))])
           (λ (val)
             (check-vector/c ctc val blame)
             (if (and (immutable? val) (not (chaperone? val)))
                 (apply vector-immutable
                        (for/list ([e (in-vector val)]
                                   [i (in-naturals)])
                          ((vector-ref elem-pos-projs i) e)))
                 (vector-wrapper
                  val
                  (λ (vec i val)
                    (with-continuation-mark
                     contract-continuation-mark-key blame
                     ((vector-ref elem-pos-projs i) val)))
                  (λ (vec i val)
                    (with-continuation-mark
                     contract-continuation-mark-key blame
                     ((vector-ref elem-neg-projs i) val)))
                  impersonator-prop:contracted ctc
                  impersonator-prop:blame blame))))))))

(define-struct (chaperone-vector/c base-vector/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:stronger vector/c-stronger
   #:projection (vector/c-ho-projection chaperone-vector)))

(define-struct (impersonator-vector/c base-vector/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:stronger vector/c-stronger
   #:projection (vector/c-ho-projection impersonate-vector)))

(define-syntax (wrap-vector/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx vector/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vec/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'vector/c-ctc)])
       (with-syntax ([(new-arg ...) (convert-args args this-one)])
         (syntax-property
          (syntax/loc stx
            (vector/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vec/c) null))))]))

(define (vector/c #:immutable [immutable 'dont-care] #:flat? [flat? #f] . cs)
  (let ([ctcs (if flat?
                  (map (λ (c) (coerce-flat-contract 'vector/c c)) cs)
                  (map (λ (c) (coerce-contract 'vector/c c)) cs))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (andmap flat-contract? ctcs)))
       (make-flat-vector/c ctcs immutable)]
      [(andmap chaperone-contract? ctcs)
       (make-chaperone-vector/c ctcs immutable)]
      [else
       (make-impersonator-vector/c ctcs immutable)])))

(define/subexpression-pos-prop (vector-immutable/c . args)
  (apply vector/c args #:immutable #t))
