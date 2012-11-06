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
                    (car args)
                    'racket/contract:positive-position
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

(define (vectorof-first-order ctc)
  (let ([check (check-vectorof ctc)])
    (λ (val)
      (let/ec return
        (check val (λ _ (return #f)) #t)))))

(define-struct (flat-vectorof base-vectorof) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection 
   (λ (ctc) 
     (define check (check-vectorof ctc))
     (λ (blame) 
       (define raise-blame (λ (val . args) 
                              (apply raise-blame-error blame val args)))
       (λ (val)
         (check val raise-blame #f)
         (let* ([elem-ctc (base-vectorof-elem ctc)]
                [p ((contract-projection elem-ctc) blame)])
           (for ([e (in-vector val)])
             (p e)))
         val)))))

(define (vectorof-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctc (base-vectorof-elem ctc)]
           [immutable (base-vectorof-immutable ctc)]
           [check (check-vectorof ctc)])
       (λ (blame)
         (let ([elem-pos-proj ((contract-projection elem-ctc) (blame-add-context blame "an element of"))]
               [elem-neg-proj ((contract-projection elem-ctc) (blame-add-context blame "an element of" #:swap? #t))])
           (define checked-ref (λ (vec i val) (elem-pos-proj val)))
           (define checked-set (λ (vec i val) (elem-neg-proj val)))
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
                  impersonator-prop:contracted ctc))))))))

(define-struct (chaperone-vectorof base-vectorof) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection chaperone-vector)))

(define-struct (impersonator-vectorof base-vectorof) ()
  #:property prop:contract
  (build-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
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

(define (vectorof c #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (let ([ctc (if flat?
                 (coerce-flat-contract 'vectorof c)
                 (coerce-contract 'vectorof c))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (flat-contract? ctc)))
       (make-flat-vectorof ctc immutable)]
      [(chaperone-contract? ctc)
       (make-chaperone-vectorof ctc immutable)]
      [else
       (make-impersonator-vectorof ctc immutable)])))

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

(define-struct (flat-vector/c base-vector/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (define blame+ctxt (blame-add-context blame "an element of"))
       (λ (val)
         (check-vector/c ctc val blame)
         (for ([e (in-vector val)]
               [c (in-list (base-vector/c-elems ctc))])
           (((contract-projection c) blame+ctxt) e))
         val)))))

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
                                  (blame-add-context blame (format "the ~a element of" (n->th i)) #:swap? #t)))])
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
                    ((vector-ref elem-pos-projs i) val))
                  (λ (vec i val)
                    ((vector-ref elem-neg-projs i) val))
                  impersonator-prop:contracted ctc))))))))

(define-struct (chaperone-vector/c base-vector/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection (vector/c-ho-projection chaperone-vector)))

(define-struct (impersonator-vector/c base-vector/c) ()
  #:property prop:contract
  (build-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
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
