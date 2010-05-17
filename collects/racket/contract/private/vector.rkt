#lang racket/base

(require (for-syntax racket/base)
         "guts.ss")

(provide (rename-out [wrap-vectorof vectorof]
                     [wrap-vector/c vector/c])
         vector-immutable/c vector-immutableof)

(define-struct vectorof (elem immutable))

(define (vectorof-name c)
  (let ([immutable (vectorof-immutable c)])
    (apply build-compound-type-name 'vectorof 
           (contract-name (vectorof-elem c))
           (append
            (if (and (flat-vectorof? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (vectorof-first-order c) 
  (let ([elem-ctc (vectorof-elem c)]
        [immutable (vectorof-immutable c)]
        [flat? (flat-vectorof? c)])
    (λ (val #:blame [blame #f])
      (let/ec return
        (define (fail . args)
          (if blame
              (apply raise-blame-error blame val args)
              (return #f)))
        (unless (vector? val)
          (fail "expected a vector, got ~a" val))
        (cond
          [(eq? immutable #t)
           (unless (immutable? val)
             (fail "expected an immutable vector, got ~a" val))]
          [(eq? immutable #f)
           (when (immutable? val)
             (fail "expected an mutable vector, got ~a" val))]
          [else (void)])
        (when (or flat? (and (immutable? val) (not blame)))
          (if blame
              (let ([elem-proj ((contract-projection elem-ctc) blame)])
                (for ([e (in-vector val)])
                  (elem-proj e)))
              (for ([e (in-vector val)])
                (unless (contract-first-order-passes? elem-ctc e)
                  (fail)))))
        #t))))

(define-struct (flat-vectorof vectorof) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (λ (val)
         ((vectorof-first-order ctc) val #:blame blame)
         val)))))

(define (vectorof-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctc (vectorof-elem ctc)]
           [immutable (vectorof-immutable ctc)])
       (λ (blame)
         (let ([elem-pos-proj ((contract-projection elem-ctc) blame)]
               [elem-neg-proj ((contract-projection elem-ctc) (blame-swap blame))])
           (λ (val)
             ((vectorof-first-order ctc) val #:blame blame)
             (if (immutable? val)
                 (apply vector-immutable
                        (for/list ([e (in-vector val)])
                          (elem-pos-proj e)))
                 (vector-wrapper
                  val
                  (λ (vec i val)
                    (elem-pos-proj val))
                  (λ (vec i val)
                    (elem-neg-proj val))))))))))

(define-struct (chaperone-vectorof vectorof) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection chaperone-vector)))

(define-struct (proxy-vectorof vectorof) ()
  #:property prop:contract
  (build-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection proxy-vector)))

(define-syntax (wrap-vectorof stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx build-vectorof)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vecof arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'ctc)])
       (define (convert-args args)
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
            (app build-vectorof new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vecof) null))))]))

(define (build-vectorof c #:immutable [immutable 'dont-care] #:flat? [flat? #f])
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
       (make-proxy-vectorof ctc immutable)])))

(define/subexpression-pos-prop (vector-immutableof c)
  (build-vectorof c #:immutable #t))

(define-struct vector/c (elems immutable))

(define (vector/c-name c)
  (let ([immutable (vector/c-immutable c)])
    (apply build-compound-type-name 'vector/c
           (append
            (map contract-name (vector/c-elems c))
            (if (and (flat-vector/c? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (vector/c-first-order c) 
  (let ([elem-ctcs (vector/c-elems c)]
        [immutable (vector/c-immutable c)]
        [flat? (flat-vector/c? c)])
    (λ (val #:blame [blame #f])
      (let/ec return
        (define (fail . args)
          (if blame
              (apply raise-blame-error blame val args)
              (return #f)))
        (unless (vector? val)
          (fail "expected a vector, got ~a" val))
        (cond
          [(eq? immutable #t)
           (unless (immutable? val)
             (fail "expected an immutable vector, got ~a" val))]
          [(eq? immutable #f)
           (when (immutable? val)
             (fail "expected an mutable vector, got ~a" val))]
          [else (void)])
        (let ([elem-count (length elem-ctcs)])
          (unless (= (vector-length val) elem-count)
            (fail "expected a vector of ~a element~a, got ~a"
                  elem-count (if (= elem-count 1) "" "s") val)))
        (when (or flat? (and (immutable? val) (not blame)))
          (if blame
              (for ([e (in-vector val)]
                    [c (in-list elem-ctcs)])
                (((contract-projection c) blame) e))
              (for ([e (in-vector val)]
                    [c (in-list elem-ctcs)])
                (unless (contract-first-order-passes? c e)
                  (fail)))))
        #t))))

(define-struct (flat-vector/c vector/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (λ (val)
         ((vector/c-first-order ctc) val #:blame blame)
         val)))))

(define (vector/c-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctcs (vector/c-elems ctc)]
           [immutable (vector/c-immutable ctc)])
       (λ (blame)
         (let ([elem-pos-projs (apply vector-immutable
                                      (map (λ (c) ((contract-projection c) blame)) elem-ctcs))]
               [elem-neg-projs (apply vector-immutable
                                      (map (λ (c) ((contract-projection c) (blame-swap blame))) elem-ctcs))])
           (λ (val)
             ((vector/c-first-order ctc) val #:blame blame)
             (if (immutable? val)
                 (apply vector-immutable
                        (for/list ([e (in-vector val)]
                                   [i (in-naturals)])
                          ((vector-ref elem-pos-projs i) e)))
                 (vector-wrapper
                  val
                  (λ (vec i val)
                    ((vector-ref elem-pos-projs i) val))
                  (λ (vec i val)
                    ((vector-ref elem-neg-projs i) val))))))))))

(define-struct (chaperone-vector/c vector/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection (vector/c-ho-projection chaperone-vector)))

(define-struct (proxy-vector/c vector/c) ()
  #:property prop:contract
  (build-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection (vector/c-ho-projection proxy-vector)))

(define-syntax (wrap-vector/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx build-vector/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vec/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'ctc)])
       (define (convert-args args)
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
       (with-syntax ([(new-arg ...) (convert-args args)]
                     [app (datum->syntax stx '#%app)])
         (syntax-property
          (syntax/loc stx
            (app build-vector/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vec/c) null))))]))

(define (build-vector/c #:immutable [immutable 'dont-care] #:flat? [flat? #f] . cs)
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
       (make-proxy-vector/c ctcs immutable)])))

(define/subexpression-pos-prop (vector-immutable/c . args)
  (apply build-vector/c args #:immutable #t))
