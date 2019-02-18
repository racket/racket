(define/who make-vector
  (case-lambda
   [(n) (make-vector n 0)]
   [(n v)
    (unless (and (fixnum? n)
                 (fx< n 1000))
      (guard-large-allocation who 'vector n (foreign-sizeof 'void*)))
    (#2%make-vector n v)]))

;; ----------------------------------------

(define (vector-immutable . args)
  (if (null? args)
      (vector->immutable-vector '#())
      (let ([vec (apply vector args)])
        (#%$vector-set-immutable! vec)
        vec)))

;; ----------------------------------------

(define (vector? v)
  (or (#%vector? v)
      (and (impersonator? v)
           (#%vector? (impersonator-val v)))))

(define (mutable-vector? v)
  (or (#%mutable-vector? v)
      (and (impersonator? v)
           (#%mutable-vector? (impersonator-val v)))))

;; ----------------------------------------

(define-record vector-chaperone chaperone (ref set))
(define-record vector-impersonator impersonator (ref set))

(define/who (chaperone-vector vec ref set . props)
  (check who vector? vec)
  (do-impersonate-vector who make-vector-chaperone vec ref set
                         make-props-chaperone props))

(define/who (impersonate-vector vec ref set . props)
  (check who mutable-vector? :contract "(and/c vector? (not/c immutable?))" vec)
  (do-impersonate-vector who make-vector-impersonator vec ref set
                         make-props-impersonator props))

(define (do-impersonate-vector who make-vector-impersonator vec ref set
                               make-props-impersonator props)
  (check who (procedure-arity-includes/c 3) :or-false ref)
  (check who (procedure-arity-includes/c 3) :or-false set)
  (check-vector-wrapper-consistent who ref set)
  (let ([val (if (impersonator? vec)
                 (impersonator-val vec)
                 vec)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? vec)
                                                (impersonator-props vec)
                                                empty-hasheq))])
    (if (or ref set)
        (make-vector-impersonator val vec props ref set)
        (make-props-impersonator val vec props))))

(define (set-vector-impersonator-hash!)
  (record-type-hash-procedure (record-type-descriptor vector-chaperone)
                              (lambda (c hash-code)
                                (hash-code (vector-copy c))))
  (record-type-hash-procedure (record-type-descriptor vector-impersonator)
                              (lambda (i hash-code)
                                (hash-code (vector-copy i)))))

(define (check-vector-wrapper-consistent who ref set)
  (unless (eq? (not ref) (not set))
    (raise-arguments-error who
                           "accessor and mutator wrapper must be both `#f` or neither `#f`"
                           "accessor wrapper" ref
                           "mutator wrapper" set)))

;; ----------------------------------------

(define-record vector*-chaperone vector-chaperone ())
(define-record vector*-impersonator vector-impersonator ())

(define/who (chaperone-vector* vec ref set . props)
  (check who vector? vec)
  (do-impersonate-vector* who make-vector*-chaperone vec ref set
                          make-props-chaperone props))

(define/who (impersonate-vector* vec ref set . props)
  (check who mutable-vector? :contract "(and/c vector? (not/c immutable?))" vec)
  (do-impersonate-vector* who make-vector*-impersonator vec ref set
                          make-props-impersonator props))

(define (do-impersonate-vector* who make-vector*-impersonator vec ref set
                                make-props-impersonator props)
  (check who (procedure-arity-includes/c 4) :or-false ref)
  (check who (procedure-arity-includes/c 4) :or-false set)
  (check-vector-wrapper-consistent who ref set)
  (let ([val (if (impersonator? vec)
                 (impersonator-val vec)
                 vec)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? vec)
                                                (impersonator-props vec)
                                                empty-hasheq))])
    (if (or ref set)
        (make-vector*-impersonator val vec props ref set)
        (make-props-impersonator val vec props))))

;; ----------------------------------------

(define-record vector-unsafe-chaperone chaperone (vec))
(define-record vector-unsafe-impersonator impersonator (vec))

(define/who (unsafe-impersonate-vector vec alt-vec . props)
  (check who mutable-vector? :contract "(and/c vector? (not/c immutable?))" vec)
  (check who (lambda (p) (and (vector? p) (not (impersonator? p))))
         :contract "(and/c vector? (not/c impersonator?))"
         alt-vec)
  (do-unsafe-impersonate-vector who make-vector-unsafe-impersonator vec alt-vec props))

(define/who (unsafe-chaperone-vector vec alt-vec . props)
  (check who vector? vec)
  (check who (lambda (p) (and (vector? p) (not (impersonator? p))))
         :contract "(and/c vector? (not/c impersonator?))"
         alt-vec)
  (do-unsafe-impersonate-vector who make-vector-unsafe-chaperone vec alt-vec props))

(define (do-unsafe-impersonate-vector who make-vector-unsafe-impersonator vec alt-vec props)
  (let ([val (if (impersonator? vec)
                 (impersonator-val vec)
                 vec)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? vec)
                                                (impersonator-props vec)
                                                empty-hasheq))])
    (make-vector-unsafe-impersonator val vec props alt-vec)))

;; ----------------------------------------

(define (vector-length vec)
  (if (#%vector? vec)
      (#3%vector-length vec)
      (impersonate-vector-length vec)))

(define (unsafe-vector-length vec)
  (vector-length vec))

(define (vector*-length vec)
  (if (#%vector? vec)
      (#3%vector-length vec)
      (bad-vector*-for-length vec)))

(define (bad-vector*-for-length vec)
  (raise-argument-error 'vector*-length "(and/c vector? (not impersonator?))" vec))

(define (impersonate-vector-length vec)
  (pariah
   (if (and (impersonator? vec)
            (#%vector? (impersonator-val vec)))
       (cond
        [(vector-unsafe-chaperone? vec)
         (#%vector-length (vector-unsafe-chaperone-vec vec))]
        [(vector-unsafe-impersonator? vec)
         (#%vector-length (vector-unsafe-impersonator-vec vec))]
        [else
         (#%vector-length (impersonator-val vec))])
       ;; Let primitive report the error:
       (#2%vector-length vec))))

;; ----------------------------------------

(define (vector-ref vec idx)
  (if (#%$vector-ref-check? vec idx)
      (#3%vector-ref vec idx)
      (impersonate-vector-ref vec idx)))

(define (unsafe-vector-ref vec idx)
  (if (#%vector? vec)
      (#3%vector-ref vec idx)
      (impersonate-vector-ref vec idx)))

(define/who (vector*-ref vec idx)
  (if (#%$vector-ref-check? vec idx)
      (#3%vector-ref vec idx)
      (bad-vector*-op who #f vec idx)))

(define (bad-vector*-op who set? vec idx)
  (cond
   [set?
    (unless (#%mutable-vector? vec)
      (raise-argument-error who "(and/c vector? (not immutable?) (not impersonator?))" vec))]
   [else
    (unless (#%vector? vec)
      (raise-argument-error who "(and/c vector? (not impersonator?))" vec))])
  (check who exact-nonnegative-integer? idx)
  (check-range who "vector" vec idx #f (fx- (#%vector-length vec) 1)))

(define (impersonate-vector-ref orig idx)
  (pariah
   (if (and (impersonator? orig)
            (#%vector? (impersonator-val orig)))
       (let loop ([o orig])
         (cond
          [(#%vector? o) (#2%vector-ref o idx)]
          [(vector-chaperone? o)
           (let* ([o-next (impersonator-next o)]
                  [val (loop o-next)]
                  [new-val (if (vector*-chaperone? o)
                               ((vector-chaperone-ref o) orig o-next idx val)
                               ((vector-chaperone-ref o) o-next idx val))])
             (unless (chaperone-of? new-val val)
               (raise-arguments-error 'vector-ref
                                      "chaperone produced a result that is not a chaperone of the original result"
                                      "chaperone result" new-val
                                      "original result" val))
             new-val)]
          [(vector-impersonator? o)
           (let* ([o-next (impersonator-next o)]
                  [val (loop o-next)])
             (if (vector*-impersonator? o)
                 ((vector-impersonator-ref o) orig o-next idx val)
                 ((vector-impersonator-ref o) o-next idx val)))]
          [(vector-unsafe-impersonator? o)
           (vector-ref (vector-unsafe-impersonator-vec o)  idx)]
          [(vector-unsafe-chaperone? o)
           (vector-ref (vector-unsafe-chaperone-vec o)  idx)]
          [else (loop (impersonator-next o))]))
       ;; Let primitive report the error:
       (#2%vector-ref orig idx))))

;; ----------------------------------------

(define (vector-set! vec idx val)
  (if (#%$vector-set!-check? vec idx)
      (#3%vector-set! vec idx val)
      (impersonate-vector-set! vec idx val)))

(define (unsafe-vector-set! vec idx val)
  (if (#%vector? vec)
      (#3%vector-set! vec idx val)
      (impersonate-vector-set! vec idx val)))

(define/who (vector*-set! vec idx val)
  (if (#%$vector-set!-check? vec idx)
      (#3%vector-set! vec idx val)
      (bad-vector*-op who #t vec idx)))

(define (impersonate-vector-set! orig idx val)
  (pariah
   (cond
    [(not (and (impersonator? orig)
               (mutable-vector? (impersonator-val orig))))
     ;; Let primitive report the error:
     (#2%vector-set! orig idx val)]
    [(or (not (exact-nonnegative-integer? idx))
         (>= idx (vector-length (impersonator-val orig))))
     ;; Let primitive report the index error:
     (#2%vector-set! (impersonator-val orig) idx val)]
    [else
     (let loop ([o orig] [val val])
       (cond
        [(#%vector? o) (#2%vector-set! o idx val)]
        [else
         (let ([next (impersonator-next o)])
           (cond
            [(vector-chaperone? o)
             (let ([new-val (if (vector*-chaperone? o)
                                ((vector-chaperone-set o) orig next idx val)
                                ((vector-chaperone-set o) next idx val))])
               (unless (chaperone-of? new-val val)
                 (raise-arguments-error 'vector-set!
                                        "chaperone produced a result that is not a chaperone of the original result"
                                        "chaperone result" new-val
                                        "original result" val))
               (loop next val))]
            [(vector-impersonator? o)
             (loop next
                   (if (vector*-impersonator? o)
                       ((vector-impersonator-set o) orig next idx val)
                       ((vector-impersonator-set o) next idx val)))]
            [(vector-unsafe-impersonator? o)
             (#2%vector-set! (vector-unsafe-impersonator-vec o) idx val)]
            [(vector-unsafe-chaperone? o)
             (#2%vector-set! (vector-unsafe-chaperone-vec o) idx val)]
            [else (loop next val)]))]))])))

;; ----------------------------------------

(define/who (vector->list vec)
  (cond
   [(#%vector? vec)
    (#3%vector->list vec)]
   [(vector? vec)
    (let ([len (vector-length vec)])
      (let loop ([i len] [accum '()])
        (cond
         [(fx= i 0) accum]
         [else
          (let ([i (fx- i 1)])
            (loop i (cons (vector-ref vec i) accum)))])))]
   [else
    (raise-argument-error who "vector?" vec)]))

;; ----------------------------------------

(define/who (vector-copy vec)
  (cond
   [(#%vector? vec)
    (#3%vector-copy vec)]
   [(vector? vec)
    (let* ([len (vector-length vec)]
           [vec2 (make-vector len)])
      (vector-copy! vec2 0 vec)
      vec2)]
   [else
    (raise-argument-error who "vector?" vec)]))

(define/who vector-copy!
  (case-lambda
   [(dest d-start src)
    (vector-copy! dest d-start src 0 (and (vector? src) (vector-length src)))]
   [(dest d-start src s-start)
    (vector-copy! dest d-start src s-start (and (vector? src) (vector-length src)))]
   [(dest d-start src s-start s-end)
    (check who mutable-vector? :contract "(and/c vector? (not/c immutable?))" dest)
    (check who exact-nonnegative-integer? d-start)
    (check who vector? src)
    (check who exact-nonnegative-integer? s-start)
    (check who exact-nonnegative-integer? s-end)
    (let ([d-len (vector-length dest)])
      (check-range who "vector" dest d-start #f d-len)
      (check-range who "vector" src s-start s-end (vector-length src))
      (let ([len (fx- s-end s-start)])
        (check-space who "vector" d-start d-len len)
        (cond
         [(and (#%vector? src) (#%vector? dest))
          (vector*-copy! dest d-start src s-start s-end)]
         [(and (eq? (strip-impersonator dest)
                    (strip-impersonator src))
               (< d-start s-start))
          ;; Need to copy from low to high to be memmove-like
          (let loop ([i 0])
            (unless (fx= i len)
              (vector-set! dest (fx+ d-start i) (vector-ref src (fx+ s-start i)))
              (loop (fx+ i 1))))]
         [else
          (let loop ([i len])
            (unless (fx= 0 i)
              (let ([i (fx1- i)])
                (vector-set! dest (fx+ d-start i) (vector-ref src (fx+ s-start i)))
                (loop i))))])))]))

;; Like `vector-copy!`, but doesn't work on impersonators, and doesn't
;; add its own tests on the vector or range (so unsafe if Rumble is
;; compiled as unsafe)
(define/who vector*-copy!
  (case-lambda
   [(dest dest-start src)
    (vector*-copy! dest dest-start src 0 (#%vector-length src))]
   [(src src-start dest dest-start)
    (vector*-copy! dest dest-start src src-start (#%vector-length src))]
   [(dest dest-start src src-start src-end)
    (let ([len (fx- src-end src-start)])
      (cond
       [(and (eq? (strip-impersonator dest)
                  (strip-impersonator src))
             (< dest-start src-start))
        ;; Need to copy from low to high to be memmove-like
        (let loop ([i 0])
          (unless (fx= len i)
            (#%vector-set! dest (fx+ dest-start i) (vector-ref src (fx+ src-start i)))
            (loop (fx+ i 1))))]
       [else
        (let loop ([i len])
          (unless (fx= 0 i)
            (let ([i (fx1- i)])
              (#%vector-set! dest (fx+ dest-start i) (vector-ref src (fx+ src-start i)))
              (loop i))))]))]))

(define/who vector->values
  (case-lambda
   [(vec)
    (check who vector? vec)
     (let ([len (vector-length vec)])
       (cond
        [(fx= len 0) (values)]
        [(fx= len 1) (vector-ref vec 0)]
        [(fx= len 2) (values (vector-ref vec 0) (vector-ref vec 1))]
        [(fx= len 3) (values (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2))]
        [else (chez:apply values (vector->list vec))]))]
   [(vec start)
    (vector->values vec start (and (vector? vec) (vector-length vec)))]
   [(vec start end)
    (check who vector? vec)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (check-range who "vector" vec start end (vector-length vec))
    (chez:apply values
                (let loop ([start start])
                  (cond
                   [(fx= start end) null]
                   [else (cons (vector-ref vec start)
                               (loop (fx1+ start)))])))]))

(define/who (vector-fill! vec v)
  (cond
   [(#%vector? vec)
    (#3%vector-fill! vec v)]
   [(vector? vec)
    (check who mutable-vector? :contract "(and/c vector? (not immutable?))" v)
    (let ([len (vector-length vec)])
      (let loop ([i 0])
        (unless (= i len)
          (vector-set! vec i v)
          (loop (fx1+ i)))))]
   [else
    (raise-argument-error who "vector?" vec)]))

(define/who (vector->immutable-vector v)
  (cond
   [(#%vector? v)
    (#3%vector->immutable-vector v)]
   [(vector? v)
    (if (mutable-vector? v)
        (#3%vector->immutable-vector
         (vector-copy v))
        v)]
   [else
    (raise-argument-error who "vector?" v)]))

(define (shared-fxvector . args)
  (register-place-shared (apply fxvector args)))

(define make-shared-fxvector
  (case-lambda
   [(size) (make-shared-fxvector size 0)]
   [(size init)
    (register-place-shared (make-fxvector size init))]))
