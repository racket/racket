
(define (unsafe-box*-cas+! b delta)
  (let ([v (unsafe-unbox* b)])
    (unless (unsafe-box*-cas! b v (+ v delta))
      (unsafe-box*-cas+! b delta))))

;; ----------------------------------------

(define-record box-chaperone chaperone (ref set))
(define-record box-impersonator impersonator (ref set))

(define (box? v)
  (or (#%box? v)
      (and (impersonator? v)
           (#%box? (impersonator-val v)))))

(define (unbox b)
  (if (#%box? b)
      (#3%unbox b)
      (impersonate-unbox b)))

(define (unsafe-unbox b)
  ;; must handle impersonators
  (unbox b))

(define/who (unbox* b)
  (if (#%box? b)
      (#3%unbox b)
      (bad-box*-op who #f b)))

(define (bad-box*-op who set? b)
  (raise-argument-error who
                        (if set?
                            "(and/c box? (not immutable?) (not impersonator?))"
                            "(and/c box? (not impersonator?))")
                        b))

(define (set-box! b v)
  (if (#%mutable-box? b)
      (#3%set-box! b v)
      (impersonate-set-box! b v)))

(define (unsafe-set-box! b v)
  ;; must handle impersonators
  (set-box! b v))

(define/who (set-box*! b v)
  (if (#%mutable-box? b)
      (#3%set-box! b v)
      (bad-box*-op who #t b)))

;; in schemified:
(define (unbox/check-undefined b name)
  (check-not-unsafe-undefined (#%unbox b) name))

;; in schemified:
(define (set-box!/check-undefined b v name)
  (check-not-unsafe-undefined/assign (#%unbox b) name)
  (#%set-box! b v))

(define/who (chaperone-box b ref set . props)
  (check who box? b)
  (do-impersonate-box 'chaperone-box make-box-chaperone b ref set
                         make-props-chaperone props))

(define/who (impersonate-box b ref set . props)
  (check who mutable-box? :contract "(and/c box? (not/c immutable?))" b)
  (do-impersonate-box 'impersonate-box make-box-impersonator b ref set
                      make-props-chaperone props))

(define (do-impersonate-box who make-box-impersonator b ref set
                            make-props-impersonator props)
  (check who (procedure-arity-includes/c 2) ref)
  (check who (procedure-arity-includes/c 2) set)
  (let ([val (if (impersonator? b)
                 (impersonator-val b)
                 b)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? b)
                                                (impersonator-props b)
                                                empty-hasheq))])
    (make-box-impersonator val b props ref set)))

(define (impersonate-unbox orig)
  (pariah
   (if (and (impersonator? orig)
            (#%box? (impersonator-val orig)))
       (let loop ([o orig])
         (cond
          [(#%box? o) (#%unbox o)]
          [(box-chaperone? o)
           (let* ([val (loop (impersonator-next o))]
                  [new-val ((box-chaperone-ref o) o val)])
             (unless (chaperone-of? new-val val)
               (raise-arguments-error 'unbox
                                      "chaperone produced a result that is not a chaperone of the original result"
                                      "chaperone result" new-val
                                      "original result" val))
             new-val)]
          [(box-impersonator? o)
           (let ([val  (loop (impersonator-next o))])
             ((box-impersonator-ref o) o val))]
          [else (loop (impersonator-next o))]))
       ;; Let primitive report the error:
       (#2%unbox orig))))

(define (impersonate-set-box! orig val)
  (pariah
   (cond
    [(not (and (impersonator? orig)
               (mutable-box? (impersonator-val orig))))
     ;; Let primitive report the error:
     (#2%set-box! orig val)]
    [else
     (let loop ([o orig] [val val])
       (cond
        [(#%box? o) (#2%set-box! o val)]
        [else
         (let ([next (impersonator-next o)])
           (cond
            [(box-chaperone? o)
             (let ([new-val ((box-chaperone-set o) next val)])
               (unless (chaperone-of? new-val val)
                 (raise-arguments-error 'set-box!
                                        "chaperone produced a result that is not a chaperone of the original result"
                                        "chaperone result" new-val
                                        "original result" val))
               (loop next val))]
            [(box-impersonator? o)
             (loop next ((box-impersonator-set o) next val))]
            [else (loop next val)]))]))])))

(define (set-box-impersonator-hash!)
  (record-type-hash-procedure (record-type-descriptor box-chaperone)
                              (lambda (c hash-code)
                                (hash-code (box (unbox c)))))
  (record-type-hash-procedure (record-type-descriptor box-impersonator)
                              (lambda (i hash-code)
                                (hash-code (box (unbox i))))))

;; ----------------------------------------

;; A wrapper to hide the pairness of weak pairs:
(define-record-type (weak-box create-weak-box weak-box?)
  (fields p))

(define (make-weak-box v)
  (create-weak-box (weak-cons v #t)))

(define/who weak-box-value
  (case-lambda
   [(v no-value)
    (check who weak-box? v)
    (let ([c (car (weak-box-p v))])
      (if (eq? c #!bwp)
          no-value
          c))]
   [(v) (weak-box-value v #f)]))
