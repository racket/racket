
;; Continuation-mark key:
(define parameterization-key '#{parameterization n1kcvqw4c9hh8t3fi3659ci94-1})

(define-record-type parameterization
  (fields ht)
  (sealed #t))

(define empty-parameterization (make-parameterization empty-hasheq))

(define/who (extend-parameterization p . args)
  (check who parameterization? p)
  (let loop ([ht (parameterization-ht p)] [args args])
    (cond
     [(null? args) (make-parameterization ht)]
     [(and (parameter? (car args))
           (pair? (cdr args)))
      (let dloop ([p (car args)] [v (cadr args)])
        (cond
         [(impersonator? p)
          (dloop (impersonator-val p) (impersonate-apply/parameter p p #f (list v)))]
         [(derived-parameter? p)
          (dloop (derived-parameter-next p) (|#%app| (parameter-guard p) v))]
         [else
          (let* ([guard (parameter-guard p)]
                 [v (if guard
                        (|#%app| guard v)
                        v)])
            (loop (intmap-set ht (parameter-key p) (make-thread-cell v #t))
                  (cddr args)))]))]
     [(parameter? (car args))
      (raise-arguments-error 'extend-parameterization
                             "missing value for parameter"
                             "parameter" (car args))]
     [else
      (raise-argument-error 'extend-parameterization "parameter?" (car args))])))

(define (call-with-parameterization parameter value thunk)
  (with-continuation-mark
   parameterization-key
   (extend-parameterization (current-parameterization) parameter value)
   (thunk)))

;; Not exported; the one for `racket/base` is in `racket/private/more-scheme`
(define (current-parameterization)
  (continuation-mark-set-first
   #f
   parameterization-key
   empty-parameterization))

(define (parameter-cell key)
  (intmap-ref (parameterization-ht
               (current-parameterization))
              key
              #f))

(define-record-type parameter-data
  (fields guard
          (mutable name) ; not actually mutable, but ensures fresh allocations
          realm))

(define-record-type derived-parameter-data
  (parent parameter-data)
  (fields next))

(define (parameter? v)
  (authentic-parameter? (strip-impersonator v)))

(define (authentic-parameter? v)
  (and (wrapper-procedure? v)
       (parameter-data? (wrapper-procedure-data v))))

(define (parameter-guard p)
  (parameter-data-guard (wrapper-procedure-data p)))

(define (parameter-key p)
  (wrapper-procedure-data p))

(define (derived-parameter? v)
  (and (wrapper-procedure? v)
       (derived-parameter-data? (wrapper-procedure-data v))))

(define (derived-parameter-next p)
  (derived-parameter-data-next (wrapper-procedure-data p)))

(define/who make-parameter
  (case-lambda
    [(v) (make-parameter v #f 'parameter-procedure default-realm)]
    [(v guard) (make-parameter v guard 'parameter-procedure default-realm)]
    [(v guard name) (make-parameter v guard name default-realm)]
    [(v guard name realm)
     (check who (procedure-arity-includes/c 1) :or-false guard)
     (check who symbol? name)
     (check who symbol? realm)
     (let ([default-c (make-thread-cell v #t)]
           [data (make-parameter-data guard name realm)])
       (make-arity-wrapper-procedure
        (case-lambda
         [()
          (let ([c (or (parameter-cell data)
                       default-c)])
            (unsafe-thread-cell-ref c))]
         [(v)
          (let ([c (or (parameter-cell data)
                       default-c)])
            (thread-cell-set! c (if guard
                                    (|#%app| guard v)
                                    v)))])
        3
        data))]))

(define/who (make-derived-parameter p guard wrap)
  (check who authentic-parameter?
         :contract "(and/c parameter? (not/c impersonator?))"
         p)
  (check who (procedure-arity-includes/c 1) guard)
  (check who (procedure-arity-includes/c 1) wrap)
  (make-arity-wrapper-procedure
   (case-lambda
    [(v) (p (guard v))]
    [() (wrap (p))])
   3
   (make-derived-parameter-data
    guard
    (parameter-data-name
     (wrapper-procedure-data p))
    (parameter-data-realm
     (wrapper-procedure-data p))
    p)))

(define/who (parameter-procedure=? a b)
  (check who parameter? a)
  (check who parameter? b)
  (eq? (strip-impersonator a) (strip-impersonator b)))

(define/who (reparameterize p)
  (check who parameterization? p)
  p)

;; ----------------------------------------

(define/who current-inspector
  (make-parameter root-inspector
                  (lambda (v)
                    (check who inspector? v)
                    v)
                  'current-inspector))

(define/who current-code-inspector
  (make-parameter root-inspector
                  (lambda (v)
                    (check who inspector? v)
                    v)
                  'current-code-inspector))
