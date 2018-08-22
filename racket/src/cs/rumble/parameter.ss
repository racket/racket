
;; Continuation-mark key:
(define parameterization-key (gensym "parameterization-key"))

(define-record parameterization (ht))

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
          (dloop (impersonator-val p) (impersonate-apply/parameter p #f (list v)))]
         [(derived-parameter? p)
          (dloop (derived-parameter-next p) (|#%app| (parameter-guard p) v))]
         [else
          (let* ([guard (parameter-guard p)]
                 [v (if guard
                        (|#%app| guard v)
                        v)])
            (loop (intmap-set ht p (make-thread-cell v #t))
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

(define (current-parameterization)
  (continuation-mark-set-first
   #f
   parameterization-key
   empty-parameterization
   the-root-continuation-prompt-tag))

(define (parameter-cell key)
  (intmap-ref (parameterization-ht
               (current-parameterization))
              key
              #f))

(define-record-type (parameter create-parameter authentic-parameter?)
  (fields proc guard))

(define-record-type (derived-parameter create-derived-parameter derived-parameter?)
  (parent parameter)
  (fields next))

(define (parameter? v)
  (authentic-parameter? (strip-impersonator v)))

(define/who make-parameter
  (case-lambda
    [(v) (make-parameter v #f)]
    [(v guard)
     (check who (procedure-arity-includes/c 1) :or-false guard)
     (let ([default-c (make-thread-cell v #t)])
       (letrec ([self
                 (create-parameter
                  (case-lambda
                   [()
                    (let ([c (or (parameter-cell self)
                                 default-c)])
                      (thread-cell-ref c))]
                   [(v)
                    (let ([c (or (parameter-cell self)
                                 default-c)])
                      (thread-cell-set! c (if guard
                                              (guard v)
                                              v)))])
                  guard)])
         self))]))

(define/who (make-derived-parameter p guard wrap)
  (check who authentic-parameter?
         :contract "(and/c parameter? (not/c impersonator?))"
         p)
  (check who (procedure-arity-includes/c 1) guard)
  (check who (procedure-arity-includes/c 1) wrap)
  (create-derived-parameter (let ([self (parameter-proc p)])
                              (case-lambda
                               [(v) (self (guard v))]
                               [() (wrap (self))]))
                            guard
                            p))

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
                    v)))

(define/who current-code-inspector
  (make-parameter root-inspector
                  (lambda (v)
                    (check who inspector? v)
                    v)))
