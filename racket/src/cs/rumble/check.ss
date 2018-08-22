
(define-syntax (who stx)
  (syntax-error stx "not bound"))

(define-syntax-rule (define-define/who define/who define)
  (...
   (define-syntax (define/who stx)
     (syntax-case stx ()
       [(_ (id . args) body ...)
        #'(define id
            (fluid-let-syntax ([who (lambda (stx)
                                      #''id)])
              (lambda args body ...)))]
       [(_ id rhs)
        #'(define id
            (fluid-let-syntax ([who (lambda (stx)
                                      #''id)])
              rhs))]))))

(define-define/who define/who define)
(define-define/who define/lift/who define/lift)
(define-define/who define/no-lift/who define/no-lift)

(define-syntax (check stx)
  (syntax-case stx (:test :contract :or-false)
    [(_ who pred :contract ctc v)
     #`(unless (pred v)
         (raise-argument-error who ctc v))]
    [(_ who :test test-expr :contract ctc v)
     #`(unless test-expr
         (raise-argument-error who ctc v))]
    [(_ who :or-false pred v)
     #`(unless (or (not v) (pred v))
         (raise-argument-error who #,(format "(or/c #f ~a)" (syntax->datum #'pred)) v))]
    [(_ who pred :or-false v)
     #`(unless (or (not v) (pred v))
         (raise-argument-error who #,(format "(or/c ~a #f)" (syntax->datum #'pred)) v))]
    [(_ who pred v)
     #`(check who pred :contract #,(format "~a" (syntax->datum #'pred)) v)]))

(define-syntax (procedure-arity-includes/c stx)
  (syntax-case stx ()
    [(_ n)
     (let ([n (syntax->datum #'n)])
       (and (integer? n)
            (exact? n)
            (not (negative? n))))
     #'(lambda (p)
         (and (procedure? p)
              (procedure-arity-includes? p n)))]))

(define (check-space who what d-start d-len s-len)
  (unless (fx<= (fx+ d-start s-len) d-len)
    (raise-arguments-error who (string-append "not enough room in target " what)
                           "target length" d-len
                           "needed length" (fx+ d-start s-len))))

(define (check-range who what in-value start end len)
  (unless (<= start len)
    (raise-range-error who what "starting " start in-value 0 len))
  (when end
    (unless (<= start end len)
      (raise-range-error who what "ending " end in-value start len 0))))

(define (check-errno who errno)
  (check who
         :test (and (pair? errno)
                    (exact-integer? (car errno))
                    (chez:memq (cdr errno) '(posix windows gai)))
         :contract "(cons/c exact-integer? (or/c 'posix 'windows 'gai))"
         errno))

(define (check-integer who lo hi v)
  (unless (and (integer? v)
               (exact? v)
               (<= lo v hi))
    (raise-argument-error who v (format "(integer-in ~a ~a)" lo hi))))
