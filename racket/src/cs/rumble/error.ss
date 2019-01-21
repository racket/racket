
(define raise
  (case-lambda
    [(v) (raise v #t)]
    [(v barrier?)
     (if barrier?
         (call-with-continuation-barrier
          (lambda ()
            (chez:raise v)))
         (chez:raise v))]))

;; ----------------------------------------

(define/who error-print-width
  (make-parameter 256
                  (lambda (v)
                    (check who
                           :test (and (integer? v)
                                      (exact? v)
                                      (>= v 3))
                           :contract "(and/c exact-integer? (>=/c 3))"
                           v)
                    v)))

(define/who error-value->string-handler
  (make-parameter (lambda (v len)
                    (cond
                     [(or (number? v)
                          (boolean? v)
                          (string? v)
                          (symbol? v))
                      (chez:format "~s" v)]
                     [else
                      "[?error-value->string-handler not ready?]"]))
                  (lambda (v)
                    (check who (procedure-arity-includes/c 2) v)
                    v)))

(define/who error-print-context-length
  (make-parameter 16
                  (lambda (v)
                    (check who exact-nonnegative-integer? v)
                    v)))

;; ----------------------------------------

(struct exn (message continuation-marks) :guard (lambda (msg cm who)
                                                  (check who string? msg)
                                                  (check who continuation-mark-set? cm)
                                                  (values (string->immutable-string msg)
                                                          cm)))
(struct exn:break exn (continuation) :guard (lambda (msg cm k who)
                                              (check who escape-continuation? k)
                                              (values msg cm k)))
(struct exn:break:hang-up exn:break ())
(struct exn:break:terminate exn:break ())
(struct exn:fail exn ())
(struct exn:fail:contract exn:fail ())
(struct exn:fail:contract:arity exn:fail:contract ())
(struct exn:fail:contract:divide-by-zero exn:fail:contract ())
(struct exn:fail:contract:non-fixnum-result exn:fail:contract ())
(struct exn:fail:contract:continuation exn:fail:contract ())
(struct exn:fail:contract:variable exn:fail:contract (id) :guard (lambda (msg cm id who)
                                                                   (check who symbol? id)
                                                                   (values msg cm id)))
(struct exn:fail:read exn:fail (srclocs) :guard (lambda (msg cm srclocs who)
                                                  (check who
                                                         :test (and (list srclocs)
                                                                    (andmap srcloc? srclocs))
                                                         :contract "(listof srcloc?)"
                                                         srclocs)
                                                  (values msg cm srclocs)))
(struct exn:fail:read:non-char exn:fail:read ())
(struct exn:fail:read:eof exn:fail:read ())
(struct exn:fail:filesystem exn:fail ())
(struct exn:fail:filesystem:exists exn:fail:filesystem ())
(struct exn:fail:filesystem:version exn:fail:filesystem ())
(struct exn:fail:filesystem:errno exn:fail:filesystem (errno) :guard (lambda (msg cm errno who)
                                                                       (check-errno who errno)
                                                                       (values msg cm errno)))
(struct exn:fail:network exn:fail ())
(struct exn:fail:network:errno exn:fail:network (errno) :guard (lambda (msg cm errno who)
                                                                 (check-errno who errno)
                                                                 (values msg cm errno)))
(struct exn:fail:out-of-memory exn:fail ())
(struct exn:fail:unsupported exn:fail ())
(struct exn:fail:user exn:fail ())

;; ----------------------------------------

(define (raise-arguments-error who what . more)
  (unless (symbol? who)
    (raise-argument-error 'raise-arguments-error "symbol?" who))
  (unless (string? what)
    (raise-argument-error 'raise-arguments-error "string?" what))
  (do-raise-arguments-error who what exn:fail:contract more))
  
(define (do-raise-arguments-error who what exn:fail:contract more)
  (raise
   (|#%app|
    exn:fail:contract
    (apply
     string-append
     (symbol->string who)
     ": "
     what
     (let loop ([more more])
       (cond
        [(null? more) '()]
        [(string? (car more))
         (cond
          [(null? more)
           (raise-arguments-error 'raise-arguments-error
                                  "missing value after field string"
                                  "string"
                                  (car more))]
          [else
           (cons (string-append "\n  "
                                (car more) ": "
                                (let ([val (cadr more)])
                                  (if (unquoted-printing-string? val)
                                      (unquoted-printing-string-value val)
                                      (error-value->string val))))
                 (loop (cddr more)))])]
        [else
         (raise-argument-error 'raise-arguments-error "string?" (car more))])))
    (current-continuation-marks))))

(define (do-raise-argument-error e-who tag who what pos arg args)
  (unless (symbol? who)
    (raise-argument-error e-who "symbol?" who))
  (unless (string? what)
    (raise-argument-error e-who "string?" what))
  (when pos
    (unless (and (integer? pos)
                 (exact? pos)
                 (not (negative? pos)))
      (raise-argument-error e-who "exact-nonnegative-integer?" pos)))
  (raise
   (|#%app|
    exn:fail:contract
    (string-append (symbol->string who)
                   ": contract violation\n  expected: "
                   (reindent what (string-length "  expected: "))
                   "\n  " tag ": "
                   (error-value->string
                    (if pos (list-ref (cons arg args) pos) arg))
                   (if (and pos (pair? args))
                       (apply
                        string-append
                        "\n  argument position: "
                        (nth-str (add1 pos))
                        "\n  other arguments...:"
                        (let loop ([pos pos] [args (cons arg args)])
                          (cond
                           [(null? args) '()]
                           [(zero? pos) (loop (sub1 pos) (cdr args))]
                           [else (cons (string-append "\n   " (error-value->string (car args)))
                                       (loop (sub1 pos) (cdr args)))])))
                       ""))
    (current-continuation-marks))))

(define (reindent s amt)
  (let loop ([i (string-length s)] [s s] [end (string-length s)])
    (cond
     [(zero? i)
      (if (= end (string-length s))
          s
          (substring s 0 end))]
     [else
      (let ([i (fx1- i)])
        (cond
         [(eqv? #\newline (string-ref s i))
          (string-append
           (loop i s (fx1+ i))
           (#%make-string amt #\space)
           (substring s (fx1+ i) end))]
         [else
          (loop i s end)]))])))

(define (error-value->string v)
  ((|#%app| error-value->string-handler)
   v
   (|#%app| error-print-width)))

(define raise-argument-error
  (case-lambda
    [(who what arg)
     (do-raise-argument-error 'raise-argument-error "given" who what #f arg #f)]
    [(who what pos arg . args)
     (do-raise-argument-error 'raise-argument-error "given" who what pos arg args)]))

(define (raise-result-error who what arg)
  (do-raise-argument-error 'raise-result-error "result" who what #f arg #f))

(define (do-raise-type-error e-who tag who what pos arg args)
  (unless (symbol? who)
    (raise-argument-error e-who "symbol?" who))
  (unless (string? what)
    (raise-argument-error e-who "string?" what))
  (when pos
    (unless (and (integer? pos)
                 (exact? pos)
                 (not (negative? pos)))
      (raise-argument-error e-who "exact-nonnegative-integer?" pos)))
  (raise
   (|#%app|
    exn:fail:contract
    (string-append (symbol->string who)
                   ": expected argument ot type <" what ">"
                   "; given: "
                   (error-value->string
                    (if pos (list-ref (cons arg args) pos) arg))
                   (if (and pos (pair? args))
                       (apply
                        string-append
                        "; other arguments:"
                        (let loop ([pos pos] [args (cons arg args)])
                          (cond
                           [(null? args) '()]
                           [(zero? pos) (loop (sub1 pos) (cdr args))]
                           [else (cons (string-append " " (error-value->string (car args)))
                                       (loop (sub1 pos) (cdr args)))])))
                       ""))
    (current-continuation-marks))))

(define raise-type-error
  (case-lambda
    [(who what arg)
     (do-raise-type-error 'raise-argument-error "given" who what #f arg #f)]
    [(who what pos arg . args)
     (do-raise-type-error 'raise-argument-error "given" who what pos arg args)]))

(define/who (raise-mismatch-error in-who what . more)
  (check who symbol? in-who)
  (check who string? what)
  (raise
   (|#%app|
    exn:fail:contract
    (apply
     string-append
     (symbol->string in-who)
     ": "
     what
     (let loop ([more more])
       (cond
        [(null? more) '()]
        [else
         (cons (error-value->string (car more))
               (loop (cdr more)))])))
    (current-continuation-marks))))

(define/who raise-range-error
  (case-lambda
   [(in-who
     type-description
     index-prefix
     index
     in-value
     lower-bound
     upper-bound
     alt-lower-bound)
    (check who symbol? in-who)
    (check who string? type-description)
    (check who string? index-prefix)
    (check who exact-integer? index)
    (check who exact-integer? lower-bound)
    (check who exact-integer? upper-bound)
    (check who :or-false exact-integer? alt-lower-bound)
    (raise
     (|#%app|
      exn:fail:contract
      (string-append (symbol->string in-who)
                     ": "
                     index-prefix "index is "
                     (cond
                      [(< upper-bound lower-bound)
                       (string-append "out of range for empty " type-description "\n"
                                      "  index: " (number->string index))]
                      [else
                       (string-append
                        (cond
                         [(and alt-lower-bound
                               (>= index alt-lower-bound)
                               (< index upper-bound))
                          (string-append "smaller than starting index\n"
                                         "  " index-prefix "index: " (number->string index) "\n"
                                         "  starting index: "  (number->string lower-bound) "\n")]
                         [else
                          (string-append "out of range\n"
                                         "  " index-prefix "index: " (number->string index) "\n")])
                        "  valid range: ["
                        (number->string (or alt-lower-bound lower-bound)) ", "
                        (number->string upper-bound) "]" "\n"
                        "  " type-description ": " (error-value->string in-value))]))
      (current-continuation-marks)))]
   [(who
     type-description
     index-prefix
     index
     in-value
     lower-bound
     upper-bound)
    (raise-range-error who
                       type-description
                       index-prefix
                       index
                       in-value
                       lower-bound
                       upper-bound
                       #f)]))

(define (arguments->context-string args)
  (cond
   [(null? args) ""]
   [else
    (apply string-append
           "\n  arguments...: "
           (let loop ([args args])
             (cond
              [(null? args) '()]
              [else (cons (string-append "\n   " (error-value->string (car args)))
                          (loop (cdr args)))])))]))

(define (do-raise-arity-error name arity-or-expect-string args)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (string-append
     (let ([name (if (procedure? name)
                     (object-name name)
                     name)])
       (if (symbol? name)
           (string-append (symbol->string name) ": ")
           ""))
     "arity mismatch;\n"
     " the expected number of arguments does not match the given number\n"
     (if (string? arity-or-expect-string)
         arity-or-expect-string
         (expected-arity-string arity-or-expect-string))
     "  given: " (number->string (length args))
     (arguments->context-string args))
    (current-continuation-marks))))

(define/who (raise-arity-error name arity . args)
  (check who (lambda (p) (or (symbol? name) (procedure? name)))
         :contract "(or/c symbol? procedure?)"
         name)
  (check who procedure-arity? arity)
  (do-raise-arity-error name arity args))
  
(define/who (raise-arity-mask-error name mask . args)
  (check who (lambda (p) (or (symbol? name) (procedure? name)))
         :contract "(or/c symbol? procedure?)"
         name)
  (check who exact-integer? mask)
  (apply raise-arity-error name (mask->arity mask) args))

(define (expected-arity-string arity)
  (let ([expected
         (lambda (s) (string-append "  expected: " s "\n"))])
    (cond
     [(number? arity) (expected (number->string arity))]
     [(arity-at-least? arity) (expected
                               (string-append "at least "
                                              (number->string (arity-at-least-value arity))))]
     [else ""])))

(define/who (raise-result-arity-error who num-expected-args where . args)
  (check who symbol? :or-false who)
  (check who exact-nonnegative-integer? num-expected-args)
  (check who string? :or-false where)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (string-append
     (if who (string-append (symbol->string who) ": ") "")
     "result arity mismatch;\n"
     " expected number of values not received\n"
     "  received: " (number->string (length args)) "\n" 
     "  expected: " (number->string num-expected-args)
     (or where "")
     (arguments->context-string args))
    (current-continuation-marks))))

(define (raise-binding-result-arity-error expected-args args)
  (apply raise-result-arity-error #f
         (length expected-args)
         "\n  at: local-binding form"
         args))

(define raise-unsupported-error
  (case-lambda
   [(id msg)
    (raise
     (|#%app|
      exn:fail:unsupported
      (string-append (symbol->string id) ": " msg)
      (current-continuation-marks)))]
   [(id) (raise-unsupported-error id "unsupported")]))

;; ----------------------------------------

(define-record-type (unquoted-printing-string new-unquoted-printing-string unquoted-printing-string?)
  (fields value))

(define make-unquoted-printing-string
  (let ([unquoted-printing-string
         (escapes-ok
           (lambda (s)
             (check 'unquoted-printing-string string? s)
             (new-unquoted-printing-string s)))])
    unquoted-printing-string))

;; ----------------------------------------

(define (nth-str n)
  (string-append
   (number->string n)
   (case (modulo n 10)
     [(1) "st"]
     [(2) "nd"]
     [(3) "rd"]
     [else "th"])))

;; ----------------------------------------

(define exception-handler-key (gensym "exception-handler-key"))

(define (default-uncaught-exception-handler exn)
  (let ([message (if (exn? exn)
                     (exn-message exn)
                     (string-append "uncaught exception: "
                                    (error-value->string exn)))])
    (unless (exn:break:hang-up? exn)
      (let ([display-handler (|#%app| error-display-handler)])
        (call-with-parameterization
         error-display-handler
         (if (eq? display-handler default-error-display-handler)
             emergency-error-display-handler
             default-error-display-handler)
         (lambda ()
           (call-with-exception-handler
            (make-nested-exception-handler "error display handler" exn)
            (lambda ()
              (call-with-break-disabled
               (lambda ()
                 (|#%app| display-handler message exn)))))))))
    (when (or (exn:break:hang-up? exn)
              (exn:break:terminate? exn))
      (engine-exit 1))
    (let ([escape-handler (|#%app| error-escape-handler)])
      (call-with-parameterization
       error-display-handler
       default-error-display-handler
       (lambda ()
         (call-with-parameterization
          error-escape-handler
          default-error-escape-handler
          (lambda ()
            (call-with-exception-handler
             (make-nested-exception-handler "error escape handler" exn)
             (lambda ()
               (call-with-break-disabled
                (lambda ()
                  (|#%app| escape-handler))))))))))
    ;; In case the escape handler doesn't escape:
    (default-error-escape-handler)))

(define-thread-local link-instantiate-continuations (make-ephemeron-eq-hashtable))

;; For `instantiate-linklet` to help report which linklet is being run:
(define (register-linklet-instantiate-continuation! k name)
  (when name
    (hashtable-set! link-instantiate-continuations k name)))

;; Convert a contination to a list of function-name and
;; source information. Cache the result half-way up the
;; traversal, so that it's amortized constant time.
(define-thread-local cached-traces (make-ephemeron-eq-hashtable))
(define (continuation->trace k)
  (call-with-values
   (lambda ()
     (let loop ([k k] [slow-k k] [move? #f])
       (cond
         [(or (not (#%$continuation? k))
              (eq? k #%$null-continuation))
          (values slow-k '())]
         [(hashtable-ref cached-traces k #f)
          => (lambda (l)
               (values slow-k l))]
         [else
          (let* ([name (or (let ([n (hashtable-ref link-instantiate-continuations
                                                   k
                                                   #f)])
                             (and n
                                  (string->symbol (format "body of ~a" n))))
                           (let* ([c (#%$continuation-return-code k)]
                                  [n (#%$code-name c)])
                             (if (special-procedure-name-string? n)
                                 #f
                                 n)))]
                 [desc
                  (let* ([ci (#%$code-info (#%$continuation-return-code k))]
                         [src (and
                               (code-info? ci)
                               (or
                                ;; when per-expression inspector info is available:
                                (find-rpi (#%$continuation-return-offset k) ci)
                                ;; when only per-function source location is available:
                                (code-info-src ci)))])
                    (and (or name src)
                         (cons name src)))])
            (#%$split-continuation k 0)
            (call-with-values
             (lambda () (loop (#%$continuation-link k) (if move? (#%$continuation-link slow-k) slow-k) (not move?)))
             (lambda (slow-k l)
               (let ([l (if desc
                            (cons desc l)
                            l)])
                 (when (eq? k slow-k)
                   (hashtable-set! cached-traces k l))
                 (values slow-k l)))))])))
   (lambda (slow-k l)
     l)))

(define (continuation->trace* k)
  (call-with-values
   (lambda ()
     (let loop ([k k] [slow-k k] [move? #f])
       (cond
         [(or (not (#%$continuation? k))
              (eq? k #%$null-continuation))
          (values slow-k '())]
         [else
          (let* ([name (or (let ([n #f])
                             (and n
                                  (string->symbol (format "body of ~a" n))))
                           (let* ([c (#%$continuation-return-code k)]
                                  [n (#%$code-name c)])
                             n))]
                 [desc
                  (let* ([ci (#%$code-info (#%$continuation-return-code k))]
                         [src (and
                               (code-info? ci)
                               (or
                                ;; when per-expression inspector info is available:
                                (find-rpi (#%$continuation-return-offset k) ci)
                                ;; when only per-function source location is available:
                                (code-info-src ci)))])
                    (and (or name src)
                         (cons name src)))])
            (#%$split-continuation k 0)
            (call-with-values
             (lambda () (loop (#%$continuation-link k) (if move? (#%$continuation-link slow-k) slow-k) (not move?)))
             (lambda (slow-k l)
               (let ([l (if desc
                            (cons desc l)
                            l)])
                 (when (eq? k slow-k)
                   (hashtable-set! cached-traces k l))
                 (values slow-k l)))))])))
   (lambda (slow-k l)
     l)))

(define (traces->context ls)
  (let loop ([l '()] [ls ls])
    (cond
     [(null? l)
      (if (null? ls)
          '()
          (loop (car ls) (cdr ls)))]
     [else
      (let* ([p (car l)]
             [name (car p)]
             [loc (and (cdr p)
                       (call-with-values (lambda ()
                                           (let* ([src (cdr p)]
                                                  [path (source-file-descriptor-path (source-object-sfd src))])
                                             (if (source-object-line src)
                                                 (values path
                                                         (source-object-line src)
                                                         (source-object-column src))
                                                 (values path
                                                         (source-object-bfp src)))))
                         (case-lambda
                          [() #f]
                          [(path line col) (|#%app| srcloc path line (sub1 col) #f #f)]
                          [(path pos) (|#%app| srcloc path #f #f (add1 pos) #f)])))])
        (if (or name loc)
            (cons (cons name loc) (loop (cdr l) ls))
            (loop (cdr l) ls)))])))

(define (default-error-display-handler msg v)
  (eprintf "~a" msg)
  (when (or (continuation-condition? v)
            (and (exn? v)
                 (not (exn:fail:user? v))))
    (let ([n (|#%app| error-print-context-length)])
      (unless (zero? n)
        (eprintf "\n  context...:")
        (let loop ([l (traces->context
                       (if (exn? v)
                           (continuation-mark-set-traces (exn-continuation-marks v))
                           (list (continuation->trace (condition-continuation v)))))]
                   [n n])
          (unless (or (null? l) (zero? n))
            (let* ([p (car l)]
                   [s (cdr p)])
              (cond
               [(and s
                     (srcloc-line s)
                     (srcloc-column s))
                (eprintf "\n   ~a:~a:~a" (srcloc-source s) (srcloc-line s) (srcloc-column s))
                (when (car p)
                  (eprintf ": ~a" (car p)))]
               [(and s (srcloc-position s))
                (eprintf "\n   ~a::~a" (srcloc-source s) (srcloc-position s))
                (when (car p)
                  (eprintf ": ~a" (car p)))]
               [(car p)
                (eprintf "\n   ~a" (car p))]))
            (loop (cdr l) (sub1 n)))))))
  (eprintf "\n"))

(define eprintf
  (lambda (fmt . args)
    (apply fprintf (current-error-port) fmt args)))

(define (emergency-error-display-handler msg v)
  (log-system-message 'error msg))

(define (set-error-display-eprintf! proc)
  (set! eprintf proc))

(define (default-error-escape-handler)
  (abort-current-continuation (default-continuation-prompt-tag) void))

(define (exn->string v)
  (format "~a~a"
          (if (who-condition? v)
              (format "~a: " (rewrite-who (condition-who v)))
              "")
          (cond
           [(exn? v)
            (exn-message v)]
           [(format-condition? v)
            (let-values ([(fmt irritants)
                          (rewrite-format (condition-message v)
                                          (condition-irritants v))])
              (apply format fmt irritants))]
           [(syntax-violation? v)
            (let ([show (lambda (s)
                          (cond
                           [(not s) ""]
                           [else (format " ~s" (syntax->datum s))]))])
              (format "~a~a~a"
                      (condition-message v)
                      (show (syntax-violation-form v))
                      (show (syntax-violation-subform v))))]
           [(message-condition? v)
            (condition-message v)]
           [else (format "~s" v)])))

(define (condition->exn v)
  (if (condition? v)
      (cond
       [(and (format-condition? v)
             (irritants-condition? v)
             (string-prefix? "incorrect number of arguments" (condition-message v))
             (let ([vs (condition-irritants v)])
               (and (pair? vs)
                    (or (#%procedure? (car vs))
                        (and (integer? (car vs))
                             (pair? (cdr vs))
                             (#%procedure? (cadr vs)))))))
        (let ([vs (condition-irritants v)])
          (if (#%procedure? (car vs))
              (make-arity-exn (car vs) #f)
              (make-arity-exn (cadr vs) (car vs))))]
       [else
        (|#%app|
         (condition->exception-constructor v)
         (exn->string v)
         (current-continuation-marks))])
      v))

(define (make-arity-exn proc n-args)
  (let* ([name (object-name proc)]
         [make-str (arity-string-maker proc)]
         [arity (procedure-arity proc)]
         [adjust-for-method (lambda (n)
                              (if (and (procedure-is-method? proc)
                                       (positive? n))
                                  (sub1 n)
                                  n))])
    (|#%app|
     exn:fail:contract:arity
     (string-append
      (if (symbol? name) (symbol->string name) "#<procedure>")
      ": arity mismatch;\n the expected number of arguments does not match the given number"
      (cond
       [make-str
        (let ([str (make-str)])
          (if (string? str)
              (string-append "\n  expected: " str)
              ""))]
       [(list? arity)
        ""]
       [else
        (string-append
         "\n  expected: "
         (cond
          [(arity-at-least? arity) (string-append "at least " (number->string
                                                               (adjust-for-method (arity-at-least-value arity))))]
          [else (number->string (adjust-for-method arity))]))])
      (cond
       [(not n-args) ""]
       [else (string-append "\n  given: " (number->string (adjust-for-method n-args)))]))
     (current-continuation-marks))))

(define/who uncaught-exception-handler
  (make-parameter default-uncaught-exception-handler
                  (lambda (v)
                    (check who (procedure-arity-includes/c 1) v)
                    v)))

(define/who error-display-handler
  (make-parameter default-error-display-handler
                  (lambda (v)
                    (check who (procedure-arity-includes/c 2) v)
                    v)))

(define/who error-escape-handler
  (make-parameter default-error-escape-handler
                  (lambda (v)
                    (check who (procedure-arity-includes/c 0) v)
                    v)))

(define (set-no-locate-source!)
  ;; Disable searching through the filesystem to convert a source +
  ;; position to line and column information. Instead, Racket
  ;; constructs source objects that preserve the line and column if
  ;; available.
  (current-locate-source-object-source
   (lambda (src start? cache?)
     (cond
      [(source-object-line src)
       ;; Line and column are available without searching
       (values (source-file-descriptor-path (source-object-sfd src))
               (source-object-column src)
               (source-object-column src))]
      [else
       ;; Don't search
       (values)]))))

(define (set-base-exception-handler!)
  (current-exception-state (create-exception-state))
  (base-exception-handler
   (lambda (v)
     #;(#%printf "~s\n" (exn->string v))
     #;(#%printf "~s\n" (continuation-mark-set-traces (current-continuation-marks)))
     (cond
      [(and (warning? v)
            (not (non-continuable-violation? v)))
       (log-system-message 'warning (exn->string exn))]
      [else
       (let ([hs (continuation-mark-set->list (current-continuation-marks/no-trace)
                                              exception-handler-key
                                              the-root-continuation-prompt-tag)]
             [init-v (condition->exn v)])
         (let ([call-with-nested-handler
                (lambda (thunk)
                  (call-with-exception-handler
                   (make-nested-exception-handler "exception handler" init-v)
                   (lambda ()
                     (call-with-break-disabled thunk))))])
           (let loop ([hs hs] [v init-v])
             (cond
              [(null? hs)
               (call-with-nested-handler
                (lambda () (|#%app| (|#%app| uncaught-exception-handler) v)))
               ;; Use `nested-exception-handler` if the uncaught-exception
               ;; handler doesn't escape:
               ((make-nested-exception-handler #f v) #f)]
              [else
               (let ([h (car hs)]
                     [hs (cdr hs)])
                 (let ([new-v (call-with-nested-handler
                               (lambda () (|#%app| h v)))])
                   (loop hs new-v)))]))))]))))

(define (make-nested-exception-handler what old-exn)
  (lambda (exn)
    (let ([msg
           (string-append
            (cond
             [(not what)
              "handler for uncaught exceptions: did not escape"]
             [else
              (string-append
               (cond [(exn? exn)
                      (string-append "exception raised by " what)]
                     [else
                      (string-append "raise called (with non-exception value) by " what)])
               ": "
               (if (exn? exn)
                   (exn-message exn)
                   (error-value->string exn)))])
            "; original "
            (if (exn? old-exn)
                "exception raised"
                "raise called (with non-exception value)")
            ": "
            (if (exn? old-exn)
                (exn-message old-exn)
                (error-value->string old-exn)))])
      (default-uncaught-exception-handler
        (|#%app| exn:fail msg (current-continuation-marks))))))

(define (call-with-exception-handler proc thunk)
  (with-continuation-mark exception-handler-key proc (thunk)))

;; ----------------------------------------

(define log-system-message void)

(define (set-log-system-message! proc)
  (set! log-system-message proc))
