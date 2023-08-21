
(define raise
  (case-lambda
   [(v) (#%$app/no-return do-raise/barrier v)]
   [(v barrier?)
    (if barrier?
        (#%$app/no-return do-raise/barrier v)
        (#%$app/no-return do-raise v))]))

(define (do-raise/barrier v)
  (assert-not-in-uninterrupted 'raise)
  (call-with-continuation-barrier
   (lambda ()
     (do-raise v))))
  
(define (do-raise v)
  (let ([get-next-h (continuation-mark-set->iterator (current-continuation-marks/no-trace)
                                                     (list exception-handler-key)
                                                     #f
                                                     the-root-continuation-prompt-tag)]
        [init-v (condition->exn v)])
    (let ([call-with-nested-handler
           (lambda (thunk)
             (call-with-exception-handler
              (make-nested-exception-handler "exception handler" init-v)
              (lambda ()
                (call-with-break-disabled thunk))))])
      (let loop ([get-next-h get-next-h] [v init-v])
        (let-values ([(hv get-next-h) (get-next-h)])
          (cond
           [(not hv)
            (call-with-nested-handler
             (lambda () (|#%app| (|#%app| uncaught-exception-handler) v)))
            ;; Use `nested-exception-handler` if the uncaught-exception
            ;; handler doesn't escape:
            ((make-nested-exception-handler #f v) #f)]
           [else
            (let ([h (vector-ref hv 0)])
              (let ([new-v (call-with-nested-handler
                            (lambda () (|#%app| h v)))])
                (loop get-next-h new-v)))]))))))

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
                    v)
                  'error-print-width
                  primitive-realm))

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
                    v)
                  'error-value->string-handler
                  primitive-realm))

(define/who error-syntax->string-handler
  (make-parameter (lambda (v len)
                    (#%format "~s" v))
                  (lambda (v)
                    (check who (procedure-arity-includes/c 2) v)
                    v)
                  'error-syntax->string-handler
                  primitive-realm))

(define/who error-print-context-length
  (make-parameter 16
                  (lambda (v)
                    (check who exact-nonnegative-integer? v)
                    v)
                  'error-print-context-length
                  primitive-realm))

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

(define (set-exn-srcloc-properties!)
  (let ([add! (lambda (rtd)
                (struct-property-set! prop:exn:srclocs rtd exn:fail:read-srclocs)
                (hashtable-set! rtd-props rtd (list prop:exn:srclocs)))])
    (add! struct:exn:fail:read)
    (add! struct:exn:fail:read:non-char)
    (add! struct:exn:fail:read:eof)))

;; ----------------------------------------

;; this is the real `raise-arguments-error`:
(define raise-arguments-error/user
  (|#%name|
   raise-arguments-error
   (lambda (who-in what . more)
     (#%$app/no-return do-raise-arguments-error 'raise-arguments-error who-in default-realm what exn:fail:contract more))))

(define/who (raise-arguments-error who-in what . more)
  (#%$app/no-return do-raise-arguments-error who who-in primitive-realm what exn:fail:contract more))

(define/who (raise-arguments-error* who-in realm what . more)
  (#%$app/no-return do-raise-arguments-error who who-in realm what exn:fail:contract more))
  
(define (do-raise-arguments-error e-who who realm what exn:fail:contract more)
  (assert-not-in-uninterrupted 'do-raise-arguments-error)
  (check e-who symbol? who)
  (check e-who symbol? realm)
  (check e-who string? what)
  (raise
   (|#%app|
    exn:fail:contract
    (error-message->adjusted-string
     who realm
     (apply
      string-append
      what
      (let loop ([more more])
        (cond
          [(null? more) '()]
          [(string? (car more))
           (cond
             [(null? (cdr more))
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
     realm)
    (current-continuation-marks))))

;; this is the real `raise-argument-error`:
(define raise-argument-error/user
  (|#%name|
   raise-argument-error
   (case-lambda
    [(who-in what arg)
     (#%$app/no-return do-raise-argument-error 'raise-argument-error "given" who-in default-realm what #f arg #f)]
    [(who-in what pos arg . args)
     (#%$app/no-return do-raise-argument-error 'raise-argument-error "given" who-in default-realm what pos arg args)])))

(define/who raise-argument-error
   (case-lambda
    [(who-in what arg)
     (#%$app/no-return do-raise-argument-error who "given" who-in primitive-realm what #f arg #f)]
    [(who-in what pos arg . args)
     (#%$app/no-return do-raise-argument-error who "given" who-in primitive-realm what pos arg args)]))

(define/who raise-argument-error*
   (case-lambda
    [(who-in realm what arg)
     (#%$app/no-return do-raise-argument-error who "given" who-in realm what #f arg #f)]
    [(who-in realm what pos arg . args)
     (#%$app/no-return do-raise-argument-error who "given" who-in realm what pos arg args)]))

(define/who raise-result-error
  (case-lambda
    [(who-in what arg)
     (#%$app/no-return do-raise-argument-error who "result" who-in default-realm what #f arg #f)]
    [(who-in what pos arg . args)
     (#%$app/no-return do-raise-argument-error who "result" who-in default-realm what pos arg args)]))

(define/who raise-result-error*
  (case-lambda
    [(who-in realm what arg)
     (#%$app/no-return do-raise-argument-error who "result" who-in realm what #f arg #f)]
    [(who-in realm what pos arg . args)
     (#%$app/no-return do-raise-argument-error who "result" who-in realm what pos arg args)]))

(define (do-raise-argument-error e-who tag who realm what pos arg args)
  (assert-not-in-uninterrupted 'do-raise-argument-error)
  (check e-who symbol? who)
  (check e-who symbol? realm)
  (check e-who string? what)
  (when pos
    (unless (and (integer? pos)
                 (exact? pos)
                 (not (negative? pos)))
      (raise-argument-error e-who "exact-nonnegative-integer?" pos)))
  (raise
   (|#%app|
    exn:fail:contract
    (error-message->adjusted-string
     who realm
     (string-append "contract violation\n  expected: "
                    (reindent (error-contract->adjusted-string what realm)
                              (string-length "  expected: "))
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
     realm)
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
  (let ([s (|#%app|
            (|#%app| error-value->string-handler)
            v
            (|#%app| error-print-width))])
    (cond
      [(string? s) s]
      [(bytes? s)
       ;; Racket BC allows byte strings, and we approximate that here
       (utf8->string s)]
      [else "..."])))

(define raise-type-error
  (case-lambda
    [(who what arg)
     (#%$app/no-return do-raise-type-error 'raise-argument-error "given" who what #f arg #f)]
    [(who what pos arg . args)
     (#%$app/no-return do-raise-type-error 'raise-argument-error "given" who what pos arg args)]))

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
    (error-message->adjusted-string
     who default-realm
     (string-append-immutable
      "expected argument of type <" what ">"
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
     default-realm)
    (current-continuation-marks))))

(define/who (raise-mismatch-error in-who what v . more)
  (check who symbol? in-who)
  (check who string? what)
  (raise
   (|#%app|
    exn:fail:contract
    (error-message->adjusted-string
     in-who default-realm
     (apply
      string-append-immutable
      what
      (let loop ([more (cons v more)])
        (cond
          [(null? more) '()]
          [else
           (cons (error-value->string (car more))
                 (loop (cdr more)))])))
     default-realm)
    (current-continuation-marks))))

;; this is the real `raise-range-error`:
(define raise-range-error/user
  (|#%name|
   raise-range-error/user
   (case-lambda
    [(in-who
      type-description
      index-prefix
      index
      in-value
      lower-bound
      upper-bound
      alt-lower-bound)
     (do-raise-range-error 'raise-range-error
                           in-who
                           default-realm
                           type-description
                           index-prefix
                           index
                           in-value
                           lower-bound
                           upper-bound
                           alt-lower-bound)]
    [(in-who
      type-description
      index-prefix
      index
      in-value
      lower-bound
      upper-bound)
     (raise-range-error/user in-who
                             type-description
                             index-prefix
                             index
                             in-value
                             lower-bound
                             upper-bound
                             #f)])))

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
    (do-raise-range-error who
                          in-who
                          primitive-realm
                          type-description
                          index-prefix
                          index
                          in-value
                          lower-bound
                          upper-bound
                          alt-lower-bound)]
   [(in-who
     type-description
     index-prefix
     index
     in-value
     lower-bound
     upper-bound)
    (raise-range-error in-who
                       type-description
                       index-prefix
                       index
                       in-value
                       lower-bound
                       upper-bound
                       #f)]))

(define/who raise-range-error*
  (case-lambda
   [(in-who
     realm
     type-description
     index-prefix
     index
     in-value
     lower-bound
     upper-bound
     alt-lower-bound)
    (do-raise-range-error who
                          in-who
                          primitive-realm
                          type-description
                          index-prefix
                          index
                          in-value
                          lower-bound
                          upper-bound
                          alt-lower-bound)]
   [(in-who
     realm
     type-description
     index-prefix
     index
     in-value
     lower-bound
     upper-bound)
    (raise-range-error* in-who
                        realm
                        type-description
                        index-prefix
                        index
                        in-value
                        lower-bound
                        upper-bound
                        #f)]))

(define (do-raise-range-error e-who
                              in-who
                              realm
                              type-description
                              index-prefix
                              index
                              in-value
                              lower-bound
                              upper-bound
                              alt-lower-bound)
    (check e-who symbol? in-who)
    (check e-who string? type-description)
    (check e-who string? index-prefix)
    (check e-who exact-integer? index)
    (check e-who exact-integer? lower-bound)
    (check e-who exact-integer? upper-bound)
    (check e-who :or-false exact-integer? alt-lower-bound)
  (raise
   (|#%app|
    exn:fail:contract
    (error-message->adjusted-string
     in-who realm
     (string-append-immutable
      index-prefix "index is "
      (cond
        [(< upper-bound lower-bound)
         (string-append-immutable "out of range for empty " type-description "\n"
                                  "  index: " (number->string index))]
        [else
         (string-append-immutable
          (cond
            [(and alt-lower-bound
                  (>= index alt-lower-bound)
                  (< index upper-bound))
             (string-append-immutable "smaller than starting index\n"
                                      "  " index-prefix "index: " (number->string index) "\n"
                                      "  starting index: "  (number->string lower-bound) "\n")]
            [else
             (string-append-immutable "out of range\n"
                                      "  " index-prefix "index: " (number->string index) "\n")])
          "  valid range: ["
          (number->string (or alt-lower-bound lower-bound)) ", "
          (number->string upper-bound) "]" "\n"
          "  " type-description ": " (error-value->string in-value))]))
     realm)
    (current-continuation-marks))))

(define (arguments->context-string args)
  (cond
   [(null? args) ""]
   [else
    (apply string-append
           "\n  arguments...:"
           (let loop ([args args])
             (cond
              [(null? args) '()]
              [else (cons (string-append "\n   " (error-value->string (car args)))
                          (loop (cdr args)))])))]))

(define/who (raise-arity-error name arity . args)
  (|#%app/no-return| do-raise-arity-error who name default-realm arity #f args))

(define/who (raise-arity-mask-error name arity-mask . args)
  (|#%app/no-return| do-raise-arity-error who name default-realm arity-mask #t args))

(define/who (raise-arity-error* name realm arity . args)
  (|#%app/no-return| do-raise-arity-error who name realm arity #f args))

(define/who (raise-arity-mask-error* name realm arity-mask . args)
  (|#%app/no-return| do-raise-arity-error who name realm arity-mask #t args))

(define (do-raise-arity-error who name realm arity/mask mask? args)
  (check who (lambda (p) (or (symbol? name) (procedure? name)))
         :contract "(or/c symbol? procedure?)"
         name)
  (check who symbol? realm)
  (if mask?
      (check who exact-integer? arity/mask)
      (check who procedure-arity? arity/mask))
  (do-arity-error name realm (if mask? (mask->arity arity/mask) arity/mask) args))

(define (do-arity-error name realm arity-or-expect-string args)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (error-message->adjusted-string
     (if (procedure? name)
         (object-name name)
         name)
     (if (procedure? name)
         (procedure-realm name)
         (or realm default-realm))
     (string-append
      "arity mismatch;\n"
      " the expected number of arguments does not match the given number\n"
      (if (string? arity-or-expect-string)
          arity-or-expect-string
          (expected-arity-string arity-or-expect-string))
      "  given: " (number->string (length args))
      (arguments->context-string args))
     primitive-realm)
    (current-continuation-marks))))

(define (expected-arity-string arity)
  (let ([expected
         (lambda (s) (string-append "  expected: " s "\n"))])
    (cond
     [(number? arity) (expected (number->string arity))]
     [(arity-at-least? arity) (expected
                               (string-append "at least "
                                              (number->string (arity-at-least-value arity))))]
     [else ""])))

(define/who (raise-result-arity-error who-in num-expected-args where . args)
  (do-raise-result-arity-error who who-in default-realm num-expected-args where args))

(define/who (raise-result-arity-error* who-in realm num-expected-args where . args)
  (do-raise-result-arity-error who who-in realm num-expected-args where args))

(define/who (do-raise-result-arity-error who who-in realm num-expected-args where args)
  (check who symbol? :or-false who)
  (check who exact-nonnegative-integer? num-expected-args)
  (check who string? :or-false where)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (error-message->adjusted-string
     who-in realm
     (string-append
      "result arity mismatch;\n"
      " expected number of values not received\n"
      "  expected: " (number->string num-expected-args) "\n"
      "  received: " (number->string (length args))
      (or where "")
      (arguments->context-string args))
     primitive-realm)
    (current-continuation-marks))))

(define (raise-binding-result-arity-error expected-args args)
  (do-raise-result-arity-error
   'internal
   #f
   default-realm
   (if (integer? expected-args)
       expected-args
       (length expected-args))
   "\n  in: local-binding form"
   args))

(define (raise-definition-result-arity-error expected-args args)
  (do-raise-result-arity-error
   'internal
   'define-values
   primitive-realm
   (length expected-args)
   (if (null? expected-args)
       ""
       (string-append "\n  in: definition of "
                      (symbol->string (car expected-args))
                      " ..."))
   args))

(define (do-raise-unsupported-error who name realm msg)
  (check who symbol? name)
  (check who symbol? realm)
  (check who string? msg)
  (raise
   (|#%app|
    exn:fail:unsupported
    (error-message->adjusted-string
     name realm
     msg
     realm)
    (current-continuation-marks))))
   
(define/who raise-unsupported-error
  (case-lambda
   [(name msg)
    (|#%app/no-return| do-raise-unsupported-error who name primitive-realm msg)]
   [(name)
    (|#%app/no-return| do-raise-unsupported-error who name primitive-realm "unsupported")]))

;; ----------------------------------------

(define-record-type (unquoted-printing-string new-unquoted-printing-string unquoted-printing-string?)
  (fields value))

(define make-unquoted-printing-string
  (|#%name|
   unquoted-printing-string
   (lambda (s)
     (check 'unquoted-printing-string string? s)
     (new-unquoted-printing-string s))))

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

(define exception-handler-key '#{exception-handler-key o9xm0uula3d2mbq9wueixh79r-0})

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
(define linklet-instantiate-key '#{linklet o9xm0uula3d2mbq9wueixh79r-1})

;; Limit on length of a context extracted from a continuation. This is
;; not a hard limit on the total length, because it only applied to an
;; individual frame in a metacontinuation, and it only applies to an
;; extension of a cached context. But it keeps from tunrning an
;; out-of-memory situation due to a deep continuation into one that
;; uses even more memory.
(define trace-length-limit 65535)

(define suppress-generation-in-trace (if (getenv "PLT_SHOW_BUILTIN_CONTEXT")
                                         255
                                         (collect-maximum-generation)))

;; Convert a continuation to a list of function-name and
;; source information. Cache the result half-way up the
;; traversal, so that it's amortized constant time.
(define-thread-local cached-traces (make-ephemeron-eq-hashtable))
(define (continuation->trace k)
  (let loop ([k k] [offset #f] [n 0] [accum '()] [accums '()] [slow-k k] [move? #f])
    (cond
      [(or (not (#%$continuation? k))
           (eq? k #%$null-continuation)
           (fx= n trace-length-limit))
       (finish-continuation-trace slow-k '() accum accums)]
      [(and (not offset)
            (hashtable-ref cached-traces k #f))
       => (lambda (l)
            (finish-continuation-trace slow-k l accum accums))]
      [else
       (let* ([name (or (and (not offset)
                             (let ([attachments (#%$continuation-attachments k)])
                               (and (pair? attachments)
                                    (not (eq? attachments (#%$continuation-attachments (#%$continuation-link k))))
                                    (let ([n (extract-mark-from-frame (car attachments) linklet-instantiate-key #f)])
                                      (and n
                                           (string->symbol (format "body of ~a" n)))))))
                        (let ([c (if offset
                                     (#%$continuation-stack-return-code k offset)
                                     (#%$continuation-return-code k))])
                          (and (not (fx> (#%$generation c) suppress-generation-in-trace))
                               (let ([n (#%$code-name c)])
                                 (if (path-or-empty-procedure-name-string? n)
                                     #f
                                     n)))))]
              [desc
               (let* ([ci (#%$code-info (if offset
                                            (#%$continuation-stack-return-code k offset)
                                            (#%$continuation-return-code k)))]
                      [src (and
                            (code-info? ci)
                            (or
                             ;; when per-expression inspector info is available:
                             (find-rpi (if offset
                                           (#%$continuation-stack-return-offset k offset)
                                           (#%$continuation-return-offset k))
                                       ci)
                             ;; when only per-function source location is available:
                             (code-info-src ci)))])
                 (and (or name src)
                      (cons name src)))])
         (let* ([offset (if offset
                            (fx- offset (#%$continuation-stack-return-frame-words k offset))
                            (fx- (#%$continuation-stack-clength k)
                                 (#%$continuation-return-frame-words k)))]
                [offset (if (fx= offset 0) #f offset)]
                [move? (and move? (not offset) (not (eq? k slow-k)))]
                [next-k (if offset k (#%$continuation-link k))]
                [accum (if desc (cons desc accum) accum)]
                [accums (if offset accums (cons (cons k accum) accums))]
                [accum (if offset accum '())])
           (loop next-k
                 offset
                 (fx+ n 1)
                 accum accums
                 (if move? (#%$continuation-link slow-k) slow-k) (not move?))))])))

;; `slow-k` is the place to cache, `l` is the tail of the result,
;; `accum` is a list in reverse order to add to `l`, and `accums`
;; is a list of `(cons k accum)` of `accum`s to add in reverse
;; order, caching the result so far if `k` is `slow-k`
(define (finish-continuation-trace slow-k l accum accums)
  (let ([reverse-onto
         (lambda (rev l)
           (let loop ([l l] [rev rev])
             (cond
               [(null? rev) l]
               [else (loop (cons (car rev) l)
                           (cdr rev))])))])
    (let loop ([l (reverse-onto accum l)] [accums accums])
      (cond
        [(null? accums) l]
        [else
         (let* ([a (car accums)]
                [l (reverse-onto (cdr a) l)])
           (when (eq? (car a) slow-k)
             (hashtable-set! cached-traces slow-k l))
           (loop l (cdr accums)))]))))

(define primitive-names #f)
(define (install-primitives-table! primitives)
  (set! primitive-names primitives))

;; Simplified variant of `continuation->trace` that can be called to
;; get a likely primitive to blame for a blocking future.
(define (continuation-current-primitive k exclusions)
  (let loop ([k (if (full-continuation? k) (full-continuation-k k) k)])
    (cond
     [(or (not (#%$continuation? k))
          (eq? k #%$null-continuation))
      #f]
     [else
      (let* ([name (or (let ([n #f])
                         (and n
                              (string->symbol (format "body of ~a" n))))
                       (let* ([c (#%$continuation-return-code k)]
                              [n (#%$code-name c)])
                         (and n (string->symbol n))))])
        (cond
         [(and name
               (hash-ref primitive-names name #f)
               (not (#%memq name exclusions)))
          name]
         [else
          (#%$split-continuation k 0)
          (loop (#%$continuation-link k))]))])))

(define (traces->context ls realms?)
  (let loop ([l '()] [ls ls])
    (cond
     [(null? l)
      (if (null? ls)
          '()
          (loop (car ls) (cdr ls)))]
     [else
      (let* ([p (car l)]
             [name (and (car p)
                        (let ([s (procedure-name-string->visible-name-string (car p))])
                          (if (string? s)
                              (string->symbol s)
                              s)))]
             [realm (or (and realms?
                             (procedure? (car p))
                             (procedure-realm (car p)))
                        default-realm)]
             [loc (and (cdr p)
                       (call-with-values (lambda ()
                                           (let* ([src (cdr p)]
                                                  [path (source-file-descriptor-path (source-object-sfd src))]
                                                  [path (if (srcloc? path)
                                                            ;; The linklet layer wraps paths in `srcloc` to trigger specific
                                                            ;; marshaling behavior
                                                            (srcloc-source path)
                                                            path)])
                                             (if (source-object-line src)
                                                 (values path
                                                         (source-object-line src)
                                                         (source-object-column src)
                                                         (source-object-bfp src)
                                                         (source-object-efp src))
                                                 (values path
                                                         (source-object-bfp src)
                                                         (source-object-efp src)))))
                         (case-lambda
                          [() #f]
                          [(path line col pos end) (|#%app| srcloc path line (sub1 col) (add1 pos) (- end pos))]
                          [(path pos end) (|#%app| srcloc path #f #f (add1 pos) (- end pos))])))])
        (if (or name loc)
            (cons (if realms?
                      (vector->immutable-vector (vector name loc realm))
                      (cons name loc))
                  (loop (cdr l) ls))
            (loop (cdr l) ls)))])))

(define (default-error-display-handler msg v)
  (eprintf "~a" msg)
  (when (or (continuation-condition? v)
            (and (exn? v)
                 (not (exn:fail:user? v))))
    (let* ([n (|#%app| error-print-context-length)]
           [locs (if (and (exn:srclocs? v)
                          (not (zero? n)))
                     ((exn:srclocs-accessor* v) v)
                     '())]
           [l (if (zero? n)
                  '()
                  (traces->context
                   (if (exn? v)
                       (continuation-mark-set-traces (exn-continuation-marks v))
                       (list (continuation->trace (condition-continuation v))))
                   #f))])
      (unless (null? locs)
        (unless (and (list? locs)
                     (andmap srcloc? locs))
          (raise-result-error '|prop:exn:srclocs procedure| "(listof srcloc?)" locs))
        (let* ([locs
                ;; Some exns are expected to include srcloc in the msg,
                ;; so skip the first srcloc of those
                (if (and (or (exn:fail:read? v)
                             (exn:fail:contract:variable? v))
                         (error-print-source-location))
                    (cdr locs)
                    locs)]
               [loc-strs (let loop ([locs locs])
                           (cond
                             [(null? locs) '()]
                             [else
                              (let ([str (srcloc->string (car locs))])
                                (if str
                                    (cons str (loop (cdr locs)))
                                    (loop (cdr locs))))]))])
          (unless (null? loc-strs)
            (eprintf "\n  location...:")
            (#%for-each (lambda (str)
                          (eprintf (string-append "\n   " str)))
                        loc-strs))))
      (unless (null? l)
        (eprintf "\n  context...:")
        (let loop ([l l]
                   [prev #f]
                   [repeats 0]
                   [n n])
          (when (and (pair? l) (zero? n))
            (eprintf "\n   ..."))
          (unless (or (null? l) (zero? n))
            (let* ([p (car l)]
                   [s (cdr p)])
              (cond
               [(equal? p prev)
                (loop (cdr l) prev (add1 repeats) n)]
               [(positive? repeats)
                (eprintf "\n   [repeats ~a more time~a]" repeats (if (= repeats 1) "" "s"))
                (loop l #f 0 (sub1 n))]
               [else
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
                  (eprintf "\n   ~a" (car p))])
                (loop (cdr l) p 0 (sub1 n))])))))))
  (eprintf "\n"))

(define eprintf
  (lambda (fmt . args)
    (apply fprintf (current-error-port) fmt args)))

(define srcloc->string
  (lambda (srcloc)
    (#%format "~s" srcloc)))

(define error-print-source-location
  (lambda () #f))

(define (emergency-error-display-handler msg v)
  (log-system-message 'error msg))

(define (set-error-display-eprintf! proc
                                    srcloc->string-proc
                                    error-print-source-location-proc)
  (set! eprintf proc)
  (set! srcloc->string srcloc->string-proc)
  (set! error-print-source-location error-print-source-location-proc))

(define (default-error-escape-handler)
  (abort-current-continuation (default-continuation-prompt-tag) void))

(define (who->symbol who)
  (cond
   [(symbol? who) who]
   [(string? who) (string->symbol who)]
   [else 'unknown-who]))

(define (exn->string v)
  (error-message->adjusted-string
   (and (who-condition? v)
        (rewrite-who (who->symbol (condition-who v))))
   primitive-realm
   (cond
     [(exn? v)
      (exn-message v)]
     [(format-condition? v)
      (let-values ([(fmt irritants)
                    (rewrite-format (and (who-condition? v)
                                         (who->symbol (condition-who v)))
                                    (condition-message v)
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
     [else (format "~s" v)])
   primitive-realm))

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
         [realm (procedure-realm proc)]
         [make-str (arity-string-maker proc)]
         [arity (procedure-arity proc)]
         [adjust-for-method (lambda (n)
                              (if (and (procedure-is-method? proc)
                                       (positive? n))
                                  (sub1 n)
                                  n))])
    (|#%app|
     exn:fail:contract:arity
     (error-message->adjusted-string
      name realm
      (string-append
       "arity mismatch;\n the expected number of arguments does not match the given number"
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
      primitive-realm)
     (current-continuation-marks))))

(define/who uncaught-exception-handler
  (make-parameter default-uncaught-exception-handler
                  (lambda (v)
                    (check who (procedure-arity-includes/c 1) v)
                    v)
                  'uncaught-exception-handler))

(define/who error-display-handler
  (make-parameter default-error-display-handler
                  (lambda (v)
                    (check who (procedure-arity-includes/c 2) v)
                    v)
                  'error-display-handler))

(define/who error-escape-handler
  (make-parameter default-error-escape-handler
                  (lambda (v)
                    (check who (procedure-arity-includes/c 0) v)
                    v)
                  'error-escape-handler))

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
      [else (do-raise v)]))))

(define (make-nested-exception-handler what old-exn)
  (lambda (exn)
    (let ([msg
           (error-message->adjusted-string
            #f primitive-realm
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
                 (error-value->string old-exn)))
            primitive-realm)])
      (default-uncaught-exception-handler
        (|#%app| exn:fail msg (current-continuation-marks))))))

(define (call-with-exception-handler proc thunk)
  (with-continuation-mark exception-handler-key proc (thunk)))

;; ----------------------------------------

(define log-system-message void)

(define (set-log-system-message! proc)
  (set! log-system-message proc))
