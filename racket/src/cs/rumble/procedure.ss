(define-values (prop:method-arity-error method-arity-error? method-arity-error-ref)
  (make-struct-type-property 'method-arity-error))

(define-values (prop:arity-string arity-string? arity-string-ref)
  (make-struct-type-property 'arity-string (lambda (v info)
                                             (check 'guard-for-prop:arity-string
                                                    (procedure-arity-includes/c 1)
                                                    v)
                                             v)))

(define-values (prop:procedure procedure-struct? procedure-struct-ref)
  (make-struct-type-property 'procedure (lambda (v info)
                                          ;; We don't have to check whether `v` is valid here,
                                          ;; because `make-struct-type` handles `prop:procedure`
                                          ;; directly; we just convert a relative position to
                                          ;; an absolute one
                                          (if (exact-integer? v)
                                              (+ v (let ([p (list-ref info 6)])
                                                     (if p
                                                         (struct-type-total*-field-count p)
                                                         0)))
                                              v))))

(define-values (prop:incomplete-arity incomplete-arity? incomplete-arity-ref)
  (make-struct-type-property 'incomplete-arity))

;; Integer value is a field position to access a mask
(define-values (prop:procedure-arity procedure-arity-prop? procedure-arity-ref)
  (make-struct-type-property 'procedure-arity))

(define (procedure? v)
  (or (#%procedure? v)
      (and (record? v)
           (not (eq? (struct-property-ref prop:procedure (record-rtd v) none) none)))))

(define/who (procedure-specialize proc)
  (check who procedure? proc)
  proc)

(define apply
  (case-lambda
    [(proc args)
     (if (#%procedure? proc)
         (#2%apply proc args)
         (#2%apply (extract-procedure proc (length args)) args))]
    [(proc)
     (raise-arity-error 'apply (|#%app| arity-at-least 2) proc)]
    [(proc . argss)
     (if (#%procedure? proc)
         (#2%apply #2%apply proc argss)
         (let ([len (let loop ([argss argss])
                      (cond
                       [(null? (cdr argss)) (length (car argss))]
                       [else (fx+ 1 (loop (cdr argss)))]))])
           (#2%apply #2%apply (extract-procedure proc len) argss)))]))

;; See copy in "expander.sls"
(define-syntax (|#%app| stx)
  (syntax-case stx ()
    [(_ rator rand ...)
     (with-syntax ([n-args (length #'(rand ...))])
       #'((extract-procedure rator n-args) rand ...))]))

(define |#%call-with-values|
  (|#%name|
   call-with-values
   (lambda (generator receiver)
     (call-with-values (if (#%procedure? generator)
                           generator
                           (lambda () (|#%app| generator)))
       (if (#%procedure? receiver)
           receiver
           (lambda args (apply receiver args)))))))

(define (extract-procedure f n-args)
  (cond
   [(#%procedure? f) f]
   [else (slow-extract-procedure f n-args)]))

(define (slow-extract-procedure f n-args)
  (pariah ; => don't inline enclosing procedure
   (do-extract-procedure f f n-args #f not-a-procedure)))

;; Returns a host-Scheme procedure, but first checks arity so that
;; checking and reporting use the right top-level function, and
;; the returned procedure may just report a not-a-procedure error
(define (do-extract-procedure f orig-f n-args success-k fail-k)
  (cond
   [(#%procedure? f)
    (if (chez:procedure-arity-includes? f n-args)
        (if success-k
            (success-k f)
            f)
        (wrong-arity-wrapper orig-f))]
   [(record? f)
    (let* ([rtd (record-rtd f)]
           [v (struct-property-ref prop:procedure rtd none)])
      (cond
       [(eq? v none) (fail-k orig-f)]
       [(fixnum? v)
        (let ([a (struct-property-ref prop:procedure-arity rtd #f)])
          (cond
           [(and a (not (bitwise-bit-set? (unsafe-struct*-ref f a) n-args)))
            (wrong-arity-wrapper orig-f)]
           [else
            (do-extract-procedure (unsafe-struct-ref f v) orig-f n-args success-k wrong-arity-wrapper)]))]
       [(eq? v 'unsafe)
        (do-extract-procedure
         (if (chaperone? f)
             (unsafe-procedure-chaperone-replace-proc f)
             (unsafe-procedure-impersonator-replace-proc f))
         orig-f
         n-args
         success-k
         wrong-arity-wrapper)]
       [else
        (let ([a (struct-property-ref prop:procedure-arity rtd #f)])
          (cond
           [(and a (not (bitwise-bit-set? (unsafe-struct*-ref f a) n-args)))
            (wrong-arity-wrapper orig-f)]
           [else
            (do-extract-procedure
             v
             orig-f
             (fx+ n-args 1)
             (lambda (v)
               (cond
                [(not v) (case-lambda)]
                [else
                 (case-lambda
                  [() (v f)]
                  [(a) (v f a)]
                  [(a b) (v f a b)]
                  [(a b c) (v f a b c)]
                  [args (chez:apply v f args)])]))
             wrong-arity-wrapper)]))]))]
   [else (fail-k orig-f)]))

(define (extract-procedure-name f)
  (cond
   [(and (reduced-arity-procedure? f)
         (reduced-arity-procedure-name f))
    => (lambda (name) name)]
   [(record? f)
    (let* ([v (struct-property-ref prop:procedure (record-rtd f) #f)])
      (cond
       [(fixnum? v)
        (let ([v (unsafe-struct-ref f v)])
          (cond
           [(procedure? v) (object-name v)]
           [else (struct-object-name f)]))]
       [(eq? v 'unsafe)
        (extract-procedure-name
         (if (chaperone? f)
             (unsafe-procedure-chaperone-replace-proc f)
             (unsafe-procedure-impersonator-replace-proc f)))]
       [else (struct-object-name f)]))]
   [else #f]))

(define/who procedure-arity-includes?
  (case-lambda
   [(f n incomplete-ok?)
    (let ([mask (get-procedure-arity-mask who f incomplete-ok?)])
      (check who exact-nonnegative-integer? n)
      (bitwise-bit-set? mask n))]
   [(f n) (procedure-arity-includes? f n #f)]))

(define (chez:procedure-arity-includes? proc n)
  (bitwise-bit-set? (#%procedure-arity-mask proc) n))

(define (procedure-arity orig-f)
  (mask->arity (get-procedure-arity-mask 'procedure-arity orig-f #t)))

(define/who (procedure-arity-mask orig-f)
  (get-procedure-arity-mask who orig-f #t))

(define (get-procedure-arity-mask who orig-f incomplete-ok?)
  (cond
   [(#%procedure? orig-f)
    (#%procedure-arity-mask orig-f)]
   [else
    (let proc-arity-mask ([f orig-f] [shift 0])
      (cond
       [(#%procedure? f)
        (bitwise-arithmetic-shift-right (#%procedure-arity-mask f) shift)]
       [(record? f)
        (let ([rtd (record-rtd f)])
          (cond
           [(and (not incomplete-ok?)
                 (struct-property-ref prop:incomplete-arity rtd #f))
            0]
           [else
            (let* ([a (struct-property-ref prop:procedure-arity rtd #f)])
              (cond
               [a (bitwise-arithmetic-shift-right (unsafe-struct*-ref f a) shift)]
               [else
                (let ([v (struct-property-ref prop:procedure rtd #f)])
                  (cond
                   [(fixnum? v)
                    (proc-arity-mask (unsafe-struct-ref f v) shift)]
                   [(eq? v 'unsafe)
                    (proc-arity-mask (impersonator-next f) shift)]
                   [else
                    (proc-arity-mask v (add1 shift))]))]))]))]
       [(eq? f orig-f)
        (raise-argument-error who "procedure?" orig-f)]
       [else 0]))]))

(define (procedure-incomplete-arity? f)
  (cond
   [(#%procedure? f) #f]
   [(record? f)
    (let ([rtd (record-rtd f)])
      (cond
       [(struct-property-ref prop:incomplete-arity rtd #f)
        #t]
       [(struct-property-ref prop:procedure-arity rtd #f)
        ;; Anything with `prop:procedure-arity` has to have `prop:incomplete-arity`
        ;; if the procedure's arity is not complete
        #f]
       [else
        (let ([v (struct-property-ref prop:procedure rtd #f)])
          (cond
           [(fixnum? v)
            (procedure-incomplete-arity? (unsafe-struct-ref f v))]
           [(eq? v 'unsafe)
            (procedure-incomplete-arity?
             (if (chaperone? f)
                 (unsafe-procedure-chaperone-replace-proc f)
                 (unsafe-procedure-impersonator-replace-proc f)))]
           [else
            (procedure-incomplete-arity? v)]))]))]
   [else #f]))

;; Public, limited variant:
(define/who (procedure-extract-target f)
  (cond
   [(record? f)
    (let* ([rtd (record-rtd f)]
           [v (struct-property-ref prop:procedure rtd #f)])
      (cond
       [(fixnum? v)
        (let ([v (unsafe-struct-ref f v)])
          (and (#%procedure? v) v))]
       [else
        (check who procedure? f)
        #f]))]
   [else
    (check who procedure? f)
    #f]))

(define (not-a-procedure f)
  (raise-arguments-error 'application
                         "not a procedure;\n expected a procedure that can be applied to arguments"
                         "given" f))

(define (wrong-arity-wrapper f)
  (lambda args
    (cond
     [(arity-string-maker f)
      => (lambda (make-str)
           (let ([expected (make-str)])
             (do-raise-arity-error
              f
              (if (string? expected)
                  (string-append "  expected: " expected "\n")
                  "")
              (if (and (pair? args)
                       (procedure-is-method? f))
                  (cdr args)
                  args))))]
     [(procedure-is-method? f)
      (chez:apply raise-arity-error
                  f
                  (let ([m (procedure-arity-mask f)])
                    (if (not (bitwise-bit-set? m 0))
                        (mask->arity (bitwise-arithmetic-shift-right m 1))
                        (mask->arity m)))
                  (if (null? args) '() (cdr args)))]
     [else
      (chez:apply raise-arity-error f (procedure-arity f) args)])))

(define/who (procedure-result-arity p)
  (check who procedure? p)
  (cond
   [(#%procedure? p)
    (and (procedure-known-single-valued? p)
         1)]
   [(impersonator? p)
    (procedure-result-arity (strip-impersonator p))]
   [(record? p)
    (let* ([rtd (record-rtd p)]
           [v (struct-property-ref prop:procedure rtd none)])
      (cond
       [(eq? v none) #f]
       [(fixnum? v)
        (procedure-result-arity (unsafe-struct-ref p v))]
       [(eq? v 'unsafe)
        (procedure-result-arity
         (if (chaperone? p)
             (unsafe-procedure-chaperone-replace-proc p)
             (unsafe-procedure-impersonator-replace-proc p)))]
       [else (procedure-result-arity v)]))]
   [else #f]))

;; ----------------------------------------

;; Host Scheme wrapper procedures data content:
;;
;;  - (vector <symbol-or-#f> <proc>) => not a method, and name is either
;;                                      <symbol-or-#f> or name of <proc>
;;  - (vector <symbol-or-#f> <proc> 'method) => is a method
;;  - (box <symbol>) => JIT function generated, name is <symbol>, not a method

;; ----------------------------------------

(define-record method-procedure (proc))

(define (method-wrapper-vector? vec)
  (fx= 3 (#%vector-length vec)))

(define/who (procedure->method proc)
  (check who procedure? proc)
  (cond
   [(procedure-is-method? proc)
    proc]
   [(#%procedure? proc)
    ;; preserve primitiveness of the procedure
    (let ([v (and (wrapper-procedure? proc)
                  (let ([v (wrapper-procedure-data proc)])
                    (and (#%vector? v)
                         v)))])
      (if v
          (make-arity-wrapper-procedure (#%vector-ref v 1)
                                        (procedure-arity-mask proc)
                                        (vector (#%vector-ref v 0)
                                                (#%vector-ref v 1)
                                                'method))
          (make-arity-wrapper-procedure proc
                                        (procedure-arity-mask proc)
                                        (vector #f proc 'method))))]
   [else
    (make-method-procedure proc)]))

(define (procedure-is-method? f)
  (cond
   [(#%procedure? f)
    (if (wrapper-procedure? f)
        (let ([name (wrapper-procedure-data f)])
          (and (#%vector? name)
               (method-wrapper-vector? name)))
        #f)]
   [(record? f)
    (or (method-arity-error? f)
        (let ([v (struct-property-ref prop:procedure (record-rtd f) #f)])
          (cond
           [(fixnum? v)
            (procedure-is-method? (unsafe-struct-ref f v))]
           [(eq? v 'unsafe)
            (procedure-is-method? (impersonator-val f))]
           [(impersonator? f)
            ;; follow wrapped procedure instead of impersonator method:
            (procedure-is-method? (strip-impersonator f))]
           [else (procedure-is-method? v)])))]
   [else #f]))

(define-syntax-rule (|#%method-arity| e)
  (procedure->method e))

;; ----------------------------------------

(define (arity-string-maker f)
  (cond
   [(record? f)
    (cond
     [(arity-string-ref f #f)
      => (lambda (make-str)
           (lambda () (make-str f)))]
     [(method-arity-error? f) #f]
     [(reduced-arity-procedure? f) #f]
     [else
      (let ([v (struct-property-ref prop:procedure (record-rtd f) #f)])
        (cond
         [(fixnum? v)
          (arity-string-maker (unsafe-struct-ref f v))]
         [(eq? v 'unsafe)
          (arity-string-maker (impersonator-val f))]
         [else #f]))])]
   [else #f]))

;; ----------------------------------------

(define-record reduced-arity-procedure (proc mask name))

(define/who procedure-reduce-arity
  (case-lambda
    [(proc a name)
     (check who procedure? proc)
     (let ([mask (arity->mask a)])
       (unless mask
         (raise-arguments-error who "procedure-arity?" a))
       (check who symbol? :or-false name)
       (unless (= mask (bitwise-and mask (procedure-arity-mask proc)))
         (raise-arguments-error who
                                "arity of procedure does not include requested arity"
                                "procedure" proc
                                "requested arity" a))
       (do-procedure-reduce-arity-mask proc mask name))]
    [(proc a) (procedure-reduce-arity proc a #f)]))

(define/who procedure-reduce-arity-mask
  (case-lambda
    [(proc mask name)
     (check who procedure? proc)
     (check who exact-integer? mask)
     (check who symbol? :or-false name)
     (unless (= mask (bitwise-and mask (procedure-arity-mask proc)))
       (raise-arguments-error who
                              "arity mask of procedure does not include requested arity mask"
                              "procedure" proc
                              "requested arity mask" mask))
     (do-procedure-reduce-arity-mask proc mask name)]
    [(proc mask) (procedure-reduce-arity-mask proc mask #f)]))

(define (do-procedure-reduce-arity-mask proc mask name)
  (cond
   [(and (wrapper-procedure? proc)
         (#%vector? (wrapper-procedure-data proc)))
    (let ([v (wrapper-procedure-data proc)])
      (make-arity-wrapper-procedure (#%vector-ref v 1)
                                    mask
                                    (cond
                                     [(method-wrapper-vector? v)
                                      (vector (or name (#%vector-ref v 0))
                                              (#%vector-ref v 1)
                                              'method)]
                                     [name (vector name
                                                   (#%vector-ref v 1))]
                                     [else v])))]
   [(#%procedure? proc)
    (make-arity-wrapper-procedure proc
                                  mask
                                  (vector name proc))]
   [(reduced-arity-procedure? proc)
    (do-procedure-reduce-arity-mask (reduced-arity-procedure-proc proc)
                                    mask
                                    (or name (reduced-arity-procedure-name proc)))]
   [else
    (make-reduced-arity-procedure proc
                                  mask
                                  name)]))

;; ----------------------------------------

(define-record named-procedure (proc name))

(define/who (procedure-rename proc name)
  (cond
   [(#%procedure? proc)
    ;; Potentially avoid an extra wrapper layer, and also work before
    ;; `procedure?` is fully filled in
    (check who symbol? name)
    (do-procedure-reduce-arity-mask proc (#%procedure-arity-mask proc) name)]
   [(reduced-arity-procedure? proc)
    ;; Avoid an extra wrapper layer, and also work before
    ;; `procedure?` is fully filled in
    (check who symbol? name)
    (make-reduced-arity-procedure (reduced-arity-procedure-proc proc)
                                  (reduced-arity-procedure-mask proc)
                                  name)]
   [else
    (check who procedure? proc)
    (check who symbol? name)
    (make-named-procedure proc name)]))

(define (procedure-maybe-rename proc name)
  (if name
      (procedure-rename proc name)
      proc))

;; ----------------------------------------

(define (make-jit-procedure force mask name)
  (letrec ([p (make-wrapper-procedure
               (lambda args
                 (let ([f (force)])
                   (with-interrupts-disabled
                    ;; atomic with respect to Racket threads,
                    (let ([name (wrapper-procedure-data p)])
                      (unless (#%box? name)
                        (set-wrapper-procedure! p f)
                        (set-wrapper-procedure-data! p (box name)))))
                   (apply p args)))
               mask
               name)])
    p))

(define (extract-wrapper-procedure-name p)
  (let ([name (wrapper-procedure-data p)])
    (cond
     [(#%box? name) (#%unbox name)]
     [(#%vector? name) (or (#%vector-ref name 0)
                           (object-name (#%vector-ref name 1)))]
     [else name])))

;; ----------------------------------------

(define-record procedure-impersonator impersonator (wrapper arity-mask))
(define-record procedure-chaperone chaperone (wrapper arity-mask))

(define-record procedure*-impersonator procedure-impersonator ())
(define-record procedure*-chaperone procedure-chaperone ())

(define-values (impersonator-prop:application-mark application-mark? application-mark-ref)
  (make-impersonator-property 'application-mark))

(define/who (impersonate-procedure proc wrapper . props)
  (do-impersonate-procedure who make-procedure-impersonator proc wrapper
                            make-props-procedure-impersonator props
                            values ""))

(define/who (chaperone-procedure proc wrapper . props)
  (do-impersonate-procedure who make-procedure-chaperone proc wrapper
                            make-props-procedure-chaperone props
                            values ""))

(define/who (impersonate-procedure* proc wrapper . props)
  (do-impersonate-procedure who make-procedure*-impersonator proc wrapper
                            make-props-procedure-impersonator props
                            (lambda (n) (bitwise-arithmetic-shift-right n 1)) " (adding an extra argument)"))

(define/who (chaperone-procedure* proc wrapper . props)
  (do-impersonate-procedure who make-procedure*-chaperone proc wrapper
                            make-props-procedure-chaperone props
                            (lambda (n) (bitwise-arithmetic-shift-right n 1)) " (adding an extra argument)"))

(define (do-impersonate-procedure who make-procedure-impersonator proc wrapper
                                  make-props-procedure-impersonator props-l
                                  arity-shift arity-shift-str)
  (check who procedure? proc)
  (let ([m (procedure-arity-mask proc)])
    (when wrapper
      (check who procedure? :or-false wrapper)
      (unless (= m (bitwise-and m (arity-shift (procedure-arity-mask wrapper))))
        (raise-arguments-error who
                               (string-append
                                "arity of wrapper procedure does not cover arity of original procedure"
                                arity-shift-str)
                               "wrapper" wrapper
                               "original" proc)))
    (let ([val (if (impersonator? proc)
                   (impersonator-val proc)
                   proc)]
          [props (add-impersonator-properties who
                                              props-l
                                              (if (impersonator? proc)
                                                  (intmap-remove (impersonator-props proc) impersonator-prop:application-mark)
                                                  empty-hasheq))])
      (cond
       [wrapper (make-procedure-impersonator val proc props wrapper m)]
       [(null? props-l) proc]
       [else
        (make-props-procedure-impersonator val proc props m)]))))

(define (procedure-impersonator*? v)
  (or (procedure*-impersonator? v)
      (procedure*-chaperone? v)
      (and (impersonator? v)
           (procedure-impersonator*? (impersonator-next v)))))

(define (call-with-application-mark props k)
  (let ([mark (intmap-ref props impersonator-prop:application-mark #f)])
    (cond
     [(pair? mark)
      (call-with-immediate-continuation-mark
       (car mark)
       (lambda (v)
         (if (eq? v none)
             (k mark #f #f)
             (k mark #t v)))
       none)]
     [else
      (k #f #f #f)])))

(define (impersonate-apply proc . args)
  (impersonate-apply/parameter proc #t args))

;; If `actually-call?` is #f, then don't call the procedure in `proc`,
;; because we're trying to get an inpersonated-parameter value
(define (impersonate-apply/parameter proc actually-call? args)
  (let ([n (length args)])
    (cond
     [(not (procedure-arity-includes? (impersonator-val proc) n))
      ;; Let primitive application complain:
      (|#%app| (impersonator-val proc) args)]
     [else
      ;; Loop through wrappers so that `{chaperone,impersonate}-procedure*`
      ;; wrappers can receive the original `proc` argument
      (let loop ([p proc] [args args])
        (cond
         [(or (procedure-impersonator? p)
              (procedure-chaperone? p))
          ;; Check for `impersonator-prop:application-mark`, since we'll need
          ;; to grab any immediately available mark in that case
          (call-with-application-mark
           (impersonator-props p)
           ;; The `mark-pair` argument is the `impersonator-prop:application-mark` value,
           ;; and `has-current-mark?` indincates whether `current-mark-val` is the value
           ;; of that mark on the current continuation frame
           (lambda (mark-pair has-current-mark? current-mark-val)
             (let* ([chaperone? (procedure-chaperone? p)]
                    [wrapper (if chaperone?
                                 (procedure-chaperone-wrapper p)
                                 (procedure-impersonator-wrapper p))]
                    [next-p (impersonator-next p)]
                    [new-args
                     ;; Call the wrapper procedure, propagating the current value
                     ;; (if any) of the `impersonator-prop:application-mark`-specified mark
                     (call-with-values
                         (lambda ()
                           (let ([call
                                  (lambda ()
                                    ;; Calling convention is different for `procedure*`
                                    ;; and non-`procedure*` variants:
                                    (if (if chaperone?
                                            (procedure*-chaperone? p)
                                            (procedure*-impersonator? p))
                                        (apply wrapper proc args)
                                        (apply wrapper args)))])
                             ;; Set mark, if any, while calling:
                             (cond
                              [has-current-mark?
                               (with-continuation-mark (car mark-pair) current-mark-val (call))]
                              [else (call)])))
                       list)]
                    [nn (length new-args)]
                    [check
                     (lambda (who args new-args)
                       (when chaperone?
                         (for-each (lambda (e e2)
                                     (unless (chaperone-of? e2 e)
                                       (raise-chaperone-error who "argument" e e2)))
                                   args
                                   new-args)))]
                    [continue
                     ;; To continue iterating through wrappers:
                     (lambda (new-args)
                       (if mark-pair
                           (with-continuation-mark (car mark-pair) (cdr mark-pair)
                             (loop next-p new-args))
                           (loop next-p new-args)))])
               ;; Loop to check for extra post proc or `'mark <key> <val>`
               (let loop ([nn nn] [new-args new-args] [post-proc #f] [pos 0])
                 (cond
                  [(fx= n nn)
                   ;; No more extra results, so `new-args` should match up with `args`:
                   (check '|procedure chaperone| args new-args)
                   (cond
                    [post-proc
                     (call-with-values
                         (lambda () (continue new-args))
                       (lambda results
                         (let* ([post-results (if actually-call? results (list (void)))]
                                [new-results (call-with-values (lambda () (apply post-proc post-results)) list)])
                           (unless (= (length post-results) (length new-results))
                             (raise-result-wrapper-result-arity-error))
                           (check '|procedure-result chaperone| post-results new-results)
                           (if actually-call?
                               (#%apply values new-results)
                               (#%apply values results)))))]
                    [else
                     (continue new-args)])]
                  [(and (fx> nn n)
                        (not post-proc)
                        (procedure? (car new-args)))
                   ;; Extra procedure result => wrapper to apply to function results
                   (loop (fx1- nn) (cdr new-args) (car new-args) (fx1+ pos))]
                  [(and (fx> nn n)
                        (eq? 'mark (car new-args)))
                   ;; 'mark => wrap call with a continuation mark
                   (unless (fx>= (fx- nn 3) n)
                     (raise-mark-missing-key-or-val-error chaperone? pos next-p wrapper))
                   (with-continuation-mark (cadr new-args) (caddr new-args)
                     (loop (fx- nn 3) (cdddr new-args) post-proc (fx+ pos 3)))]
                  [(fx> nn n)
                   (raise-wrapper-bad-extra-result-error chaperone? pos (car new-args) next-p wrapper)]
                  [else
                   (raise-wrapper-result-arity-error chaperone? proc wrapper n nn)])))))]
         [(unsafe-procedure-impersonator? p)
          (apply p args)]
         [(unsafe-procedure-chaperone? p)
          (apply p args)]
         [(impersonator? p)
          (loop (impersonator-next p) args)]
         [(not actually-call?)
          (apply values args)]
         [else
          ;; If `p` is a structure whose `prop:procedure` value is an
          ;; integer `i`, then we should extract the field at position
          ;; `i` from `proc`, not from `p`, so that any interpositions
          ;; on that access are performed.
          (let ([v (and (record? p)
                        (struct-property-ref prop:procedure (record-rtd p) #f))])
            (cond
             [(integer? v)
              (apply (unsafe-struct-ref proc v) args)]
             [else
              (apply p args)]))]))])))

(define (set-procedure-impersonator-hash!)
  (record-type-hash-procedure (record-type-descriptor procedure-chaperone)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c))))
  (record-type-hash-procedure (record-type-descriptor procedure-impersonator)
                              (lambda (i hash-code)
                                (hash-code (impersonator-next i)))))

(define (raise-result-wrapper-result-arity-error)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (string-append "procedure-result chaperone: result arity mismatch;\n"
                   " expected number of values not received from wrapper on the original procedure's result")
    (current-continuation-marks))))

(define (raise-mark-missing-key-or-val-error chaperone? pos next-p wrapper)
  (raise-arguments-error (if chaperone?
                             '|procedure chaperone|
                             '|procedure impersonator|)
                         (string-append
                          "wrapper's " (nth-str pos) " result needs addition extra results;\n"
                          "  " (nth-str pos) " extra result (before original argument count) needs an additional\n"
                          "  two results after 'mark")
                         "original" next-p
                         "wrapper" wrapper))

(define (raise-wrapper-bad-extra-result-error chaperone? pos v next-p wrapper)
  (raise-arguments-error (if chaperone?
                             '|procedure chaperone|
                             '|procedure impersonator|)
                         (string-append
                          "wrapper's " (nth-str pos) " result is not valid;\n"
                          " " (nth-str pos) " extra result (before original argument count) should be\n"
                          " 'mark" (if (zero? pos)
                                       " or a wrapper for the original procedure's result"
                                       ""))
                         "original" next-p
                         "wrapper" wrapper
                         "received" v))

(define (raise-wrapper-result-arity-error chaperone? proc wrapper expected-n got-n)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (string-append
     (if chaperone?
         "procedure chaperone"
         "procedure impersonator")
     ": arity mismatch;\n"
     " expected number of results not received from wrapper on the original\n"
     " procedure's arguments\n"
     "  original: " (error-value->string proc)
     "\n"
     "  wrapper: " (error-value->string wrapper)
     "\n"
     "  expected: " (number->string expected-n) " or more\n"
     "  received: " (number->string got-n))
    (current-continuation-marks))))

;; ----------------------------------------

(define-record unsafe-procedure-impersonator impersonator (replace-proc))
(define-record unsafe-procedure-chaperone chaperone (replace-proc))

(define/who (unsafe-impersonate-procedure proc replace-proc . props)
  (do-unsafe-impersonate-procedure who make-unsafe-procedure-impersonator
                                   proc replace-proc props))

(define/who (unsafe-chaperone-procedure proc replace-proc . props)
  (do-unsafe-impersonate-procedure who make-unsafe-procedure-chaperone
                                   proc replace-proc props))

(define (do-unsafe-impersonate-procedure who make-unsafe-procedure-impersonator proc replace-proc props-l)
  (let ([m (procedure-arity-mask proc)])
    (unless (= m (bitwise-and m (procedure-arity-mask replace-proc)))
      (raise-arguments-error who
                             "arity of replacement procedure does not cover arity of original procedure"
                             "replacement" replace-proc
                             "original" proc))
    (make-unsafe-procedure-impersonator
     (strip-impersonator proc)
     proc
     (add-impersonator-properties who
                                  props-l
                                  (if (impersonator? proc)
                                      (intmap-remove (impersonator-props proc) impersonator-prop:application-mark)
                                      empty-hasheq))
     replace-proc)))

;; ----------------------------------------

(define/who (procedure-closure-contents-eq? p1 p2)
  (check who procedure? p1)
  (check who procedure? p2)
  (when (and (#%procedure? p1)
             (#%procedure? p2))
    (let* ([i1 (inspect/object p1)]
           [i2 (inspect/object p2)]
           [l1 (i2 'length)]
           [l2 (i2 'length)])
      (and (eq? ((i1 'code) 'value)
                ((i2 'code) 'value))
           (= l1 l2)
           (let loop ([i 0])
             (or (fx= i l1)
                 (and (eq? (((i1 'ref i) 'ref) 'value) (((i2 'ref i) 'ref) 'value))
                      (loop (fx1+ i)))))))))

;; ----------------------------------------

(define-values (prop:checked-procedure checked-procedure? checked-procedure-ref)
  (make-struct-type-property 'checked-procedure
                             (lambda (v s)
                               (unless (not (list-ref s 6))
                                 (raise-arguments-error 'prop:checked-procedure
                                                        "not allowed on a structure type with a supertype"))
                               (unless (>= (+ (list-ref s 1) (list-ref s 2)) 2)
                                 (raise-arguments-error 'prop:checked-procedure
                                                        "need at least two fields in the structure type"))
                               #t)))

(define/who (checked-procedure-check-and-extract st v alt-proc v1 v2)
  (check who record-type-descriptor?
         :contract "(and/c struct-type? (not/c impersonator?))"
         st)
  (if (and (checked-procedure? v)
           (record? v st)
           (|#%app| (unsafe-struct*-ref v 0) v1 v2))
      (unsafe-struct*-ref v 1)
      (|#%app| alt-proc v v1 v2)))

;; ----------------------------------------

(define (primitive? v)
  (or (eq? v make-struct-type-property)
      (eq? v make-struct-type)
      (eq? v car)))

(define (primitive-closure? v) #f)

(define (primitive-result-arity prim)
  (cond
   [(eq? prim make-struct-type-property) 3]
   [(eq? prim make-struct-type) 5]
   [(eq? prim car) 1]
   [else
    (raise-argument-error 'primitive-result-arity "primitive?" prim)]))

;; ----------------------------------------

;; Used to encode an 'inferred-name property as a Scheme expression
(define-syntax (|#%name| stx)
  (syntax-case stx ()
    [(_ name val) #`(let ([name val]) name)]))

;; ----------------------------------------

(define (set-primitive-applicables!)
  (struct-property-set! prop:procedure
                        (record-type-descriptor parameter)
                        0)
  (struct-property-set! prop:procedure
                        (record-type-descriptor derived-parameter)
                        0)

  (struct-property-set! prop:procedure
                        (record-type-descriptor position-based-accessor)
                        (lambda (pba s p)
                          (cond
                           [(and (record? s (position-based-accessor-rtd pba))
                                 (fixnum? p)
                                 (fx>= p 0)
                                 (fx< p (position-based-accessor-field-count pba)))
                            (unsafe-struct*-ref s (+ p (position-based-accessor-offset pba)))]
                           [(and (impersonator? s)
                                 (record? (impersonator-val s) (position-based-accessor-rtd pba))
                                 (fixnum? p)
                                 (fx>= p 0)
                                 (fx< p (position-based-accessor-field-count pba)))
                            (impersonate-ref (lambda (s)
                                               (unsafe-struct*-ref s (+ p (position-based-accessor-offset pba))))
                                             (position-based-accessor-rtd pba)
                                             p
                                             s)]
                           [else (error 'struct-ref "bad access")])))

  (struct-property-set! prop:procedure
                        (record-type-descriptor position-based-mutator)
                        (lambda (pbm s p v)
                          (cond
                           [(and (record? s (position-based-mutator-rtd pbm))
                                 (fixnum? p)
                                 (fx>= p 0)
                                 (< p (position-based-mutator-field-count pbm)))
                            (unsafe-struct-set! s (+ p (position-based-mutator-offset pbm)) v)]
                           [(and (impersonator? s)
                                 (record? (impersonator-val s) (position-based-mutator-rtd pbm))
                                 (fixnum? p)
                                 (fx>= p 0)
                                 (< p (position-based-mutator-field-count pbm)))
                            (let ([abs-pos (+ p (position-based-mutator-offset pbm))])
                              (impersonate-set! (lambda (s v)
                                                  (unsafe-struct-set! s abs-pos v))
                                                (position-based-mutator-rtd pbm)
                                                p
                                                abs-pos
                                                s
                                                v))]
                           [else
                            (error 'struct-set! "bad assignment")])))

  (struct-property-set! prop:procedure
                        (record-type-descriptor named-procedure)
                        0)
  (struct-property-set! prop:object-name
                        (record-type-descriptor named-procedure)
                        1)

  (struct-property-set! prop:procedure
                        (record-type-descriptor reduced-arity-procedure)
                        0)
  (struct-property-set! prop:procedure-arity
                        (record-type-descriptor reduced-arity-procedure)
                        1)

  (struct-property-set! prop:procedure
                        (record-type-descriptor method-procedure)
                        0)
  (struct-property-set! prop:method-arity-error
                        (record-type-descriptor method-procedure)
                        #t)

  (let ([register-procedure-impersonator-struct-type!
         (lambda (rtd)
           (struct-property-set! prop:procedure rtd impersonate-apply))])
    (let ([register-procedure-impersonator-struct-type!
           (lambda (rtd)
             (register-procedure-impersonator-struct-type! rtd)
             (struct-property-set! prop:procedure-arity rtd 4))])
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure-chaperone))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure-impersonator))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure*-chaperone))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure*-impersonator))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure-struct-chaperone))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure-struct-impersonator))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure~-struct-chaperone))
      (register-procedure-impersonator-struct-type! (record-type-descriptor procedure~-struct-impersonator)))
    (register-procedure-impersonator-struct-type! (record-type-descriptor procedure-struct-undefined-chaperone))
    (register-procedure-impersonator-struct-type! (record-type-descriptor procedure~-struct-undefined-chaperone)))

  (let ([register-procedure-incomplete-arity!
         (lambda (rtd)
           (struct-property-set! prop:incomplete-arity rtd #t))])
    (register-procedure-incomplete-arity! (record-type-descriptor procedure~-struct-chaperone))
    (register-procedure-incomplete-arity! (record-type-descriptor procedure~-struct-impersonator))
    (register-procedure-incomplete-arity! (record-type-descriptor procedure~-struct-undefined-chaperone)))

  (let ([register-unsafe-procedure-impersonator-struct-type!
         (lambda (rtd)
           (struct-property-set! prop:procedure rtd 'unsafe))])
    (register-unsafe-procedure-impersonator-struct-type! (record-type-descriptor unsafe-procedure-chaperone))
    (register-unsafe-procedure-impersonator-struct-type! (record-type-descriptor unsafe-procedure-impersonator))))
