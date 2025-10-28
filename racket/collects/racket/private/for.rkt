(module for '#%kernel

  (#%require "misc.rkt"
             "define.rkt"
             "letstx-scheme.rkt"
             "reverse.rkt"
             "sort.rkt"
             "performance-hint.rkt"
             "promise.rkt"
             '#%unsafe
             '#%flfxnum
             (for-syntax '#%kernel
                         "stx.rkt"
                         "qqstx.rkt"
                         "define.rkt"
                         "fixnum.rkt"
                         "define-et-al.rkt" "qq-and-or.rkt" "cond.rkt"
                         "stxcase-scheme.rkt"
                         "more-scheme.rkt"))

  (#%provide for/fold for*/fold
             for/foldr for*/foldr
             for for*
             for/list for*/list
             for/vector for*/vector
             for/lists for*/lists
             for/and for*/and
             for/or for*/or
             for/first for*/first
             for/last for*/last
             for/sum for*/sum
             for/product for*/product
             for/hash for*/hash
             for/hasheq for*/hasheq
             for/hasheqv for*/hasheqv
             for/hashalw for*/hashalw

             for/fold/derived for*/fold/derived
             for/foldr/derived for*/foldr/derived
             (for-syntax split-for-body)
             (for-syntax (rename expand-clause expand-for-clause))

             (rename *in-range in-range)
             (rename *in-inclusive-range in-inclusive-range)
             (rename *in-naturals in-naturals)
             (rename *in-list in-list)
             (rename *in-mlist in-mlist)
             (rename *in-vector in-vector)
             (rename *in-string in-string)
             (rename *in-bytes in-bytes)
             (rename *in-stream in-stream)
             (rename *in-input-port-bytes in-input-port-bytes)
             (rename *in-input-port-chars in-input-port-chars)
             (rename *in-port in-port)
             (rename *in-lines in-lines)
             (rename *in-bytes-lines in-bytes-lines)
             
             in-hash
             in-hash-keys
             in-hash-values
             in-hash-pairs
             in-mutable-hash
             in-mutable-hash-keys
             in-mutable-hash-values
             in-mutable-hash-pairs
             in-immutable-hash
             in-immutable-hash-keys
             in-immutable-hash-values
             in-immutable-hash-pairs
             in-weak-hash
             in-weak-hash-keys
             in-weak-hash-values
             in-weak-hash-pairs
             in-ephemeron-hash
             in-ephemeron-hash-keys
             in-ephemeron-hash-values
             in-ephemeron-hash-pairs

             (rename *in-directory in-directory)

             in-sequences
             in-cycle
             in-parallel
             in-parallel-values
             in-values-sequence
             in-values*-sequence
             stop-before
             stop-after
             (rename *in-producer in-producer)
             (rename *in-indexed in-indexed)
             (rename *in-value in-value)

             stream?
             stream-empty?
             stream-first
             stream-rest
             prop:stream
             stream-ref stream-via-prop? ; only provided for racket/stream
             sequence->stream
             empty-stream make-do-stream

             sequence?
             sequence-generate
             sequence-generate*
             prop:sequence

             define-sequence-syntax
             make-do-sequence
             :do-in

             define-splicing-for-clause-syntax

             define-in-vector-like
             define-:vector-like-gen
             (for-syntax make-in-vector-like
                         for-clause-syntax-protect))

  (module* expand #f
    (#%provide (for-syntax syntax-local-splicing-for-clause-introduce)))

  ;; redefininition of functions not in #%kernel
  (begin-for-syntax
    (define (format-id ctx str . args)
      (define datum 
        (string->symbol (apply format str (map syntax->datum args))))
      (datum->syntax ctx datum))
    (define (join-ids ids sep) ; joins ids with sep; ids = stx-pair
      (syntax-case ids ()
       [(id) #'id]
       [(id . ids) (format-id #'id "~a~a~a" #'id sep (join-ids #'ids sep))])))

  ;; Raise run-time errors from this module as 'racket/primitive
  (define (raise-argument-error who . args)
    (apply raise-argument-error* who 'racket/primitive args))
  (define (raise-arguments-error who . args)
    (apply raise-arguments-error* who 'racket/primitive args))
  (define (raise-range-error who . args)
    (apply raise-range-error* who 'racket/primitive args))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; sequence transformers:

  (begin-for-syntax
    (define-values (struct:sequence-transformer
                    make-sequence-transformer
                    sequence-transformer?
                    sequence-transformer-ref
                    sequence-transformer-set!)
      (make-struct-type 'sequence-transformer #f
                        2 0 #f
                        null (current-inspector)
                        0))

    (define (create-sequence-transformer proc1 proc2)
      (unless (and (procedure? proc1)
                   (or (procedure-arity-includes? proc1 1)
                       (procedure-arity-includes? proc1 0)))
        (raise-argument-error 'define-sequence-syntax
                              "(or/c (procedure-arity-includes/c 0) (procedure-arity-includes/c 1))"
                              0
                              proc1 proc2))
      (unless (and (procedure? proc2)
                   (procedure-arity-includes? proc2 1))
        (raise-argument-error 'define-sequence-syntax
                          "(procedure-arity-includes/c 1)"
                          1
                          proc1 proc2))
      (make-sequence-transformer
       (if (procedure-arity-includes? proc1 0)
           (lambda (stx)
             (if (identifier? stx)
                 (proc1)
                 (datum->syntax stx
                                ;; Use cons, not #`(#,op #,@args), to avoid replacing implicit #%app binding
                                (cons (proc1) (cdr (syntax-e stx)))
                                stx
                                stx)))
           proc1)
       proc2))

    (define (for-clause-syntax-protect clause)
      clause)

    (define sequence-specialization-logger
      (make-logger 'sequence-specialization (current-logger)))

    (define (check-identifier-bindings orig-stx ids-stx kind result)
      (let ([ids (syntax->list ids-stx)])
        (for-each (lambda (id)
                    (unless (identifier? id)
                      (raise-syntax-error
                       #f
                       "expected an identifier to bind"
                       orig-stx
                       id)))
                  ids)
        (let ([dup (check-duplicate-identifier ids)])
          (when dup
            (raise-syntax-error #f
                                (format "duplicate identifier as ~a binding" kind) orig-stx dup)))
        result))

    (define lst-sym (string->uninterned-symbol "lst"))
    (define vec-sym (string->uninterned-symbol "vec"))
    (define rest-sym (string->uninterned-symbol "rest"))
    (define pos-sym (string->uninterned-symbol "pos"))
    (define start-sym (string->uninterned-symbol "start"))
    (define end-sym (string->uninterned-symbol "end"))
    (define inc-sym (string->uninterned-symbol "inc"))
    (define len-sym (string->uninterned-symbol "len"))
    (define i-sym (string->uninterned-symbol "i"))
    (define (expand-clause orig-stx clause flatten-ok?)
      ;; expanded-rhs :: (or/c #f syntax?)
      (let eloop ([use-transformer? #t] [expanded-rhs #f])
        (syntax-case clause (values in-parallel stop-before stop-after :do-in)
          [[(id ...) rhs]
           (check-identifier-bindings orig-stx #'(id ...) "sequence" #f)
           'just-checking]
          [[(id ...) (form . rest)]
           (and use-transformer?
                (identifier? #'form)
                (sequence-transformer? (syntax-local-value #'form (lambda () #f))))
           (let ([m (syntax-local-value #'form)])
             (let ([xformer (sequence-transformer-ref m 1)])
               (let ([xformed
                      (syntax-local-apply-transformer xformer
                                                      #'form
                                                      'expression
                                                      #f
                                                      clause)])
                 (if xformed
                     (let ([r (expand-clause orig-stx
                                             xformed
                                             flatten-ok?)])
                       (syntax-property r
                                        'disappeared-use
                                        (cons (syntax-local-introduce #'form)
                                              (or (syntax-property r 'disappeared-use)
                                                  null))))
                     (eloop #f #f)))))]
          [[(id ...) (:do-in . body)]
           (syntax-case #'body ()
             [(([(outer-id ...) outer-rhs] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-rhs] ...)
               inner-check
               pre-guard
               post-guard
               (loop-arg ...))
              #'body]
             [(([(outer-id ...) outer-rhs] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-rhs] ...)
               pre-guard
               post-guard
               (loop-arg ...))
              #'(([(outer-id ...) outer-rhs] ...)
                 outer-check
                 ([loop-id loop-expr] ...)
                 pos-guard
                 ([(inner-id ...) inner-rhs] ...)
                 (begin)
                 pre-guard
                 post-guard
                 (loop-arg ...))]
             [else (raise-syntax-error #f "bad :do-in clause" orig-stx clause)])]
          [[(id) (values rhs)]
           (expand-clause orig-stx #'[(id) rhs] flatten-ok?)]
          [[(id ...) (in-parallel rhs ...)]
           (and (= (length (syntax->list #'(id ...)))
                   (length (syntax->list #'(rhs ...))))
                (flatten-ok?))
           ;; flatten in-parallel iterations:
           (with-syntax ([(((outer-binding ...)
                            outer-check
                            (loop-binding ...)
                            pos-guard
                            (inner-binding ...)
                            inner-check
                            pre-guard
                            post-guard
                            (loop-arg ...)) ...)
                          (map (lambda (id rhs)
                                 (expand-clause orig-stx #`[(#,id) #,rhs] flatten-ok?))
                               (syntax->list #'(id ...))
                               (syntax->list #'(rhs ...)))])
             #`((outer-binding ... ...)
                (begin outer-check ...)
                (loop-binding ... ...)
                (and pos-guard ...)
                (inner-binding ... ...)
                (begin inner-check ...)
                (and pre-guard ...)
                (and post-guard ...)
                (loop-arg ... ...)))]
          [[(id ...) (stop-before gen-expr pred)]
           (with-syntax ([((outer-binding ...)
                           outer-check
                           (loop-binding ...)
                           pos-guard
                           (inner-binding ...)
                           inner-check
                           pre-guard
                           post-guard
                           (loop-arg ...))
                          (expand-clause orig-stx #`[(id ...) gen-expr] flatten-ok?)])
             #`((outer-binding ...)
                outer-check
                (loop-binding ...)
                pos-guard
                (inner-binding ...)
                inner-check
                (and pre-guard (not (pred id ...)))
                post-guard
                (loop-arg ...)))]
          [[(id ...) (stop-after gen-expr pred)]
           (with-syntax ([((outer-binding ...)
                           outer-check
                           (loop-binding ...)
                           pos-guard
                           (inner-binding ...)
                           inner-check
                           pre-guard
                           post-guard
                           (loop-arg ...))
                          (expand-clause orig-stx #`[(id ...) gen-expr] flatten-ok?)])
             #`((outer-binding ...)
                outer-check
                (loop-binding ...)
                pos-guard
                (inner-binding ...)
                inner-check
                pre-guard
                (and post-guard (not (pred id ...)))
                (loop-arg ...)))]
          [[(id ...) rhs]
           expanded-rhs
           (let ([introducer (make-syntax-introducer)])
             ;; log non-specialized clauses, for performance tuning
             (when (log-level? sequence-specialization-logger 'debug)
               (log-message sequence-specialization-logger
                            'debug
                            (format "non-specialized for clause: ~a:~a:~a"
                                    (syntax-source #'rhs)
                                    (syntax-line   #'rhs)
                                    (syntax-column #'rhs))
                            #'rhs))
             (with-syntax ([[(id ...) rhs*]
                            (introducer (syntax-local-introduce #`[(id ...) #,expanded-rhs]))])
               (with-syntax ([(post-id ...) (generate-temporaries #'(id ...))]
                             [pos pos-sym])
                  (syntax-local-introduce
                   (introducer
                    #`(([(pos->vals pos-pre-inc pos-next init pos-cont? val-cont? all-cont?)
                         #,(syntax-property
                            (syntax/loc #'rhs (make-sequence '(id ...) rhs*))
                            'feature-profile:generic-sequence #t)])
                       (void)
                       ([pos init])
                       #,(syntax-property
                          (syntax/loc #'rhs (if pos-cont? (pos-cont? pos) #t))
                          'feature-profile:generic-sequence #t)
                       ([(id ... all-cont?/pos)
                         (let-values ([(id ...) #,(syntax-property
                                                   (syntax/loc #'rhs (pos->vals pos))
                                                   'feature-profile:generic-sequence #t)])
                           (values id ...
                                   ;; If we need to call `all-cont?`, close over
                                   ;; `id`s here, so `id`s are not implicitly
                                   ;; retained while the body runs:
                                   (and all-cont?
                                        (lambda (pos)
                                          (all-cont? pos id ...)))))]
                        [(pos) #,(syntax-property
                                  (syntax/loc #'rhs (if pos-pre-inc (pos-pre-inc pos) pos))
                                  'feature-profile:generic-sequence #t)])
                       (void)
                       #,(syntax-property
                          (syntax/loc #'rhs (if val-cont? (val-cont? id ...) #t))
                          'feature-profile:generic-sequence #t)
                       #,(syntax-property
                          (syntax/loc #'rhs (if all-cont?/pos (all-cont?/pos pos) #t))
                          'feature-profile:generic-sequence #t)
                       #,(syntax-property
                          (syntax/loc #'rhs ((pos-next pos)))
                          'feature-profile:generic-sequence #t)))))))]
          [[(id ...) rhs]
           (not (syntax-property #'rhs 'for:no-implicit-optimization))
           (with-syntax ([expanded-rhs (local-expand #'rhs 'expression (list #'quote))])
             (syntax-case #'expanded-rhs (quote)
               [(quote e)
                (let ([content (syntax-e #'e)])
                  (cond
                    [(exact-nonnegative-integer? content)
                     (expand-clause orig-stx #'[(id ...) (*in-range (quote e))] flatten-ok?)]
                    [(list? content)
                     (expand-clause orig-stx #'[(id ...) (*in-list (quote e))] flatten-ok?)]
                    [(vector? content)
                     (expand-clause orig-stx #'[(id ...) (*in-vector (quote e))] flatten-ok?)]
                    [(and (hash? content) (immutable? content))
                     (expand-clause orig-stx #'[(id ...) (in-immutable-hash (quote e))] flatten-ok?)]
                    [(string? content)
                     (expand-clause orig-stx #'[(id ...) (*in-string (quote e))] flatten-ok?)]
                    [(bytes? content)
                     (expand-clause orig-stx #'[(id ...) (*in-bytes (quote e))] flatten-ok?)]
                    [else (eloop #f #'expanded-rhs)]))]
               [_ (eloop #f #'expanded-rhs)]))]
          ;; when for:no-implicit-optimization is true
          [[(id ...) rhs] (eloop #f #'rhs)]
          [_
           (raise-syntax-error #f
                               "bad sequence binding clause" orig-stx clause)]))))

  (define-syntax (:do-in stx)
    (raise-syntax-error #f
      "illegal outside of a loop or comprehension binding" stx))

  (define-syntax-rule (unless-unsafe e)
    (unless (variable-reference-from-unsafe? (#%variable-reference)) e))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  splicing clause expansions

  (begin-for-syntax
    (define-values (struct:splicing-for-clause-transformer
                    make-splicing-for-clause-transformer
                    splicing-for-clause-transformer?
                    splicing-for-clause-transformer-ref
                    splicing-for-clause-transformer-set!)
      (make-struct-type 'splicing-for-clause-transformer #f
                        1 0 #f
                        null (current-inspector)
                        0))

    (define (create-splicing-for-clause-transformer proc)
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 1))
        (raise-argument-error 'define-splicing-for-clause-syntax
                              "(procedure-arity-includes/c 1)"
                              proc))
      (make-splicing-for-clause-transformer proc)))

  (define-syntax define-splicing-for-clause-syntax
    (syntax-rules ()
      [(_ id transformer-expr)
       (define-syntax id
         (create-splicing-for-clause-transformer transformer-expr))]))

  (define-for-syntax (syntax-local-splicing-for-clause-introduce stx)
    (unless (syntax? stx)
      (raise-argument-error 'syntax-local-splicing-for-clause-introduce "syntax?" stx))
    (syntax-local-introduce stx))

  (define-for-syntax (expand-splicing-clause orig-form form)
    (syntax-case form ()
      [(id . _)
       (and (identifier? #'id)
            (splicing-for-clause-transformer? (syntax-local-value #'id (lambda () #f))))
       (let ([xformer (splicing-for-clause-transformer-ref (syntax-local-value #'id) 0)])
         (let ([xformed
                (syntax-local-apply-transformer xformer
                                                #'id
                                                'expression
                                                #f
                                                form)])
           (syntax-case xformed ()
             [(_ ...)
              (cons #'id xformed)]
             [_
              (raise-syntax-error #f
                                  "expansion is not a sequence"
                                  orig-form
                                  form)])))]
      [_
       (raise-syntax-error #f
                           "not a splicing for-clause form"
                           orig-form
                           form)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  streams & sequences

  ;; structure type for generic sequences:
  (define-values (struct:do-sequence
                  make-do-sequence
                  do-sequence?
                  do-sequence-ref
                  do-sequence-set!)
    (make-struct-type 'sequence #f 1 0 #f))

  ;; property for generic streams
  (define-values (prop:stream stream-via-prop? stream-ref)
    (make-struct-type-property
     'stream
     (lambda (v si)
       (unless (and (vector? v)
                    (= 3 (vector-length v))
                    (procedure? (vector-ref v 0))
                    (procedure-arity-includes? (vector-ref v 0) 1)
                    (procedure? (vector-ref v 1))
                    (procedure-arity-includes? (vector-ref v 1) 1)
                    (procedure? (vector-ref v 2))
                    (procedure-arity-includes? (vector-ref v 2) 1))
         (raise-argument-error 'guard-for-prop:stream
                               (string-append
                                "(vector/c (procedure-arity-includes/c 1)\n"
                                "          (procedure-arity-includes/c 1)\n"
                                "          (procedure-arity-includes/c 1))")
                               v))
       (vector->immutable-vector v))
     '()
     #t))

  ;; new-style sequence property, where the property value is a procedure
  ;; to get the sequence-driving value and procedures;
  ;; this property is not currently exported
  (define-values (prop:gen-sequence sequence-via-prop? sequence-ref)
    (make-struct-type-property
     'sequence
     (lambda (v si)
       (unless (and (procedure? v)
                    (procedure-arity-includes? v 1))
         (raise-argument-error 'guard-for-prop:sequence
                               "(procedure-arity-includes/c 1)"
                               v))
       v)))

  ;; exported sequence property, where the property value
  ;; is a procedure to get a sequence
  (define-values (prop:sequence :sequence? :sequence-ref)
    (make-struct-type-property
     'sequence
     (lambda (v sinfo)
       (unless (and (procedure? v) (procedure-arity-includes? v 1))
         (raise-argument-error 'sequence-property-guard "(procedure-arity-includes/c 1)" v))
       (lambda (self)
         (let ([s (v self)])
           (unless (sequence? s)
             (raise-mismatch-error
              'sequence-generate
              "procedure (value of prop:sequence) produced a non-sequence: "
              s))
           s)))))

  (define-syntax define-sequence-syntax
    (syntax-rules ()
      [(_ id expr-transformer-expr clause-transformer-expr)
       (define-syntax id
         (create-sequence-transformer expr-transformer-expr
                                      clause-transformer-expr))]))

  (define (stream? v)
    (or (list? v)
        (stream-via-prop? v)))

  (define (unsafe-stream-not-empty? v)
    (if (null? v)
        #f
        (or (pair? v)
            (not ((unsafe-vector-ref (stream-ref v) 0) v)))))

  (define (stream-empty? v)
    (or (null? v)
        (if (stream? v)
            (if (pair? v)
                #f
                ((unsafe-vector-ref (stream-ref v) 0) v))
            (raise-argument-error 'stream-empty?
                                  "stream?"
                                  v))))

  (define (unsafe-stream-first v)
    (cond [(pair? v) (car v)]
          [else ((unsafe-vector-ref (stream-ref v) 1) v)]))

  (define (stream-first v)
    (if (and (stream? v)
             (not (stream-empty? v)))
        (unsafe-stream-first v)
        (raise-argument-error 'stream-first
                              "(and/c stream? (not/c stream-empty?))"
                              v)))

  (define (unsafe-stream-rest v)
    (cond [(pair? v) (cdr v)]
          [else (let ([r ((unsafe-vector-ref (stream-ref v) 2) v)])
                  (unless (stream? r)
                    (raise-mismatch-error 'stream-rest-guard
                                          "result is not a stream: "
                                          r))
                  r)]))

  (define (stream-rest v)
    (if (and (stream? v)
             (not (stream-empty? v)))
        (unsafe-stream-rest v)
        (raise-argument-error 'stream-rest
                              "(and/c stream? (not/c stream-empty?))"
                              v)))

  (define (sequence? v)
    (or (exact-nonnegative-integer? v)
        (do-sequence? v)
        (sequence-via-prop? v)
        (stream? v)
        (mpair? v)
        (vector? v)
        (flvector? v)
        (fxvector? v)
        (string? v)
        (bytes? v)
        (input-port? v)
        (hash? v)
        (and (:sequence? v) (not (struct-type? v)))))

  (define (make-sequence who v)
    (cond
      [(exact-nonnegative-integer? v) (:integer-gen v)]
      [(do-sequence? v)
       (call-with-values (lambda () ((do-sequence-ref v 0)))
         (case-lambda
           [(pos->vals pos-next init pos-cont? val-cont? all-cont?)
            (values pos->vals #f pos-next init pos-cont? val-cont? all-cont?)]
           [(pos->vals pre-pos-next pos-next init pos-cont? val-cont? all-cont?)
            (values pos->vals pre-pos-next pos-next init pos-cont? val-cont? all-cont?)]))]
      [(mpair? v) (:mlist-gen v)]
      [(list? v) (:list-gen v)]
      [(vector? v) (:vector-gen v 0 (vector-length v) 1)]
      [(flvector? v) (:flvector-gen v 0 (flvector-length v) 1)]
      [(fxvector? v) (:fxvector-gen v 0 (fxvector-length v) 1)]
      [(string? v) (:string-gen v 0 (string-length v) 1)]
      [(bytes? v) (:bytes-gen v 0 (bytes-length v) 1)]
      [(input-port? v) (:input-port-gen v)]
      [(hash? v) (:hash-gen v hash-iterate-key+value
                              hash-iterate-first
                              hash-iterate-next)]
      [(sequence-via-prop? v) ((sequence-ref v) v)]
      [(:sequence? v) (make-sequence who ((:sequence-ref v) v))]
      [(stream? v) (:stream-gen v)]
      [else (raise
             (exn:fail:contract
              (format "for: expected a sequence for ~a, got something else: ~v"
                      (if (= 1 (length who))
                          (car who)
                          who)
                      v)
              (current-continuation-marks)))]))

  (define-values (struct:range
                  make-range
                  range?
                  range-ref
                  range-set!)
    (make-struct-type 'stream #f 3 0 #f
                      (list (cons prop:stream
                                  (vector
                                   (lambda (v)
                                     (let ([cont? (range-ref v 2)])
                                       (and cont?
                                            (not (cont? (range-ref v 0))))))
                                   (lambda (v) (range-ref v 0))
                                   (lambda (v) (make-range
                                                ((range-ref v 1) (range-ref v 0))
                                                (range-ref v 1)
                                                (range-ref v 2)))))
                            (cons prop:gen-sequence
                                  (lambda (v)
                                    (values
                                     values
                                     #f
                                     (range-ref v 1)
                                     (range-ref v 0)
                                     (range-ref v 2)
                                     #f
                                     #f))))))

  (define (check-range a b step)
    (check-range-generic 'in-range a b step))

  (define (check-range-generic who a b step)
    (unless (real? a) (raise-argument-error who "real?" a))
    (unless (real? b) (raise-argument-error who "real?" b))
    (unless (real? step) (raise-argument-error who "real?" step)))

  (define in-range
    (case-lambda
      [(b) (in-range 0 b 1)]
      [(a b) (in-range a b 1)]
      [(a b step)
       (check-range a b step)
       (let* ([cont? (if (step . >= . 0)
                         (lambda (x) (< x b))
                         (lambda (x) (> x b)))]
              [inc (lambda (x) (+ x step))])
         (make-range a inc cont?))]))

  (define in-inclusive-range
    (case-lambda
      [(a b) (in-inclusive-range a b 1)]
      [(a b step)
       (check-range-generic 'in-inclusive-range a b step)
       (let* ([cont? (if (step . >= . 0)
                         (lambda (x) (<= x b))
                         (lambda (x) (>= x b)))]
              [inc (lambda (x) (+ x step))])
         (make-range a inc cont?))]))

  (define (:integer-gen v)
    (values values #f add1 0 (lambda (i) (i . < . v)) #f #f))

  (begin-encourage-inline
    (define (check-naturals n)
      (unless (and (integer? n)
                   (exact? n)
                   (n . >= . 0))
        (raise-argument-error 'in-naturals
                              "exact-nonnegative-integer?"
                              n))))

  (define in-naturals
    (case-lambda
      [() (in-naturals 0)]
      [(n)
       (check-naturals n)
       (make-range n add1 #f)]))

  (define-values (struct:list-stream
                  make-list-stream
                  list-stream?
                  list-stream-ref
                  list-stream-set!)
    (make-struct-type 'stream #f 1 0 #f
                      (list (cons prop:stream
                                  (vector
                                   (lambda (v) (not (pair? (list-stream-ref v 0))))
                                   (lambda (v) (car (list-stream-ref v 0)))
                                   (lambda (v) (make-list-stream (cdr (list-stream-ref v 0))))))
                            (cons prop:gen-sequence
                                  (lambda (v)
                                    (values
                                     car
                                     cdr
                                     values
                                     (list-stream-ref v 0)
                                     pair?
                                     #f
                                     #f))))))

  (define (check-list l)
    (unless (list? l) (raise-argument-error 'in-list "list?" l)))
  (define (in-list l)
    (check-list l)
    (make-list-stream l))

  (define (:list-gen l)
    (values car cdr values l pair? #f #f))

  (define (check-mlist l)
    (unless (or (mpair? l) (null? l)) (raise-argument-error 'in-mlist "(or/c mpair? null?)" l)))
  (define (in-mlist l)
    (check-mlist l)
    (make-do-sequence (lambda () (:mlist-gen l))))

  (define (:mlist-gen l)
    (values mcar #f mcdr l mpair? #f #f))

  (define (check-in-input-port-bytes p)
    (unless (input-port? p)
      (raise-argument-error 'in-input-port-bytes "input-port?" p)))
  (define (in-input-port-bytes p)
    (check-in-input-port-bytes p)
    (make-do-sequence (lambda () (:input-port-gen p))))

  (define (:input-port-gen p)
    (values read-byte #f values p #f
            (lambda (x) (not (eof-object? x)))
            #f))

  (define (check-in-input-port-chars p)
    (unless (input-port? p)
      (raise-argument-error 'in-input-port-chars "input-port?" p)))
  (define (in-input-port-chars p)
    (check-in-input-port-chars p)
    (in-producer (lambda () (read-char p)) eof))

  (define (check-in-port r p)
    (unless (and (procedure? r) (procedure-arity-includes? r 1))
      (raise-argument-error 'in-port "(procedure-arity-includes/c 1)" r))
    (unless (input-port? p) (raise-argument-error 'in-port "input-port?" p)))

  (define in-port
    (case-lambda
      [()  (in-port read (current-input-port))]
      [(r) (in-port r (current-input-port))]
      [(r p)
       (check-in-port r p)
       (in-producer (lambda () (r p)) eof)]))

  (define (check-in-lines p mode)
    (unless (input-port? p) (raise-argument-error 'in-lines "input-port?" p))
    (unless (memq mode '(linefeed return return-linefeed any any-one))
      (raise-argument-error
       'in-lines
       "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
       mode)))

  (define in-lines
    (case-lambda
      [()  (in-lines (current-input-port) 'any)]
      [(p) (in-lines p 'any)]
      [(p mode)
       (check-in-lines p mode)
       (in-producer (lambda () (read-line p mode)) eof)]))

  (define (check-in-bytes-lines p mode)
    (unless (input-port? p) (raise-argument-error 'in-bytes-lines "input-port" p))
    (unless (memq mode '(linefeed return return-linefeed any any-one))
      (raise-argument-error
       'in-bytes-lines
       "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
       mode)))

  (define in-bytes-lines
    (case-lambda
      [()  (in-bytes-lines (current-input-port) 'any)]
      [(p) (in-bytes-lines p 'any)]
      [(p mode)
       (check-in-bytes-lines p mode)
       (in-producer (lambda () (read-bytes-line p mode)) eof)]))

  (define (check-stream l)
    (unless (stream? l) (raise-argument-error 'in-stream "stream?" l)))
  (define (in-stream l)
    (check-stream l)
    (make-do-sequence (lambda () (:stream-gen l))))
  
  (define (:stream-gen l)
    (values 
     unsafe-stream-first unsafe-stream-rest values l unsafe-stream-not-empty? #f #f))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; hash sequences
  
  ;; assembles hash iterator functions to give to make-do-sequence
  (define :hash-gen
    (case-lambda
      [(ht -get -first -next)
       (values (lambda (pos) (-get ht pos))
               #f
               (lambda (pos) (-next ht pos))
               (-first ht)
               (lambda (pos) pos) ; #f position means stop
               #f
               #f)]
      [(ht -get -first -next bad-v)
       (values (lambda (pos) (-get ht pos bad-v))
               #f
               (lambda (pos) (-next ht pos))
               (-first ht)
               (lambda (pos) pos) ; #f position means stop
               #f
               #f)]))

  (define (mutable? ht) (not (immutable? ht)))

  ;; Each call defines 4 in-HASHTYPE-VALs sequences,
  ;;   where VAL = key, value, pair, key+value (key+value not used in seq name)
  ;;   and HASHTYPE specifies the the set of hash-iterate- fns to use
  ;;     eg, hash, immutable-hash, mutable-hash, weak-hash
  (define-syntax (define-in-hash-sequences stx)
    (syntax-case stx (element-types:)
     [(_ element-types: V ...)
      (with-syntax
       ([VAL (join-ids #'(V ...) #'+)])
       (with-syntax 
        ([IN-HASH-DEFINER (format-id #'VAL "define-in-hash-~as-seq" #'VAL)])
        #'(begin
           ;; 1) define sequence syntax definer
           ;; where HASHTYPE = hash, immutable-hash, etc
           ;; and "checks" are predicates to apply to the input hash
           ;; (not including hash?)
           (define-syntax (IN-HASH-DEFINER stx)
            (syntax-case stx (hash-type: checks:)
             [(def hash-type: HASHTYPE) #'(def hash-type: HASHTYPE checks:)]
             [(def hash-type: HASHTYPE checks: p? (... ...))
              (with-syntax
               ([IN-HASH-SEQ
                 (if (equal? (syntax->datum #'VAL) 'key+value)
                     (format-id #'def "in-~a" #'HASHTYPE)
                     (format-id #'def "in-~a-~as" #'HASHTYPE #'VAL))]
                [PREFIX
                 (if (equal? (syntax->datum #'HASHTYPE) 'hash)
                     (format-id #'def "~a-iterate" #'HASHTYPE)
                     (format-id #'def "unsafe-~a-iterate" #'HASHTYPE))]
                [HASHTYPE? #'(lambda (ht) (and (hash? ht) (p? ht) (... ...)))]
                [ERR-STR
                 (datum->syntax #'HASHTYPE
                  (if (null? (syntax->list #'(p? (... ...)))) 
                      "hash?"
                      (string-append 
                        "(and/c hash? "
                        (symbol->string 
                          (syntax->datum (join-ids #'(p? (... ...)) #'" "))) 
                        ")")))])
               (with-syntax
                ([-first (format-id #'PREFIX "~a-first" #'PREFIX)]
                 [-next (format-id #'PREFIX "~a-next" #'PREFIX)]
                 [-VAL (format-id #'PREFIX "~a-~a" #'PREFIX #'VAL)]
                 [CHECK-SEQ (format-id #'def "check-~a" #'IN-HASH-SEQ)]
                 [AS-EXPR-SEQ (format-id #'def "default-~a" #'IN-HASH-SEQ)])
                #'(begin
                   (begin-encourage-inline
                     (define (CHECK-SEQ ht)
                       (unless (HASHTYPE? ht)
                         (raise-argument-error 'IN-HASH-SEQ ERR-STR ht))))
                   (define AS-EXPR-SEQ
                     (let ([IN-HASH-SEQ
                            (case-lambda
                              [(ht)
                               (CHECK-SEQ ht)
                               (make-do-sequence (lambda () (:hash-gen ht -VAL -first -next)))]
                              [(ht bad-v)
                               (CHECK-SEQ ht)
                               (make-do-sequence (lambda () (:hash-gen ht -VAL -first -next bad-v)))])])
                       IN-HASH-SEQ))
                   (define-sequence-syntax IN-HASH-SEQ
                    (lambda () #'AS-EXPR-SEQ)
                    (lambda (stx)
                      (define (transform stx)
                        (syntax-case stx ()
                          [[(V ...) (_ ht-expr . extra-args)]
                           (with-syntax ([i i-sym])
                             #'[(V ...)
                                (:do-in
                                 ;;outer bindings
                                 ([(ht) ht-expr])
                                 ;; outer check
                                 (unless-unsafe (CHECK-SEQ ht))
                                 ;; loop bindings
                                 ([i (-first ht)])
                                 ;; pos check
                                 i
                                 ;; inner bindings
                                 ([(V ...) (-VAL ht i . extra-args)])
                                 ;; pre guard
                                 #t
                                 ;; post guard
                                 #t
                                 ;; loop args
                                 ((-next ht i)))])]))
                      (syntax-case stx ()
                        [[(V ...) (_ ht-expr)]
                         (transform stx)]
                        [[(V ...) (_ ht-expr bad-index-expr)]
                         (transform stx)]
                        [_ #f]))))))]))
          ;; 2) define sequence syntaxes (using just-defined definer):
          (IN-HASH-DEFINER hash-type: hash)
          (IN-HASH-DEFINER hash-type: mutable-hash   checks: mutable? hash-strong?)
          (IN-HASH-DEFINER hash-type: immutable-hash checks: immutable?)
          (IN-HASH-DEFINER hash-type: weak-hash      checks: hash-weak?)
          (IN-HASH-DEFINER hash-type: ephemeron-hash checks: hash-ephemeron?))))]))
  (define-in-hash-sequences element-types: key value)
  (define-in-hash-sequences element-types: key)
  (define-in-hash-sequences element-types: value)
  (define-in-hash-sequences element-types: pair)

  ;; Vector-like sequences --------------------------------------------------

  ;; (: check-ranges (Symbol String Natural Integer Integer Natural -> Void))
  ;;
  ;; As no object can have more slots than can be indexed by
  ;; the largest fixnum, after running these checks start,
  ;; stop, and step are guaranteed to be fixnums.
  (define (check-ranges who type-name vec start stop step len)
    (unless (exact-nonnegative-integer? start)
      (raise-argument-error who "exact-nonnegative-integer?" start))
    (unless (or (< start len) (= len start stop))
      (raise-range-error who type-name "starting " start vec 0 (sub1 len)))
    (unless (exact-integer? stop)
      (raise-argument-error who "exact-integer?" stop))
    (unless (and (<= -1 stop) (<= stop len))
      (raise-range-error who type-name "stopping " stop vec -1 len))
    (unless (and (exact-integer? step) (not (zero? step)))
      (raise-argument-error who "(and/c exact-integer? (not/c zero?))" step))
    (when (and (< start stop) (< step 0))
      (raise-arguments-error who 
                             "starting index less than stopping index, but given a negative step"
                             "starting index" start
                             "stopping index" stop
                             "step" step))
    (when (and (< stop start) (> step 0))
      (raise-arguments-error who 
                             "starting index more than stopping index, but given a positive step"
                             "starting index" start
                             "stopping index" stop
                             "step" step)))

  ;; (: normalise-inputs (A) (Symbol String (Any -> Boolean) (A -> Natural) Any Any Any Any -> (values Fixnum Fixnum Fixnum)))
  ;;
  ;; Checks all inputs are valid for an in-vector sequence,
  ;; and if so returns the vector, start, stop, and
  ;; step. Start, stop, and step are guaranteed to be Fixnum
  (define (normalise-inputs who type-name vector? unsafe-vector-length
                            vec start stop step)
    (unless (vector? vec)
      (raise-argument-error who (string-append type-name "?") vec))
    (let* ([len (unsafe-vector-length vec)]
           [stop* (if stop stop len)])
      (check-ranges who type-name vec start stop* step len)
      (values vec start stop* step)))

  (define (unsafe-normalise-inputs unsafe-vector-length vec start stop step)
    (values vec start (or stop (unsafe-vector-length vec)) step))

  (define-syntax define-in-vector-like
    (syntax-rules ()
      [(define-in-vector-like (in-vector-name check-vector-name)
         type-name-str vector?-id vector-length-id :vector-gen-id)
       (begin
         (define in-vector-name
           (case-lambda
             [(v) (in-vector-name v 0 #f 1)]
             [(v start) (in-vector-name v start #f 1)]
             [(v start stop) (in-vector-name v start stop 1)]
             [(v start stop step)
              (let-values (([v start stop step]
                            (normalise-inputs 'in-vector-name type-name-str vector?-id vector-length-id
                                              v start stop step)))
                (make-do-sequence (lambda () (:vector-gen-id v start stop step))))]))
         (define (check-vector-name v)
           (unless (vector?-id v)
             (raise-argument-error 'in-vector-name (string-append type-name-str "?") v))))]))

  (define-syntax define-:vector-like-gen
    (syntax-rules ()
      [(define-:vector-like-gen :vector-like-name unsafe-vector-ref-id)
       (define (:vector-like-name v start stop step)
         (values
          ;; pos->element
          (lambda (i) (unsafe-vector-ref-id v i))
          ;; pre-pos-inc
          #f
          ;; next-pos
          ;; Minor optimisation.  I assume add1 is faster than \x.x+1
          (if (= step 1) add1 (lambda (i) (+ i step)))
          ;; initial pos
          start
          ;; continue?
          (if (> step 0)
              (lambda (i) (< i stop))
              (lambda (i) (> i stop)))
          #f
          #f))]))

  (define-for-syntax (make-in-vector-like in-vector-name
                                          type-name-str
                                          vector?-id
                                          unsafe-vector-length-id
                                          in-vector-id
                                          check-vector-id
                                          unsafe-vector-ref-id)
    (define (in-vector-like stx)
      (with-syntax ([in-vector-name in-vector-name]
                    [type-name type-name-str]
                    [vector? vector?-id]
                    [in-vector in-vector-id]
                    [check-vector check-vector-id]
                    [unsafe-vector-length unsafe-vector-length-id]
                    [unsafe-vector-ref unsafe-vector-ref-id])
        (syntax-case stx ()
          ;; Fast case
          [[(id) (_ vec-expr)]
           (with-syntax ([pos pos-sym]
                         [len len-sym]
                         [vec vec-sym])
             #'[(id)
                (:do-in
                 ;;outer bindings
                 ([(vec len) (let ([vec vec-expr])
                               (unless-unsafe (check-vector vec))
                               (values vec (unsafe-vector-length vec)))])
                 ;; outer check
                 (void)
                 ;; loop bindings
                 ([pos 0])
                 ;; pos check
                 (pos . unsafe-fx< . len)
                 ;; inner bindings
                 ([(id) (unsafe-vector-ref vec pos)])
                 ;; pre guard
                 #t
                 ;; post guard
                 #t
                 ;; loop args
                 ((unsafe-fx+ 1 pos)))])]
          ;; General case
          [((id) (_ vec-expr start))
           (in-vector-like (syntax ((id) (_ vec-expr start #f 1))))]
          [((id) (_ vec-expr start stop))
           (in-vector-like (syntax ((id) (_ vec-expr start stop 1))))]
          [((id) (_ vec-expr start stop step))
           (let ([all-fx? (memq (syntax-e #'step) '(1 -1))])
             #`[(id)
                (:do-in
                 ;; Outer bindings
                 ;; start*, stop*, and step* are guaranteed to be exact integers
                 ([(v* start* stop* step*)
                   (if (variable-reference-from-unsafe? (#%variable-reference))
                       (unsafe-normalise-inputs unsafe-vector-length
                                                vec-expr start stop step)
                       (normalise-inputs (quote in-vector-name) type-name
                                         ;; reverse-eta triggers JIT inlining of
                                         ;; primitives, which is good for futures:
                                         (lambda (x) (vector? x))
                                         (lambda (x) (unsafe-vector-length x))
                                         vec-expr start stop step))])
                 ;; Outer check is done by normalise-inputs
                 (void)
                 ;; Loop bindings
                 ([idx start*])
                 ;; Pos guard
                 #,(cond
                     [(not (number? (syntax-e #'step)))
                      #`(if (step* . >= . 0) (< idx stop*) (> idx stop*))]
                     [((syntax-e #'step) . >= . 0)
                      (if all-fx?
                          #'(unsafe-fx< idx stop*)
                          #'(< idx stop*))]
                     [else
                      (if all-fx?
                          #'(unsafe-fx> idx stop*)
                          #'(> idx stop*))])
                 ;; Inner bindings
                 ([(id) (unsafe-vector-ref v* idx)])
                 ;; Pre guard
                 #t
                 ;; Post guard
                 #t
                 ;; Loop args
                 ((#,(if all-fx? #'unsafe-fx+ #'+) idx step)))])]
          [_ #f])))
    in-vector-like)

  (define-:vector-like-gen :vector-gen unsafe-vector-ref)

  (define-in-vector-like (in-vector check-vector)
    "vector" vector? vector-length :vector-gen)

  (define-sequence-syntax *in-vector
    (lambda () #'in-vector)
    (make-in-vector-like 'in-vector
                         "vector"
                         #'vector?
                         #'unsafe-vector-length
                         #'in-vector
                         #'check-vector
                         #'unsafe-vector-ref))

  (define-:vector-like-gen :string-gen string-ref)

  (define-in-vector-like (in-string check-string)
    "string" string? string-length :string-gen)

  (define-sequence-syntax *in-string
    (lambda () #'in-string)
    (make-in-vector-like 'in-string
                         "string"
                         #'string?
                         #'unsafe-string-length
                         #'in-string
                         #'check-string
                         #'string-ref))

  (define-:vector-like-gen :bytes-gen unsafe-bytes-ref)

  (define-in-vector-like (in-bytes check-bytes)
    "bytes" bytes? bytes-length :bytes-gen)

  (define-sequence-syntax *in-bytes
    (lambda () #'in-bytes)
    (make-in-vector-like 'in-bytes
                         "byte string"
                         #'bytes?
                         #'unsafe-bytes-length
                         #'in-bytes
                         #'check-bytes
                         #'unsafe-bytes-ref))

  (define-:vector-like-gen :flvector-gen unsafe-flvector-ref)
  ;; in-flvector is defined in racket/flonum
  (define-:vector-like-gen :fxvector-gen unsafe-fxvector-ref)
  ;; in-fxvector is defined in racket/fixnum

  ;; ------------------------------------------------------------------------

  (define (stop-before g pred)
    (unless (sequence? g) (raise-argument-error 'stop-before "sequence?" g))
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (raise-argument-error 'stop-before "(procedure-arity-includes/c 1)" pred))
    (make-do-sequence (lambda ()
                        (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                                      (make-sequence #f g)])
                          (values pos->val
                                  pre-pos-next
                                  pos-next
                                  init
                                  pos-cont?
                                  (case-lambda
                                    [(val) (and (if pre-cont? (pre-cont? val) #t)
                                                (not (pred val)))]
                                    [vals (and (if pre-cont? (apply pre-cont? vals) #t)
                                               (not (apply pred vals)))])
                                  post-cont?)))))

  (define (stop-after g pred)
    (unless (sequence? g) (raise-argument-error 'stop-after "sequence?" g))
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (raise-argument-error 'stop-after "(procedure-arity-includes/c 1)" pred))
    (make-do-sequence (lambda ()
                        (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                                      (make-sequence #f g)])
                          (values pos->val
                                  pre-pos-next
                                  pos-next
                                  init
                                  pos-cont?
                                  pre-cont?
                                  (case-lambda
                                    [(pos val) (and (if post-cont? (post-cont? pos val) #t)
                                                    (not (pred val)))]
                                    [(pos . vals) (and (if post-cont? (apply post-cont? pos vals) #t)
                                                       (not (apply pred vals)))]))))))

  (define (in-indexed g)
    (unless (sequence? g) (raise-argument-error 'in-indexed "sequence?" g))
    (make-do-sequence (lambda ()
                        (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                                      (make-sequence #f g)])
                          (values (lambda (pos) (values (pos->val (car pos)) (cdr pos)))
                                  (and pre-pos-next
                                       (lambda (pos) (cons (pre-pos-next (car pos)) (cdr pos))))
                                  (lambda (pos) (cons (pos-next (car pos)) (add1 (cdr pos))))
                                  (cons init 0)
                                  (and pos-cont?
                                       (lambda (pos) (pos-cont? (car pos))))
                                  (and pre-cont?
                                       (lambda (val idx) (pre-cont? val)))
                                  (and post-cont?
                                       (lambda (pos val idx) (post-cont? (car pos) val))))))))

  (define (in-value v)
    (make-do-sequence (lambda ()
                        (values (lambda (pos) v)
                                (lambda (pos) #f)
                                #t
                                (lambda (pos) pos)
                                #f
                                #f))))

  (define (in-values-sequence g)
    (unless (sequence? g) (raise-argument-error 'in-values-sequence "sequence?" g))
    (make-do-sequence (lambda ()
                        (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                                      (make-sequence #f g)])
                          (values (lambda (pos) (call-with-values (lambda () (pos->val pos))
                                                  list))
                                  pre-pos-next
                                  pos-next
                                  init
                                  pos-cont?
                                  (and pre-cont?
                                       (lambda (vals) (apply pre-cont? vals)))
                                  (and post-cont?
                                       (lambda (pos vals) (apply post-cont? pos vals))))))))

  (define (in-values*-sequence g)
    (unless (sequence? g) (raise-argument-error 'in-values*-sequence "sequence?" g))
    (make-do-sequence (lambda ()
                        (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                                      (make-sequence #f g)])
                          (values (lambda (pos) (call-with-values (lambda () (pos->val pos))
                                                  (case-lambda
                                                    [(v) (if (list? v) (list v) v)]
                                                    [vs vs])))
                                  pre-pos-next
                                  pos-next
                                  init
                                  pos-cont?
                                  (and pre-cont?
                                       (lambda (vals)
                                         (if (list? vals)
                                             (apply pre-cont? vals)
                                             (pre-cont? vals))))
                                  (and post-cont?
                                       (lambda (pos vals)
                                         (if (list? vals)
                                             (apply post-cont? pos vals)
                                             (post-cont? pos vals)))))))))

  ;; ----------------------------------------

  (define (append-sequences sequences cyclic?)
    (define (seqs->m+g+r seqs)
      (if (pair? seqs)
        (let-values ([(more? get) (sequence-generate (car seqs))]
                     [(seqs) (cdr seqs)])
          (if (more?) (list* more? get seqs) (seqs->m+g+r seqs)))
        (and cyclic? (seqs->m+g+r sequences))))
    (make-do-sequence
     (lambda ()
       ;; place is (cur-more? cur-get rest-seqs ...) or #f
       (values (lambda (m+g+r) ((cadr m+g+r)))
               (lambda (m+g+r)
                 (if (and (pair? m+g+r) (not ((car m+g+r))))
                   (seqs->m+g+r (cddr m+g+r))
                   m+g+r))
               (seqs->m+g+r sequences)
               values
               #f
               #f))))

  (define (check-sequences who sequences)
    (for-each (lambda (g)
                (unless (sequence? g) (raise-argument-error who "sequence?" g)))
              sequences))

  (define (in-sequences . sequences)
    (check-sequences 'in-sequences sequences)
    (if (and (pair? sequences) (null? (cdr sequences)))
        (car sequences)
        (append-sequences sequences #f)))
  (define (in-cycle . sequences)
    (check-sequences 'in-cycle sequences)
    (append-sequences sequences #t))

  (define (in-parallel . sequences)
    (check-sequences 'in-parallel sequences)
    (if (= 1 (length sequences))
        (car sequences)
        (make-do-sequence
         (lambda ()
           (let-values ([(pos->vals pre-pos-nexts pos-nexts inits pos-cont?s pre-cont?s post-cont?s)
                         (for/lists (p->v p-p-n p-n i ps? pr? po?) ([g sequences])
                           (make-sequence #f g))])
             (values
              (lambda (poses) (apply values (map (lambda (pos->val pos) (pos->val pos))
                                                 pos->vals
                                                 poses)))
              (and (ormap values pre-pos-nexts)
                   (lambda (poses) (map (lambda (pre-pos-next pos) (if pre-pos-next (pre-pos-next pos) pos))
                                   pre-pos-nexts
                                   poses)))
              (lambda (poses) (map (lambda (pos-next pos) (pos-next pos))
                              pos-nexts
                              poses))
              inits
              (and (ormap values pos-cont?s)
                   (lambda (poses) (andmap (lambda (pos-cont? pos)
                                             (if pos-cont? (pos-cont? pos) #t))
                                           pos-cont?s
                                           poses)))
              (and (ormap values pre-cont?s)
                   (lambda vals (andmap (lambda (pre-cont? val)
                                          (if pre-cont? (pre-cont? val) #t))
                                        pre-cont?s
                                        vals)))
              (and (ormap values post-cont?s)
                   (lambda (poses . vals) (andmap (lambda (post-cont? pos val)
                                                    (if post-cont? (post-cont? pos val) #t))
                                                  post-cont?s
                                                  poses
                                                  vals)))))))))

  (define (in-parallel-values . orig-counts+sequences)
    (define who 'in-parallel-values)
    (define-values (sequences counts)
      (let loop ([counts+sequences orig-counts+sequences] [s-accum null] [c-accum null])
        (cond
          [(null? counts+sequences)
           (values (reverse s-accum) (reverse c-accum))]
          [(not (exact-nonnegative-integer? (car counts+sequences)))
           (raise-argument-error who "exact-nonnegative-integer?" (car counts+sequences))]
          [(null? (cdr counts+sequences))
           (raise-arguments-error who "missing a sequence after its result count"
                                  "result count" (car counts+sequences))]
          [(not (sequence? (cadr counts+sequences)))
           (raise-argument-error who "sequence?" (cadr counts+sequences))]
          [else (loop (cddr counts+sequences)
                      (cons (cadr counts+sequences) s-accum)
                      (cons (car counts+sequences) c-accum))])))
    (define (apply-val-preds poses vals cont?s)
      (let loop ([vals vals] [poses poses] [counts counts] [cont?s cont?s])
        (cond
          [(null? vals) #f]
          [else
           (define cont? (car cont?s))
           (or (and cont?
                    (let ([vals (let loop ([count (car counts)] [vals vals])
                                  (if (= 0 count)
                                      null
                                      (cons (car vals) (loop (sub1 count) (cdr vals)))))])
                      (if poses
                          (apply cont? (car poses) vals)
                          (apply cont? vals))))
               (loop (list-tail vals (car counts)) (and poses (cdr poses)) (cdr counts) (cdr cont?s)))])))
    (if (= 1 (length sequences))
        (car sequences)
        (make-do-sequence
         (lambda ()
           (let-values ([(pos->vals pre-pos-nexts pos-nexts inits pos-cont?s pre-cont?s post-cont?s)
                         (for/lists (p->v p-p-n p-n i ps? pr? po?) ([g sequences])
                           (make-sequence #f g))])
             (values
              (lambda (poses) (apply values
                                     (apply append (map (lambda (pos->val pos)
                                                          (call-with-values (lambda () (pos->val pos))
                                                                            list))
                                                        pos->vals
                                                        poses))))
              (and (ormap values pre-pos-nexts)
                   (lambda (poses) (map (lambda (pre-pos-next pos) (if pre-pos-next (pre-pos-next pos) pos))
                                   pre-pos-nexts
                                   poses)))
              (lambda (poses) (map (lambda (pos-next pos) (pos-next pos))
                              pos-nexts
                              poses))
              inits
              (and (ormap values pos-cont?s)
                   (lambda (poses) (andmap (lambda (pos-cont? pos)
                                             (if pos-cont? (pos-cont? pos) #t))
                                           pos-cont?s
                                           poses)))
              (and (ormap values pre-cont?s)
                   (lambda vals
                     (apply-val-preds #f vals pre-cont?s)))
              (and (ormap values post-cont?s)
                   (lambda (poses . vals)
                     (apply-val-preds poses vals post-cont?s)))))))))

  (define in-producer
    (case-lambda
      [(producer)
       ;; simple stop-less version
       (make-do-sequence (lambda () (values (λ _ (producer)) void (void) #f #f #f)))]
      [(producer stop . more)
       (define produce!
         (if (null? more)
           (lambda (_) (producer))
           (lambda (_) (apply producer more))))
       (define stop?
         (cond [(not (procedure? stop))
                (lambda (x) (not (eq? x stop)))]
               [(equal? 1 (procedure-arity stop))
                (lambda (x) (not (stop x)))]
               [else
                (lambda xs (not (apply stop xs)))]))
       (make-do-sequence
        (lambda ()
          (values produce! void (void) #f stop? #f)))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  running sequences outside of a loop:

  (define-values (struct:do-stream
                  make-do-stream
                  do-stream?
                  do-stream-ref
                  do-stream-set!)
    (make-struct-type 'stream #f 3 0 #f
                      (list (cons prop:stream
                                  (vector
                                   (lambda (v) ((do-stream-ref v 0)))
                                   (lambda (v) ((do-stream-ref v 1)))
                                   (lambda (v) ((do-stream-ref v 2))))))))

  (define empty-stream (make-do-stream (lambda () #t) void void))

  (define (sequence->stream s)
    (unless (sequence? s)
      (raise-argument-error 'sequence->stream "sequence?" s))
    (cond
      [(stream? s) s]
      [else
       (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                     (make-sequence #f s)])
         (define (gen-stream pos)
           (let ([done? #f]
                 [vals #f]
                 [empty? #f]
                 [next #f])
             (define (force!)
               (unless done?
                 (if (if pos-cont? (pos-cont? pos) #t)
                     (begin
                       (set! vals (call-with-values (lambda () (pos->val pos)) list))
                       (when pre-pos-next (set! pos (pre-pos-next pos)))
                       (unless (if pre-cont? (apply pre-cont? vals) #t)
                         (set! vals #f)
                         (set! empty? #t)))
                     (set! empty? #t))
                 (set! done? #t)))
             (make-do-stream (lambda () (force!) empty?)
                             (lambda () (force!) (apply values vals))
                             (lambda ()
                               (force!)
                               (if next
                                   next
                                   (begin
                                     (if (if post-cont? (apply post-cont? pos vals) #t)
                                         (set! next (gen-stream (pos-next pos)))
                                         (set! next empty-stream))
                                     next))))))
         (gen-stream init))]))

  (define (no-more)
    (raise (exn:fail:contract "sequence has no more values"
                              (current-continuation-marks))))

  (define (sequence-generate g)
    (unless (sequence? g)
      (raise-argument-error 'sequence-generate "sequence?" g))
    (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                  (make-sequence #f g)])
      (let ([pos init])
        (letrec ([more? #f]
                 [prep-val! #f]
                 [next #f])
          (letrec ([init-more?
                    (lambda () (prep-val!) (more?))]
                   [init-next
                    (lambda () (prep-val!) (next))]
                   [init-prep-val!
                    (lambda ()
                      (if (if pos-cont? (pos-cont? pos) #t)
                          (call-with-values
                           (lambda ()
                             (begin0
                              (pos->val pos)
                              (when pre-pos-next
                                (set! pos (pre-pos-next pos)))))
                           (lambda vals
                             (if (if pre-cont? (apply pre-cont? vals) #t)
                                 (begin
                                   (set! more? (lambda () #t))
                                   (set! next
                                         (lambda ()
                                           (let ([v vals])
                                             (set! prep-val!
                                                   (let ([saved-vals (and post-cont? vals)])
                                                     (lambda ()
                                                       (if (if post-cont?
                                                               (apply post-cont? pos saved-vals)
                                                               #t)
                                                           (begin
                                                             (set! pos (pos-next pos))
                                                             (set! prep-val! init-prep-val!)
                                                             (prep-val!))
                                                           (begin
                                                             (set! more? (lambda () #f))
                                                             (set! next no-more))))))
                                             (set! more? init-more?)
                                             (set! next init-next)
                                             (apply values v))))
                                   (set! prep-val! void)
                                   (apply values vals))
                                 (begin
                                   (set! more? (lambda () #f))
                                   (set! next no-more)))))
                          (begin
                            (set! more? (lambda () #f))
                            (set! next no-more))))])
            (set! more? init-more?)
            (set! prep-val! init-prep-val!)
            (set! next init-next)
            (let ([sequence-more? (lambda () (more?))]
                  [sequence-next (lambda () (next))])
              (values sequence-more?
                      sequence-next)))))))

  (define (sequence-generate* g)
    (unless (sequence? g)
      (raise-argument-error 'sequence-generate* "sequence?" g))
    (let-values ([(pos->val pre-pos-next pos-next init pos-cont? pre-cont? post-cont?)
                  (make-sequence #f g)])
      (letrec ([next!
                (lambda (pos)
                  (if (if pos-cont? (pos-cont? pos) #t)
                      (call-with-values
                        (lambda () (begin0
                               (pos->val pos)
                               (when pre-pos-next
                                 (set! pos (pre-pos-next pos)))))
                        (lambda vals
                          (if (if pre-cont? (apply pre-cont? vals) #t)
                              (values vals
                                      (let ([saved-vals (and post-cont? vals)])
                                        (lambda ()
                                          (if (if post-cont?
                                                  (apply post-cont? pos saved-vals)
                                                  #t)
                                              (next! (pos-next pos))
                                              (values #f no-more)))))
                              (values #f no-more))))
                      (values #f no-more)))])
        (next! init))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  core `for/fold' syntax

  (define-syntax values*
    (syntax-rules ()
      [(_ x) x]
      [(_ x ...) (values x ...)]))

  (define-syntax-rule (inner-recur/fold ([int-var fold-var] ...) [expr ...] next-k)
    (let-values ([(fold-var ...) (let-values () expr ...)])
      (let ([int-var fold-var]
            ...)
        next-k)))

  (define-for-syntax ((make-inner-recur/foldr/strict fold-vars) stx)
    (syntax-case stx ()
      [(_ () [expr ...] next-k)
       #`(let-values ([#,(map syntax-local-introduce fold-vars) next-k])
           expr ...)]))

  (define-for-syntax ((make-inner-recur/foldr/lazy fold-vars delayed-id delayer-id) stx)
    (syntax-case stx ()
      [(_ () [expr ...] next-k)
       (with-syntax ([(fold-var ...) (map syntax-local-introduce fold-vars)]
                     [delayed-id (syntax-local-introduce delayed-id)]
                     [delayer-id delayer-id])
         #`(let*-values
               ([(delayed-id) (delayer-id next-k)]
                #,@(cond
                     [(= (length fold-vars) 1)
                      #`([(fold-var ...) delayed-id])]
                     [(delayer? (syntax-local-value #'delayer-id (lambda () #f)))
                      #`([(fold-var) (delayer-id (let-values ([(fold-var ...) (force delayed-id)])
                                                   fold-var))]
                         ...)]
                     [else #'()]))
             expr ...))]))
  
  (define-syntax (push-under-break stx)
    (syntax-case stx ()
      [(_ inner-recur fold-bind [expr ...] next-k break-k final?-id)
       (let loop ([l (syntax->list #'(expr ...))] [pre-accum null])
         (cond
          [(null? l) 
           ;; No #:break form
           #'(inner-recur fold-bind [expr ...] (if final?-id break-k next-k))]
          [(eq? '#:break (syntax-e (car l)))
           ;; Found a #:break form
           #`(let-values ()
               #,@(reverse pre-accum)
               (if #,(cadr l)
                   break-k
                   (push-under-break inner-recur fold-bind #,(cddr l) next-k break-k final?-id)))]
          [(eq? '#:final (syntax-e (car l)))
           ;; Found a #:final form
           #`(let-values ()
               #,@(reverse pre-accum)
               (let ([final? (or #,(cadr l) final?-id)])
                 (push-under-break inner-recur fold-bind #,(cddr l) next-k break-k final?)))]
          [else (loop (cdr l) (cons (car l) pre-accum))]))]))

  (define-for-syntax (wrap-init bind-init body)
    (syntax-case bind-init ()
      [() body]
      [(bind ...) #`(letrec-syntax (bind ...)
                      #,body)]))

  ;; For checking that parallel sequences end together; `state` for each sequence
  ;; got to `#f` when it has terminated
  (define-syntax (if/c stx)
    (syntax-case stx ()
      [(_ (and tst ...) => () thn els)
       ;; common case: no checking
       #'(if (and tst ...) thn els)]
      [(_ (and tst ...) => (state ...) thn els)
       (with-syntax ([((tst/s ...) (tst ...)) (let loop ([tsts (syntax->list #'(tst ...))]
                                                         [states (syntax->list #'(state ...))]
                                                         [accum null])
                                                  (cond
                                                    [(null? states) (list (reverse accum) tsts)]
                                                    [else (loop (cdr tsts) (cdr states) (cons (car tsts) accum))]))])
         #`(let ([state (and state tst/s)]
                 ...)
             (if (and (or state ...) tst ...)
                 thn
                 els)))]))
  (define-syntax (let-values/c stx)
    (syntax-case stx ()
      [(let-values/c ((bind ...) ...) <= () on-ragged #f body ...)
       #'(let-values (bind ... ...) body ...)]
      [(let-values/c ((bind ...) ...) <= () on-ragged (ragged-expr) body ...)
       ;; no tracking needed, but have `ragged-expr` appear in expansion
       #'(let-values (bind ... ...) (if #f ragged-expr (void)) body ...)]
      [(let-values/c (([(id ...) rhs] ...) ...) <= (state ...) on-ragged (ragged-expr) body ...)
       (with-syntax ([((state ...) ...) (let loop ([states (syntax->list #'(state ...))]
                                                   [idsss (syntax->list #'(((id ...) ...) ...))])
                                          (cond
                                            [(null? states) null]
                                            [else (cons (map (lambda (ids) (car states)) (syntax->list (car idsss)))
                                                        (loop (cdr states) (cdr idsss)))]))])
         #'(let-values ([(id ...) (if state
                                      rhs
                                      (values (and #f 'id) ...))]
                        ...
                        ...
                        [(on-ragged) (lambda () (if #t ; make sure `ragged-expr` is just an expression
                                                    ragged-expr
                                                    (void)))])
             body
             ...))]))
  (define-syntax (guard/c stx)
    (syntax-case stx ()
      [(_ states) #'(void)]
      [(_ () inner-check ...)
       #'(begin inner-check ...)]
      [(_ (state ...) inner-check ...)
       #'(begin (when state inner-check (void)) ...)]))
  (define-syntax (check/c stx)
    (syntax-case stx ()
      [(_ () on-ragged thn els) #'els]
      [(_ (state ...) on-ragged thn els) #'(if (and (not (and state ...))
                                                    (or state ...))
                                               (begin
                                                 (on-ragged)
                                                 thn)
                                               els)]))

  (define-syntax (for/foldX/derived stx)
    (syntax-case stx ()
      ;; Force expression context
      [_
       (not (eq? 'expression (syntax-local-context)))
       #`(#%expression #,stx)]
      ;; Done case (no more clauses, and no generated clauses to emit):
      [(_ [orig-stx inner-recur nested? emit? () #f] ([int-var fold-var] ...) bind-init next-k break-k final?-id ()
          expr1 expr ...)
       (wrap-init
        #'bind-init
        (if (syntax-e #'inner-recur)
            ;; General, non-nested-loop approach:
            #`(push-under-break inner-recur ([int-var fold-var] ...) [expr1 expr ...] next-k break-k final?-id)
            ;; Nested-loop approach (which is slightly faster when it works):
            #`(let-values ([(fold-var ...) (let () expr1 expr ...)])
                (values fold-var ...))))]
      ;; Switch-to-emit case (no more clauses to generate):
      [(_ [orig-stx inner-recur nested? #f binds ragged] fold-bind bind-init next-k break-k final?-id () . body)
       #`(for/foldX/derived [orig-stx inner-recur nested? #t binds ragged] fold-bind bind-init next-k break-k final?-id () . body)]
      ;; Emit case:
      [(_ [orig-stx inner-recur nested? #t binds ragged] ([int-var fold-var] ...) bind-init next-k break-k final?-id rest expr1 . body)
       (with-syntax ([(([outer-binding ...]
                        outer-check
                        [loop-binding ...]
                        pos-guard
                        [inner-binding ...]
                        inner-check
                        pre-guard
                        post-guard
                        [loop-arg ...]) ...)
                      (reverse (syntax->list #'binds))])
         (with-syntax ([(state ...) (if (and (syntax-e #'ragged)
                                             ;; no ragged tracking needed if there's less than 2 clauses
                                             ((length (syntax->list #'(outer-check ...))) . >= . 2))
                                        (generate-temporaries #'(outer-check ...))
                                        '())]
                       [(nonempty? ...) (if (null? (syntax->list #'(outer-check ...)))
                                            ;; `#:on-length-mismatch` can create an emit demand even when there are no clauses
                                            '(#f)
                                            #'())])
           (quasisyntax/loc #'orig-stx
             (let-values (outer-binding ... ...)
              outer-check ...
              #,(quasisyntax/loc #'orig-stx
                  (let for-loop ([int-var int-var]
                                 ...
                                 loop-binding ... ...
                                 [state #t] ...)
                    #,(wrap-init
                       #'bind-init
                       #`(if/c (and pos-guard ...) => (state ...)
                               (let-values/c ((inner-binding ...) ...) <= (state ...) on-ragged ragged
                                 (guard/c (state ...) inner-check ...)
                                 (if/c (and pre-guard ...) => (state ...)
                                       #,(if (syntax-e #'inner-recur)
                                             ;; The general non-nested-loop approach:
                                             #'(let ()
                                                 (define (next-k-proc int-var ...)
                                                   (if/c (and post-guard ... nonempty? ...) => (state ...)
                                                         (for-loop int-var ... loop-arg ... ... state ...)
                                                         next-k))
                                                 (check/c
                                                  (state ...)
                                                  on-ragged
                                                  next-k
                                                  (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
                                                    ([int-var fold-var] ...) ()
                                                    (next-k-proc int-var ...) break-k final?-id
                                                    rest expr1 . body)))
                                             ;; The specialized nested-loop approach, which is
                                             ;; slightly faster when it works:
                                             #'(let ()
                                                 (check/c
                                                  (state ...)
                                                  on-ragged
                                                  next-k
                                                  (let-values ([(int-var ...)
                                                                (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
                                                                  ([int-var fold-var] ...) ()
                                                                  next-k break-k final?-id
                                                                  rest expr1 . body)])
                                                    (if/c (and post-guard ... (not final?-id) nonempty? ...) => (state ...)
                                                          (for-loop int-var ... loop-arg ... ... state ...)
                                                          next-k)))))
                                       next-k))
                               next-k))))))))]
      ;; Bad body cases:
      [(_ [orig-stx . _] fold-bind bind-init next-k break-k final?-id ())
       (raise-syntax-error
        #f "missing body expression after sequence bindings" #'orig-stx)]
      [(_ [orig-stx . _] fold-bind bind-init next-k break-k final?-id () . rest)
       (raise-syntax-error
        #f "bad syntax (illegal use of `.') after sequence bindings" #'orig-stx)]
      ;; Splicing-expand case:
      [(_ [orig-stx inner-recur nested? emit? binds ragged] fold-bind bind-init next-k break-k final?-id (#:splice form . rest) . body)
       (with-syntax ([(id clause ...) (expand-splicing-clause #'orig-stx #'form)])
         (syntax-property #'(for/foldX/derived [orig-stx inner-recur nested? emit? binds ragged]
                              fold-bind bind-init next-k break-k final?-id (clause ... #:when #t . rest) . body)
                          'disappeared-use
                          (syntax-local-introduce #'id)))]
      ;; Ragged-finish case
      [(_ [orig-stx inner-recur nested? #f binds #f] fold-bind bind-init next-k break-k final?-id (#:on-length-mismatch expr . rest) . body)
       #'(for/foldX/derived [orig-stx inner-recur nested? #t binds (expr)] fold-bind bind-init next-k break-k final?-id rest . body)]
      ;; Guard case, no pending emits:
      [(_ [orig-stx inner-recur nested? #f () #f] fold-bind bind-init next-k break-k final?-id (#:when expr . rest) . body)
       (wrap-init
        #'bind-init
        #'(if expr
              (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
                fold-bind () next-k break-k final?-id rest . body)
              next-k))]
      ;; Negative guard case, no pending emits:
      [(_ [orig-stx inner-recur nested? #f () #f] fold-bind bind-init next-k break-k final?-id (#:unless expr . rest) . body)
       (wrap-init
        #'bind-init
        #'(if expr
              (if final?-id break-k next-k)
              (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
                fold-bind () next-k break-k final?-id rest . body)))]
      ;; Break case, no pending emits:
      [(_ [orig-stx inner-recur nested? #f () #f] fold-bind bind-init next-k break-k final?-id (#:break expr . rest) . body)
       (wrap-init
        #'bind-init
        #'(if expr
              break-k
              (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
                fold-bind () next-k break-k final?-id rest . body)))]
      ;; Final case, no pending emits:
      [(_ [orig-stx inner-recur nested? #f () #f] fold-bind bind-init next-k break-k final?-id (#:final expr . rest) . body)
       (wrap-init
        #'bind-init
        #'(let ([final? (or expr final?-id)])
            (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
              fold-bind () next-k break-k final? rest . body)))]
      ;; General "do" case, no pending emits:
      [(_ [orig-stx inner-recur nested? #f () #f] fold-bind bind-init next-k break-k final?-id (#:do forms . rest) . body)
       (syntax-case #'forms ()
         [(form ...)
          (wrap-init
           #'bind-init
           #'(let ()
               form ...
               (for/foldX/derived [orig-stx inner-recur nested? #f () #f]
                 fold-bind () next-k break-k final?-id rest . body)))]
         [_
          (raise-syntax-error #f "expected parenthesized sequence after `#:do`" #'orig-stx #'forms)])]
      ;; Keyword case, pending emits need to be flushed first
      [(_ [orig-stx inner-recur nested? #f binds ragged] fold-bind bind-init next-k break-k final?-id (kw expr . rest) . body)
       (or (eq? (syntax-e #'kw) '#:when)
           (eq? (syntax-e #'kw) '#:unless)
           (eq? (syntax-e #'kw) '#:break)
           (eq? (syntax-e #'kw) '#:final)
           (eq? (syntax-e #'kw) '#:do))
       #'(for/foldX/derived [orig-stx inner-recur nested? #t binds ragged] fold-bind bind-init next-k break-k final?-id (kw expr . rest) . body)]
      ;; Convert single-value form to multi-value form:
      [(_ [orig-stx inner-recur nested? #f binds ragged] fold-bind bind-init next-k break-k final?-id ([id rhs] . rest) . body)
       (identifier? #'id)
       #'(for/foldX/derived [orig-stx inner-recur nested? #f binds ragged]
           fold-bind bind-init next-k break-k final?-id
           ([(id) rhs] . rest) . body)]
      ;; Expand one multi-value clause, and push it into the results to emit:
      [(_ [orig-stx inner-recur nested? #f binds ragged] fold-bind bind-init next-k break-k final?-id (clause . rest) . body)
       (with-syntax ([bind (expand-clause #'orig-stx #'clause (lambda ()
                                                                ;; flattening `in-parallel` is ok if there's no
                                                                ;; `#:on-length-mismatch` afterward
                                                                (let loop ([rest #'rest])
                                                                  (syntax-case rest ()
                                                                    [() #t]
                                                                    [(#:on-length-mismatch . _) #f]
                                                                    [(kw . _) (keyword? (syntax-e #'kw)) #t]
                                                                    [(_ . rest) (loop #'rest)]))))])
         (let ([r #`(for/foldX/derived [orig-stx inner-recur nested? nested? (bind . binds) ragged]
                      fold-bind bind-init next-k break-k final?-id rest . body)]
               [d (syntax-property #'bind 'disappeared-use)])
           (if d
               (syntax-property r 'disappeared-use d)
               r)))]
      [(_ [orig-stx . _] fold-bind bind-init next-k break-k final?-id clauses . _)
       (not (syntax->list #'clauses))
       (raise-syntax-error #f "bad sequence binding clauses" #'orig-stx #'clauses)]
      [(_ [orig-stx . _] . _)
       (raise-syntax-error #f "bad syntax" #'orig-stx)]))

  (define-syntax (for/fold/derived/final stx)
    (syntax-case stx ()
      [(_ [orig-stx nested?] fold-bind bind-init done-k (clause ...) expr ...)
       ;; If there's a `#:break` or `#:final`, then we need to use the
       ;; non-nested loop approach to implement them:
       (ormap (lambda (s) (or (eq? '#:break (syntax-e s))
                              (eq? '#:final (syntax-e s))
                              ;; might generate `#:break` or `#:final`:
                              (eq? '#:splice (syntax-e s))))
              (syntax->list #'(clause ... expr ...)))
       #'(for/foldX/derived [orig-stx inner-recur/fold nested? #f () #f] fold-bind bind-init done-k done-k #f (clause ...) expr ...)]
      [(_ [orig-stx nested?] fold-bind bind-init done-k . rest)
       ;; Otherwise, allow compilation as nested loops, which can be slightly faster:
       #'(for/foldX/derived [orig-stx #f nested? #f () #f] fold-bind bind-init done-k done-k #f . rest)]))

  (define-for-syntax (make-fold-var sym)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! id rhs)
          (with-syntax ([(set! . _) stx])
            (datum->syntax stx (list #'set! (datum->syntax #'id sym #'id #'id) #'rhs) stx stx))]
         [(id . args) (datum->syntax stx (cons (datum->syntax #'id sym #'id #'id) #'args) stx stx)]
         [_ (datum->syntax stx sym stx stx)]))))

  (define-syntaxes (for/fold/derived for*/fold/derived for/foldr/derived for*/foldr/derived)
    (let ()
      (define (parse-bindings+options stx orig-stx right?)
        (let loop ([stx stx]
                   [parsed-any-opts? #f]
                   [bindings '()]
                   [result-expr #f]
                   [delay? #f]
                   [delayed-id #f]
                   [delayer-id #f])
          (syntax-case stx ()
            [()
             (let ([delay? (or delay? delayed-id delayer-id)])
               (values (reverse bindings)
                       result-expr
                       delay?
                       (or delayed-id #'delayed)
                       (or delayer-id #'delay)))]
            [([fold-var fold-init] . rest)
             (not parsed-any-opts?)
             (loop #'rest #f (cons #'[fold-var fold-init] bindings)
                   result-expr delay? delayed-id delayer-id)]
            [(#:result expr . rest)
             (not (keyword? (syntax-e #'expr)))
             (if result-expr
                 (raise-syntax-error #f "duplicate #:result option" orig-stx #'kw)
                 (loop #'rest #t bindings #'expr delay? delayed-id delayer-id))]
            [(#:result . rest)
             (raise-syntax-error #f "expected expression for #:result option" orig-stx #'rest)]
            [(kw . rest)
             (and right? (eq? (syntax-e #'kw) '#:delay))
             (if delay?
                 (raise-syntax-error #f "duplicate #:delay option" orig-stx #'kw)
                 (loop #'rest #t bindings result-expr #t delayed-id delayer-id))]
            [(#:delay-as id . rest)
             (and right? (identifier? #'id))
             (if delayed-id
                 (raise-syntax-error #f "duplicate #:delay-as option" orig-stx #'id)
                 (loop #'rest #t bindings result-expr delay? #'id delayer-id))]
            [(#:delay-as . rest)
             right?
             (raise-syntax-error #f "expected identifier for #:delay-as option" orig-stx #'rest)]
            [(#:delay-with id . rest)
             (and right? (identifier? #'id))
             (if delayer-id
                 (raise-syntax-error #f "duplicate #:delay-with option" orig-stx #'id)
                 (loop #'rest #t bindings result-expr delay? delayed-id #'id))]
            [(#:delay-with . rest)
             right?
             (raise-syntax-error #f "expected identifier for #:delay-with option" orig-stx #'rest)]
            [_
             (raise-syntax-error #f
                                 (if parsed-any-opts?
                                     "invalid accumulator option(s)"
                                     "invalid accumulator binding clause(s)")
                                 orig-stx
                                 stx)])))

      (define ((make for*? right?) stx-in)
        ;; Add a fresh scope that acts the "outer edge" scope of `let`, so
        ;; that as we expand clause transformers, we don't end up with
        ;; ambiguities between things that started out in the form and that
        ;; were introduced by expansion. This is necessary due to the way
        ;; `for` performs its own expansion of clauses.
        (define stx (internal-definition-context-add-scopes
                     (syntax-local-make-definition-context)
                     stx-in))
        (syntax-case stx ()
          [(_ orig-stx bindings+options . rest)
           (let ()
             (define-values (bindings result-expr delay? delayed-id delayer-id)
               (parse-bindings+options #'bindings+options #'orig-stx right?))
             (with-syntax ([([fold-var fold-init] ...) bindings]
                           [delayed-id delayed-id]
                           [delayer-id delayer-id])
               (check-identifier-bindings #'orig-stx #`(fold-var ... delayed-id) "accumulator" (void))
               (cond
                 [right?
                  (define loop-stx
                    (quasisyntax/loc #'orig-stx
                      (for/foldX/derived [orig-stx inner-recur/foldr #,for*? #f () #f]
                        ()
                        ()
                        (done-k-proc)
                        (done-k-proc)
                        #f
                        . rest)))
                  (quasisyntax/loc #'orig-stx
                    (let ([done-k-proc (lambda () (values* fold-init ...))])
                      (define-syntax inner-recur/foldr
                        #,(if delay?
                              #'(make-inner-recur/foldr/lazy
                                 (list (quote-syntax fold-var) ...)
                                 (quote-syntax delayed-id)
                                 (quote-syntax delayer-id))
                              #'(make-inner-recur/foldr/strict
                                 (list (quote-syntax fold-var) ...))))
                      #,(if result-expr
                            ;; Make sure `fold-var`s in `result-expr` are also delayed, if relevant
                            #`(inner-recur/foldr () [#,result-expr] #,loop-stx)
                            loop-stx)))]
                 [else
                  (with-syntax ([(int-var ...)
                                 (map (lambda (fold-var)
                                        (datum->syntax fold-var
                                                       (string->uninterned-symbol
                                                        (symbol->string (syntax-e fold-var)))
                                                       fold-var
                                                       fold-var))
                                      (syntax->list #'(fold-var ...)))])
                    (quasisyntax/loc #'orig-stx
                      (let ([int-var (let ([fold-var fold-init])
                                       fold-var)]
                            ...)
                        #,(let ([loop-stx
                                 (quasisyntax/loc #'orig-stx
                                   (for/fold/derived/final [orig-stx #,for*?]
                                     ([int-var fold-var] ...)
                                     ([fold-var (make-fold-var 'int-var)] ...)
                                     (values* int-var ...)
                                     . rest))])
                            (if result-expr
                                (quasisyntax/loc #'orig-stx
                                  (let-values ([(fold-var ...) #,loop-stx])
                                    #,result-expr))
                                loop-stx)))))])))]
          [(_ orig-stx . rst)
           (raise-syntax-error #f "bad syntax" #'orig-stx)]))

      (values (make #f #f) (make #t #f) (make #f #t) (make #t #t))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  derived `for' syntax

  (define-for-syntax (split-for-body stx body-stx)
    (let ([lst (syntax->list body-stx)])
      (if lst
          (let loop ([exprs lst] [pre-kw null] [post-kw null])
            (cond
             [(null? exprs)
              (if (null? post-kw)
                  (if (null? pre-kw)
                      (raise-syntax-error #f
                                          "missing body"
                                          stx)
                      (raise-syntax-error #f
                                          (format "missing body form after ~a clause" (syntax-e (cadr pre-kw)))
                                          stx
                                          (cadr pre-kw)))
                  (list (reverse pre-kw) (reverse post-kw)))]
             [(memq (syntax-e (car exprs)) '(#:break #:final))
              (if (pair? (cdr exprs))
                  (loop (cddr exprs) 
                        (append (list* (cadr exprs) (car exprs) post-kw)
                                pre-kw)
                        null)
                  (raise-syntax-error #f
                                      (format "missing expression after ~a" (syntax-e (car exprs)))
                                      stx
                                      (car exprs)))]
             [else
              (loop (cdr exprs) pre-kw (cons (car exprs) post-kw))]))
          (raise-syntax-error #f "bad syntax" stx))))

  (define-for-syntax (for-variant-stx stx derived-id-stx nonnested-id-stx fold-bind-stx wrap rhs-wrap combine combine*)
    (with-syntax ([fold-bind fold-bind-stx])
      (syntax-case stx ()
        ;; When there's a bindings clause...
        [(_ (bind ...) expr1 expr ...)
         (let ([bs (syntax->list #'(bind ...))])
           (define wrap? (and combine*
                              (not (ormap (lambda (b) (eq? '#:splice (syntax-e b))) bs))))
           (with-syntax ([(bind ...)
                          (cond
                            [wrap?
                             ;; wrap each binding clause
                             (let loop ([bs bs])
                               (if (null? bs)
                                   null
                                   (syntax-case (car bs) ()
                                     [[ids rhs]
                                      (or (identifier? #'ids)
                                          (andmap identifier? (or (syntax->list #'ids) '(#f))))
                                      (cons #`[ids #,(rhs-wrap #'rhs)]
                                            (loop (cdr bs)))]
                                     [kw
                                      (memq (syntax-e #'kw) '(#:when #:unless #:break #:final #:do))
                                      (cons (car bs)
                                            (if (null? (cdr bs))
                                                null
                                                (cons (cadr bs) (loop (cddr bs)))))]
                                     [_
                                      ;; a syntax error; let the /derived form
                                      ;; handle it, and no need to wrap any more:
                                      bs])))]
                            [(not (free-identifier=? derived-id-stx nonnested-id-stx))
                             ;; add `#:when #t` after each binding clause to trigger nesting
                             (let loop ([bs bs])
                               (if (null? bs)
                                   null
                                   (syntax-case (car bs) ()
                                     [[_ _]
                                      (list* (car bs)
                                             #'#:when
                                             #'#t
                                             (loop (cdr bs)))]
                                     [kw
                                      (memq (syntax-e #'kw) '(#:when #:unless #:break #:final #:do #:splice))
                                      (cons (car bs)
                                            (if (null? (cdr bs))
                                                null
                                                (cons (cadr bs) (loop (cddr bs)))))]
                                     [_
                                      ;; a syntax error; let the /derived form
                                      ;; handle it, and no need to insert any more:
                                      bs])))]
                            [else bs])]
                         [derived-id (if wrap?
                                         derived-id-stx
                                         nonnested-id-stx)]
                         [((middle-expr ...) (end-expr ...))
                          (split-for-body stx #'(expr1 expr ...))])
             (quasisyntax/loc stx
               #,(wrap (quasisyntax/loc stx
                         (derived-id #,stx fold-bind (bind ...)
                                     middle-expr ...
                                     #,@(let ([e (syntax/loc stx (let () end-expr ...))])
                                          (if (and (not wrap?) combine*)
                                              (combine* e)
                                              (list (combine e))))))))))]
        ;; Let `derived-id-stx' complain about the missing bindings and body expression:
        [(_ . rest)
         #`(#,derived-id-stx #,stx fold-bind . rest)])))
  
  (define-syntax define-syntax-via-derived
    (syntax-rules ()
      [(_ id derived-id nonnested-id fold-bind wrap rhs-wrap combine combine*)
       (define-syntax (id stx)
         (for-variant-stx stx #'derived-id #'nonnested-id  #'fold-bind wrap rhs-wrap combine combine*))]))

  (define-syntax define-for-variants
    (syntax-rules ()
      [(_ (for for*) fold-bind wrap
          ;; used for original mode, which creates expansions for `for/or`, etc.,
          ;; that Typed Racket can recognize:
          rhs-wrap combine
          ;; used for newer mode that doesn't try to wrap binding clauses before
          ;; expansion, so `#:splice` can work, can be #f if `rhs-wrap` doesn't wrap:
          combine*)
       (begin
         (define-syntax-via-derived for for/fold/derived for/fold/derived fold-bind wrap rhs-wrap combine combine*)
         (define-syntax-via-derived for* for*/fold/derived for/fold/derived fold-bind wrap rhs-wrap combine combine*))]))

  (define-syntaxes (for/fold for*/fold for/foldr for*/foldr)
    (let ()
      (define ((make f/f/d-id) stx)
        (syntax-case stx ()
          [(_ . rest)
           (quasisyntax/loc stx
             (#,f/f/d-id #,stx . rest))]))
      (values (make #'for/fold/derived)
              (make #'for*/fold/derived)
              (make #'for/foldr/derived)
              (make #'for*/foldr/derived))))

  (define-for-variants (for for*)
    ()
    (lambda (x) `(,#'begin ,x ,#'(void)))
    (lambda (x) x)
    (lambda (x) `(,#'begin ,x ,#'(values)))
    #f)

  (define-for-variants (for/list for*/list)
    ([fold-var null])
    (lambda (x) `(,#'alt-reverse ,x))
    (lambda (x) x)
    (lambda (x) `(,#'cons ,x ,#'fold-var))
    #f)

  (define (grow-vector vec)
    (define n (vector-length vec))
    (define new-vec (make-vector (* 2 n)))
    (vector-copy! new-vec 0 vec 0 n)
    new-vec)

  (define (shrink-vector vec i)
    (define new-vec (make-vector i))
    (vector-copy! new-vec 0 vec 0 i)
    new-vec)

  (define-for-syntax (for_/vector stx orig-stx for_/vector-stx for_/fold/derived-stx wrap-all?)
    (syntax-case stx ()
      [(_ (for-clause ...) body ...)
       (with-syntax ([orig-stx orig-stx]
                     [for_/fold/derived for_/fold/derived-stx]
                     [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))])
          (syntax/loc stx
            (let-values ([(vec i)
                          (for_/fold/derived
                           orig-stx
                           ([vec (make-vector 16)]
                            [i 0])
                           (for-clause ...) 
                           middle-body ...
                           (let ([new-vec (if (eq? i (unsafe-vector*-length vec))
                                              (grow-vector vec)
                                              vec)])
                             (unsafe-vector*-set! new-vec i (let () last-body ...))
                             (values new-vec (unsafe-fx+ i 1))))])
              (shrink-vector vec i))))]
      [(_ #:length length-expr #:fill fill-expr (for-clause ...) body ...)
       (with-syntax ([orig-stx orig-stx]
                     [(limited-for-clause ...)
                      ;; If `wrap-all?', wrap all binding clauses. Otherwise, wrap
                      ;; only the first and the first after each keyword clause:
                      (let loop ([fcs (syntax->list #'(for-clause ...))] [wrap? #t])
                        (cond
                         [(null? fcs) null]
                         [(keyword? (syntax-e (car fcs)))
                          (if (null? (cdr fcs))
                              fcs
                              (list* (car fcs) (cadr fcs) (loop (cddr fcs) #t)))]
                         [(not wrap?)
                          (cons (car fcs) (loop (cdr fcs) #f))]
                         [else
                          (define fc (car fcs))
                          (define wrapped-fc
                            (syntax-case fc ()
                              [[ids rhs]
                               (or (identifier? #'ids)
                                   (let ([l (syntax->list #'ids)])
                                     (and l (andmap identifier? l))))
                               (syntax/loc fc [ids (stop-after
                                                    rhs
                                                    (lambda x
                                                      (unsafe-fx= i len)))])]
                              [_ fc]))
                          (cons wrapped-fc
                                (loop (cdr fcs) wrap-all?))]))]
                     [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))]
                     [for_/vector for_/vector-stx]
                     [for_/fold/derived for_/fold/derived-stx])
          (syntax/loc stx
            (let ([len length-expr])
              (unless (exact-nonnegative-integer? len)
                (raise-argument-error 'for_/vector "exact-nonnegative-integer?" len))
              (let ([v (make-vector len fill-expr)])
                (unless (zero? len)
                  (for_/fold/derived
                   orig-stx 
                   ([i 0])
                   (limited-for-clause ...)
                   middle-body ...
                   (unsafe-vector*-set! v i (let () last-body ...))
                   (unsafe-fx+ 1 i)))
                v))))]
      [(_ #:length length-expr (for-clause ...) body ...)
       (for_/vector #'(fv #:length length-expr #:fill 0 (for-clause ...) body ...) 
                    orig-stx for_/vector-stx for_/fold/derived-stx wrap-all?)]))

  (define-syntax (for/vector stx)
    (for_/vector stx stx #'for/vector #'for/fold/derived #f))

  (define-syntax (for*/vector stx)
    (for_/vector stx stx #'for*/vector #'for*/fold/derived #t))

  (define-for-syntax (do-for/lists for/fold-id stx)
    (define (do-without-result-clause normalized-stx)
      (with-syntax ([(_ (id ...) bindings expr1 expr ...)
                     normalized-stx])
        (define ids (syntax->list #'(id ...)))
        (for-each (lambda (id)
                    (unless (identifier? id)
                      (raise-syntax-error #f
                                          "not an identifier"
                                          stx
                                          id)))
                  ids)
        (with-syntax ([(id2 ...) (generate-temporaries ids)]
                      [for/fold for/fold-id]
                      [orig-stx stx]
                      [((middle-body ...) (body ...)) (split-for-body stx #'(expr1 expr ...))])
          #'(let-values ([(id ...)
                          (for/fold orig-stx ([id null] ...) bindings
                            middle-body ...
                            (let-values ([(id2 ...) (let () body ...)])
                              (values* (cons id2 id) ...)))])
              (values* (alt-reverse id) ...)))))
    (syntax-case stx ()
      [(_ (id ... #:result result-expr) bindings expr1 expr ...)
        #`(let-values ([(id ...)
                        #,(do-without-result-clause
                           #'(_ (id ...) bindings expr1 expr ...))])
            result-expr)]
      [(_ (id ...) bindings expr1 expr ...)
       (do-without-result-clause stx)]))

  (define-syntax (for/lists stx) (do-for/lists #'for/fold/derived stx))
  (define-syntax (for*/lists stx) (do-for/lists #'for*/fold/derived stx))

  (define-for-variants (for/and for*/and)
    ([result #t])
    (lambda (x) x)
    (lambda (rhs) #`(stop-after #,rhs (lambda x (not result))))
    (lambda (x) x)
    (lambda (x) #`((define result #,x)
                   #:final (not result)
                   result)))

  (define-for-variants (for/or for*/or)
    ([result #f])
    (lambda (x) x)
    (lambda (rhs) #`(stop-after #,rhs (lambda x result)))
    (lambda (x) x)
    (lambda (x) #`((define result #,x)
                   #:final result
                   result)))

  (define-for-variants (for/first for*/first)
    ([val #f] [stop? #f])
    (lambda (x) #`(let-values ([(val _) #,x]) val))
    (lambda (rhs) #`(stop-after #,rhs (lambda x stop?)))
    (lambda (x) #`(values #,x #t))
    (lambda (x) #`(#:final #t
                   (values #,x #t))))

  (define-for-variants (for/last for*/last)
    ([result #f])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x) x)
    #f)

  (define-for-variants (for/sum for*/sum)
    ([result 0])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x) #`(+ result #,x))
    #f)

  (define-for-variants (for/product for*/product)
    ([result 1])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x) #`(* result #,x))
    #f)

  (define-for-variants (for/hash for*/hash)
    ([table #hash()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val)))
    #f)

  (define-for-variants (for/hasheq for*/hasheq)
    ([table #hasheq()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val)))
    #f)

  (define-for-variants (for/hasheqv for*/hasheqv)
    ([table #hasheqv()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val)))
    #f)

  (define-for-variants (for/hashalw for*/hashalw)
    ([table (hashalw)])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val)))
    #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  specific sequences

  (define-for-syntax (generate-for-clause-for-in-range-like
                      id a b step
                      all-fx? check
                      unsafe-fx< unsafe-fx> < >)
    (with-syntax ([id id]
                  [a a]
                  [b b]
                  [step step]
                  [(check ...) check]
                  [unsafe-fx< unsafe-fx<]
                  [unsafe-fx> unsafe-fx>]
                  [< <]
                  [> >])
      (with-syntax ([pos pos-sym]
                    [start start-sym]
                    [end end-sym]
                    [inc inc-sym])
        #`[(id)
           (:do-in
            ;; outer bindings:
            ([(start) a] [(end) b] [(inc) step])
            ;; outer check:
            ;; let `check' report the error:
            (unless-unsafe (check ... start end inc))
            ;; loop bindings:
            ([pos start])
            ;; pos check
            #,(cond [all-fx?
                     ;; Special case, can use unsafe ops:
                     (if ((syntax-e #'step) . >= . 0)
                         #'(unsafe-fx< pos end)
                         #'(unsafe-fx> pos end))]
                    ;; General cases:
                    [(not (number? (syntax-e #'step)))
                     #'(if (step . >= . 0) (< pos end) (> pos end))]
                    [((syntax-e #'step) . >= . 0)
                     #'(< pos end)]
                    [else
                     #'(> pos end)])
            ;; inner bindings
            ([(id) pos])
            ;; pre guard
            #t
            ;; post guard
            #t
            ;; loop args
            ((#,(if all-fx? #'unsafe-fx+ #'+) pos inc)))])))

  (define-sequence-syntax *in-range
    (lambda () #'in-range)
    (lambda (stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ a b step)]
           (generate-for-clause-for-in-range-like
            #'id #'a #'b #'step
            (and (memq (syntax-e #'step) '(1 -1))
                 (fixnum-for-every-system? (syntax-e #'a))
                 (fixnum-for-every-system? (syntax-e #'b)))
            #'(check-range)
            #'unsafe-fx< #'unsafe-fx> #'< #'>)]
          [[(id) (_ a b)] (loop #'[(id) (_ a b 1)])]
          [[(id) (_ b)] (loop #'[(id) (_ 0 b 1)])]
          [_ #f]))))

  (define-sequence-syntax *in-inclusive-range
    (lambda () #'in-inclusive-range)
    (lambda (stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ a b step)]
           (generate-for-clause-for-in-range-like
            #'id #'a #'b #'step
            (and (memq (syntax-e #'step) '(1 -1))
                 (fixnum-for-every-system? (syntax-e #'a))
                 (fixnum-for-every-system? (syntax-e #'b))
                 (fixnum-for-every-system? ((if (eq? (syntax-e #'step) 1) add1 sub1)
                                            (syntax-e #'b))))
            #'(check-range-generic 'in-inclusive-range)
            #'unsafe-fx<= #'unsafe-fx>= #'<= #'>=)]
          [[(id) (_ a b)] (loop #'[(id) (_ a b 1)])]
          [_ #f]))))

  (define-sequence-syntax *in-naturals
    (lambda () #'in-naturals)
    (lambda (stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ start-expr)]
           (with-syntax ([start start-sym]
                         [pos pos-sym])
             #`[(id)
                (:do-in
                 ;; outer bindings:
                 ([(start) start-expr])
                 ;; outer check:
                 ;; let `check-naturals' report the error:
                 (unless-unsafe (check-naturals start))
                 ;; loop bindings:
                 ([pos start])
                 ;; pos check
                 #t
                 ;; inner bindings
                 ([(id) pos])
                 ;; pre guard
                 #t
                 ;; post guard
                 #t
                 ;; loop args
                 ((+ pos 1)))])]
          [[(id) (_)]
           (loop #'[(id) (_ 0)])]
          [_ #f]))))

  (define-sequence-syntax *in-list
    (lambda () #'in-list)
    (lambda (stx)
      (syntax-case stx (list)
        [[(id) (_ (list expr))] #'[(id) (:do-in ([(id) expr]) (void) () #t () #t #f ())]]
        [[(id) (_ lst-expr)]
         (with-syntax ([lst lst-sym]
                       [rest rest-sym])
           #'[(id)
              (:do-in
               ;;outer bindings
               ([(lst) lst-expr])
               ;; outer check
               (unless-unsafe (check-list lst))
               ;; loop bindings
               ([lst lst])
               ;; pos check
               (pair? lst)
               ;; inner bindings
               ([(id) (unsafe-car lst)]
                [(rest) (unsafe-cdr lst)]) ; so `lst` is not necessarily retained during body
               ;; pre guard
               #t
               ;; post guard
               #t
               ;; loop args
               (rest))])]
        [_ #f])))

  (define-sequence-syntax *in-mlist
    (lambda () #'in-mlist)
    (lambda (stx)
      (syntax-case stx (mlist)
        [[(id) (_ (mlist expr))] #'[(id) (:do-in ([(id) expr]) (void) () #t () #t #f ())]]
        [[(id) (_ lst-expr)]
         #'[(id)
            (:do-in
             ;;outer bindings
             ([(lst) lst-expr])
             ;; outer check
             (unless-unsafe (check-mlist lst))
             ;; loop bindings
             ([lst lst])
             ;; pos check
             (not (null? lst))
             ;; inner bindings
             ([(id) (mcar lst)])
             ;; pre guard
             #t
             ;; post guard
             #t
             ;; loop args
             ((mcdr lst)))]]
        [_ #f])))

  (define-sequence-syntax *in-stream
    (lambda () #'in-stream)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ lst-expr)]
         (with-syntax ([lst lst-sym]
                       [rest rest-sym])
           #'[(id)
              (:do-in
               ;;outer bindings
               ([(lst) lst-expr])
               ;; outer check
               (unless-unsafe (check-stream lst))
               ;; loop bindings
               ([lst lst])
               ;; pos check
               (unsafe-stream-not-empty? lst)
               ;; inner bindings
               ([(id) (unsafe-stream-first lst)]
                [(rest) (unsafe-stream-rest lst)])  ; so `lst` is not necessarily retained during body
               ;; pre guard
               #t
               ;; post guard
               #t
               ;; loop args
               (rest))])]
        [_ #f])))

  (define-sequence-syntax *in-indexed
    (lambda () #'in-indexed)
    (lambda (stx)
      (syntax-case stx ()
        [[(id1 id2) (_ gen-expr)]
         #'[(id1 id2) (in-parallel gen-expr (*in-naturals))]]
        [_ #f])))

  (define-sequence-syntax *in-value
    (lambda () #'in-value)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ expr)]
         #'[(id) (:do-in ([(id*) expr]) (void) () #t ([(id) id*]) #t #f ())]]
        [_ #f])))

  (define-sequence-syntax *in-producer
    (lambda () #'in-producer)
    (lambda (stx)
      (syntax-case stx ()
        ;; cheap & simple stop-less and arg-less version
        [[(id ...) (_ producer)]
         #'[(id ...)
            (:do-in ([(producer*) producer]) (void) () #t ([(id ...) (producer*)])
                    #t #t ())]]
        ;; full version
        [[(id ...) (_ producer stop more ...)]
         (with-syntax ([(more* ...) (generate-temporaries #'(more ...))])
           #`[(id ...)
              (:do-in
               ;; outer bindings
               ([(producer*) producer]
                [(more*) more] ...
                [(stop?)
                 (let ([s stop])
                   (cond [(procedure? s) s]
                         [else #,(if (= 1 (length (syntax->list #'(id ...))))
                                     #'(lambda (x) (eq? x s))
                                     #'(raise-arguments-error
                                        'in-producer
                                        "stop condition for multiple values must be a predicate"
                                        "stop condition" s))]))])
               ;; outer check
               (void)
               ;; loop bindings
               ()
               ;; pos check
               #t
               ;; inner bindings
               ([(id ...) (producer* more* ...)])
               ;; pre guard
               (not (stop? id ...))
               ;; post guard
               #t
               ;; loop args
               ())])]
        [_ #f])))

  ;; Some iterators that are implemented using `*in-producer' (note: do not use
  ;; `in-producer', since in this module it is the procedure version).

  (define-sequence-syntax *in-port
    (lambda () #'in-port)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_)]   #'[(id) (*in-port read (current-input-port))]]
        [[(id) (_ r)] #'[(id) (*in-port r (current-input-port))]]
        [[(id) (_ r p)]
         #'[(id) (*in-producer
                  (let ([r* r] [p* p])
                    (unless-unsafe (check-in-port r* p*))
                    (lambda () (r* p*)))
                  eof)]]
        [_ #f])))

  (define-sequence-syntax *in-lines
    (lambda () #'in-lines)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_)]   #'[(id) (*in-lines (current-input-port) 'any)]]
        [[(id) (_ p)] #'[(id) (*in-lines p 'any)]]
        [[(id) (_ p mode)]
         #'[(id) (*in-producer
                  (let ([p* p] [mode* mode])
                    (unless-unsafe (check-in-lines p* mode*))
                    (lambda () (read-line p* mode*)))
                  eof)]]
        [_ #f])))

  (define-sequence-syntax *in-bytes-lines
    (lambda () #'in-bytes-lines)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_)]   #'[(id) (*in-bytes-lines (current-input-port) 'any)]]
        [[(id) (_ p)] #'[(id) (*in-bytes-lines p 'any)]]
        [[(id) (_ p mode)]
         #'[(id) (*in-producer
                  (let ([p* p] [mode* mode])
                    (unless-unsafe (check-in-bytes-lines p* mode*))
                    (lambda () (read-bytes-line p* mode*)))
                  eof)]]
        [_ #f])))

  (define-sequence-syntax *in-input-port-bytes
    (lambda () #'in-input-port-bytes)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ p)]
         #'[(id) (*in-producer
                  (let ([p* p])
                    (unless-unsafe (check-in-input-port-bytes p*))
                    (lambda () (read-byte p*)))
                  eof)]]
        [_ #f])))

  (define-sequence-syntax *in-input-port-chars
    (lambda () #'in-input-port-chars)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ p)]
         #'[(id) (*in-producer
                  (let ([p* p])
                    (unless-unsafe (check-in-input-port-chars p*))
                    (lambda () (read-char p*)))
                  eof)]]
        [_ #f])))

  (define (dir-list full-d d acc)
    (for/fold ([acc acc]) ([f (in-list (reverse (sort (directory-list full-d) path<?)))])
	      (cons (build-path d f) acc)))

  (define (next-body l d init-dir use-dir?)
    (let ([full-d (path->complete-path d init-dir)])
      (if (and (directory-exists? full-d)
               (use-dir? full-d))
	  (dir-list full-d d (cdr l))
	  (cdr l))))

  (define (initial-state orig-dir init-dir)
    (if orig-dir
	(dir-list (path->complete-path orig-dir init-dir)
		  orig-dir null)
	(sort (directory-list init-dir) path<?)))

  (define in-directory
    (case-lambda 
     [() (in-directory #f (lambda (d) #t))]
     [(orig-dir) (in-directory orig-dir (lambda (d) #t))]
     [(orig-dir use-dir?)
      (define init-dir (current-directory))
      ;; current state of the sequence is a list of paths to produce; when
      ;; incrementing past a directory, add the directory's immediate
      ;; content to the front of the list:
      (define (next l)
	(define d (car l))
	(next-body l d init-dir use-dir?))
      (make-do-sequence
       (lambda ()
	 (values
	  car
	  next
	  (initial-state orig-dir init-dir)
	  pair?
	  #f
	  #f)))]))
     
  (define-sequence-syntax *in-directory
    (λ () #'in-directory)
    (λ (stx)
       (syntax-case stx ()
	 [((d) (_)) #'[(d) (in-directory #f)]]
	 [((d) (_ dir)) #'[(d) (in-directory dir (lambda (d) #t))]]
	 [((d) (_ dir use-dir?-expr))
	  #'[(d) 
	     (:do-in
	      ([(orig-dir) (or dir #f)]
               [(init-dir) (current-directory)]
               [(use-dir?) use-dir?-expr])
	      (void)
	      ([l (initial-state orig-dir init-dir)])
	      (pair? l)
	      ([(d) (car l)])
	      #true
	      #true
	      [(next-body l (car l) init-dir use-dir?)])]]
	 [_ #f])))

  )
