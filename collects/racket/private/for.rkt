(module for '#%kernel

  (#%require "more-scheme.rkt"
             "misc.rkt"
             "define.rkt"
             "letstx-scheme.rkt"
             '#%unsafe
             (for-syntax '#%kernel
                         "stx.rkt"
                         "qqstx.rkt"
                         "define.rkt"
                         "small-scheme.rkt"
                         "stxcase-scheme.rkt"))

  (#%provide for/fold for*/fold
             for for*
             for/list for*/list
             for/lists for*/lists
             for/and for*/and
             for/or for*/or
             for/first for*/first
             for/last for*/last
             for/hash for*/hash
             for/hasheq for*/hasheq
             for/hasheqv for*/hasheqv

             for/fold/derived for*/fold/derived

             (rename *in-range in-range)
             (rename *in-naturals in-naturals)
             (rename *in-list in-list)
             (rename *in-vector in-vector)
             (rename *in-string in-string)
             (rename *in-bytes in-bytes)
             (rename *in-input-port-bytes in-input-port-bytes)
             (rename *in-input-port-chars in-input-port-chars)
             (rename *in-port in-port)
             (rename *in-lines in-lines)
             (rename *in-bytes-lines in-bytes-lines)
             in-hash
             in-hash-keys
             in-hash-values
             in-hash-pairs
             in-directory

             in-sequences
             in-cycle
             in-parallel
             stop-before
             stop-after
             (rename *in-producer in-producer)
             (rename *in-indexed in-indexed)
             (rename *in-value in-value)

             sequence?
             sequence-generate
             prop:sequence

             define-sequence-syntax
             make-do-sequence
             :do-in)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; sequence transformers:

  (begin-for-syntax
    (define-values (struct:sequence-transformer
                    make-sequence-transformer
                    sequence-transformer?
                    sequence-transformer-ref
                    sequence-transformer-set!)
      (make-struct-type 'sequence-transformer #f
                        3 0 #f
                        null (current-inspector)
                        0))

    (define (create-sequence-transformer proc1 proc2 cert)
      (unless (and (procedure? proc1)
                   (or (procedure-arity-includes? proc1 1)
                       (procedure-arity-includes? proc1 0)))
        (raise-type-error 'define-sequence-syntax
                          "procedure (arity 0 or 1)"
                          0
                          proc1 proc2))
      (unless (and (procedure? proc2)
                   (procedure-arity-includes? proc2 1))
        (raise-type-error 'define-sequence-syntax
                          "procedure (arity 1)"
                          1
                          proc1 proc2))
      (make-sequence-transformer
       (if (procedure-arity-includes? proc1 0)
         (lambda (stx)
           (if (identifier? stx)
             (proc1)
             (datum->syntax stx
                            #`(#,(proc1) . #,(cdr (syntax-e stx)))
                            stx
                            stx)))
         proc1)
       proc2
       cert))

    (define cert-key (gensym 'for-cert))

    (define (certify-clause src-stx clause certifier introducer)
      ;; This is slightly painful. The expansion into `:do-in' involves a lot
      ;; of pieces that are no treated as sub-expressions. We have to push the
      ;; certificates down to all the relevant identifiers and expressions:
      (define (recert s) (syntax-recertify s src-stx (current-inspector) cert-key))
      (define (cert s) (certifier (recert s) cert-key introducer))
      (define (map-cert s) (map cert (syntax->list s)))

      (syntax-case clause (:do-in)
        [[(id ...) (:do-in ([(outer-id ...) outer-expr] ...)
                            outer-check
                            ([loop-id loop-expr] ...)
                            pos-guard
                            ([(inner-id ...) inner-expr] ...)
                            pre-guard
                            post-guard
                            (loop-arg ...))]
         (with-syntax ([((outer-id ...) ...)
                        (map map-cert
                             (syntax->list #'((outer-id ...) ...)))]
                       [(outer-expr ...) (map-cert #'(outer-expr ...))]
                       [outer-check (cert #'outer-check)]
                       [(loop-expr ...) (map-cert #'(loop-expr ...))]
                       [pos-guard (cert #'pos-guard)]
                       [((inner-id ...) ...)
                        (map map-cert (syntax->list #'((inner-id ...) ...)))]
                       [pre-guard (cert #'pre-guard)]
                       [post-guard (cert #'post-guard)]
                       [(loop-arg ...) (map-cert #'(loop-arg ...))])
           #`[(id ...) (:do-in ([(outer-id ...) outer-expr] ...)
                                outer-check
                                ([loop-id loop-expr] ...)
                                pos-guard
                                ([(inner-id ...) inner-expr] ...)
                                pre-guard
                                post-guard
                                (loop-arg ...))])]
        [[(id ...) rhs]
         #`[(id ...) #,(cert #'rhs)]]
        [_
         ;; ill-formed clause...
         clause]))

    (define (expand-clause orig-stx clause)
      (let eloop ([use-transformer? #t])
        (syntax-case clause (values in-parallel stop-before stop-after :do-in)
          [[(id ...) rhs]
           (let ([ids (syntax->list #'(id ...))])
             (for-each (lambda (id)
                         (unless (identifier? id)
                           (raise-syntax-error
                            #f
                            "expected an identifier to bind"
                            orig-stx
                            id)))
                       ids)
             (let ([dup (check-duplicate-identifier (syntax->list #'(id ...)))])
               (when dup
                 (raise-syntax-error #f
                   "duplicate identifier as sequence binding" orig-stx dup)))
             #f)
           'just-checking]
          [[(id ...) (form . rest)]
           (and use-transformer?
                (identifier? #'form)
                (sequence-transformer? (syntax-local-value #'form (lambda () #f))))
           (let ([m (syntax-local-value #'form)])
             (let ([xformer (sequence-transformer-ref m 1)]
                   [introducer (make-syntax-introducer)]
                   [certifier (sequence-transformer-ref m 2)])
               (let ([xformed (xformer (introducer (syntax-local-introduce clause)))])
                 (if xformed
                     (expand-clause orig-stx (certify-clause (syntax-case clause ()
                                                               [(_ rhs) #'rhs])
                                                             (syntax-local-introduce (introducer xformed))
                                                             certifier
                                                             introducer))
                     (eloop #f)))))]
          [[(id ...) (:do-in . body)]
           (syntax-case #'body ()
             [(([(outer-id ...) outer-rhs] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-rhs] ...)
               pre-guard
               post-guard
               (loop-arg ...)) #'body]
             [else (raise-syntax-error #f "bad :do-in clause" orig-stx clause)])]
          [[(id) (values rhs)]
           (expand-clause orig-stx #'[(id) rhs])]
          [[(id ...) (in-parallel rhs ...)]
           (and (= (length (syntax->list #'(id ...)))
                   (length (syntax->list #'(rhs ...)))))
           ;; flatten in-parallel iterations:
           (with-syntax ([(((outer-binding ...)
                            outer-check
                            (loop-binding ...)
                            pos-guard
                            (inner-binding ...)
                            pre-guard
                            post-guard
                            (loop-arg ...)) ...)
                          (map (lambda (id rhs)
                                 (expand-clause orig-stx #`[(#,id) #,rhs]))
                               (syntax->list #'(id ...))
                               (syntax->list #'(rhs ...)))])
             #`((outer-binding ... ...)
                (and outer-check ...)
                (loop-binding ... ...)
                (and pos-guard ...)
                (inner-binding ... ...)
                (and pre-guard ...)
                (and post-guard ...)
                (loop-arg ... ...)))]
          [[(id ...) (stop-before gen-expr pred)]
           (with-syntax ([((outer-binding ...)
                           outer-check
                           (loop-binding ...)
                           pos-guard
                           (inner-binding ...)
                           pre-guard
                           post-guard
                           (loop-arg ...))
                          (expand-clause orig-stx #`[(id ...) gen-expr])])
             #`((outer-binding ...)
                outer-check
                (loop-binding ...)
                pos-guard
                (inner-binding ...)
                (and pre-guard (not (pred id ...)))
                post-guard
                (loop-arg ...)))]
          [[(id ...) (stop-after gen-expr pred)]
           (with-syntax ([((outer-binding ...)
                           outer-check
                           (loop-binding ...)
                           pos-guard
                           (inner-binding ...)
                           pre-guard
                           post-guard
                           (loop-arg ...))
                          (expand-clause orig-stx #`[(id ...) gen-expr])])
             #`((outer-binding ...)
                outer-check
                (loop-binding ...)
                pos-guard
                (inner-binding ...)
                pre-guard
                (and post-guard (not (pred id ...)))
                (loop-arg ...)))]
          [[(id ...) rhs]
           (let ([introducer (make-syntax-introducer)])
             (with-syntax ([[(id ...) rhs] (introducer (syntax-local-introduce clause))])
               (syntax-local-introduce
                (introducer
                 #`(([(pos->vals pos-next init pos-cont? val-cont? all-cont?)
                      (#,((syntax-local-certifier #f) #'make-sequence) '(id ...) rhs)])
                    (void)
                    ([pos init])
                    (pos-cont? pos)
                    ([(id ...) (pos->vals pos)])
                    (val-cont? id ...)
                    (all-cont? pos id ...)
                    ((pos-next pos)))))))]
          [_
           (raise-syntax-error #f
             "bad sequence binding clause" orig-stx clause)]))))

  (define-syntax (:do-in stx)
    (raise-syntax-error #f
      "illegal outside of a loop or comprehension binding" stx))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  sequences

  (define-values (struct:do-sequence
                  make-do-sequence
                  do-sequence?
                  do-sequence-ref
                  do-sequence-set!)
    (make-struct-type 'sequence #f 1 0 #f))

  (define-values (prop:sequence :sequence? :sequence-ref)
    (make-struct-type-property
     'sequence
     (lambda (v sinfo)
       (unless (and (procedure? v) (procedure-arity-includes? v 1))
         (raise-type-error 'sequence-property-guard "procedure (arity 1)" v))
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
                                      clause-transformer-expr
                                      (syntax-local-certifier #f)))]))

  (define (sequence? v)
    (or (do-sequence? v)
        (list? v)
        (vector? v)
        (string? v)
        (bytes? v)
        (input-port? v)
        (hash? v)
        (and (:sequence? v) (not (struct-type? v)))))

  (define (make-sequence who v)
    (cond
      [(do-sequence? v) ((do-sequence-ref v 0))]
      [(list? v) (:list-gen v)]
      [(vector? v) (:vector-gen v 0 (vector-length v) 1)]
      [(string? v) (:string-gen v 0 (string-length v) 1)]
      [(bytes? v) (:bytes-gen v 0 (bytes-length v) 1)]
      [(input-port? v) (:input-port-gen v)]
      [(hash? v) (:hash-key+val-gen v)]
      [(:sequence? v) (make-sequence who ((:sequence-ref v) v))]
      [else (raise
             (exn:fail:contract
              (format "for: expected a sequence for ~a, got something else: ~v"
                      (if (= 1 (length who))
                          (car who)
                          who)
                      v)
              (current-continuation-marks)))]))

  (define in-range
    (case-lambda
      [(b) (in-range 0 b 1)]
      [(a b) (in-range a b 1)]
      [(a b step)
       (unless (real? a) (raise-type-error 'in-range "real-number" a))
       (unless (real? b) (raise-type-error 'in-range "real-number" b))
       (unless (real? step) (raise-type-error 'in-range "real-number" step))
       (make-do-sequence (lambda ()
                                (values
                                 (lambda (x) x)
                                 (lambda (x) (+ x step))
                                 a
                                 (if (step . >= . 0)
                                     (lambda (x) (< x b))
                                     (lambda (x) (> x b)))
                                 void
                                 void)))]))

  (define in-naturals
    (case-lambda
     [() (in-naturals 0)]
     [(n)
      (unless (and (integer? n)
                   (exact? n)
                   (n . >= . 0))
        (raise-type-error 'in-naturals
                          "exact non-negative integer"
                          n))
      (make-do-sequence (lambda () (values values add1 n void void void)))]))

  (define (in-list l)
    ;; (unless (list? l) (raise-type-error 'in-list "list" l))
    (make-do-sequence (lambda () (:list-gen l))))

  (define (:list-gen l)
    (values car cdr l pair? void void))

  (define (check-ranges who start stop step)
    (unless (exact-nonnegative-integer? start) (raise-type-error who "exact non-negative integer" start))
    (unless (exact-nonnegative-integer? stop) (raise-type-error who "exact non-negative integer or #f" stop))
    (unless (and (exact-integer? step) (not (zero? step)))
      (raise-type-error who "exact non-zero integer" step))
    (when (and (< start stop) (< step 0))
      (raise-mismatch-error who (format "start: ~a less than stop: ~a but given negative step: "
                                        start stop)
                            step))
    (when (and (< stop start) (> step 0))
      (raise-mismatch-error who (format "start: ~a more than stop: ~a but given positive step: "
                                        start stop)
                            step)))

  (define in-vector
    (case-lambda
     [(v) (in-vector v 0 #f 1)]
     [(v start) (in-vector v start #f 1)]
     [(v start stop) (in-vector v start stop 1)]
     [(v start stop step)
      (unless (vector? v) (raise-type-error 'in-vector "vector" v))
      (let ([stop (or stop (vector-length v))])
        (check-ranges 'in-vector start stop step)
        (make-do-sequence (lambda () (:vector-gen v start stop step))))]))

  (define (:vector-gen v start stop step)
    (values
     ;; pos->element
     (lambda (i) (unsafe-vector-ref v i))
     ;; next-pos
     ;; Minor optimisation.  I assume add1 is faster than \x.x+1
     (if (= step 1) add1 (lambda (i) (+ i step)))
     ;; initial pos
     start
     ;; continue?
     (if (> step 0)
         (lambda (i) (< i stop))
         (lambda (i) (> i stop)))
     void
     void))

  (define in-string
    (case-lambda
     [(l) (in-string l 0 #f 1)]
     [(l start) (in-string l start #f 1)]
     [(l start stop) (in-string l start stop 1)]
     [(l start stop step)
      (unless (string? l) (raise-type-error 'in-string "string" l))
      (let ([stop (or stop (string-length l))])
        (check-ranges 'in-string start stop step)
        (make-do-sequence (lambda () (:string-gen l start stop step))))]))

  (define (:string-gen v start stop step)
    (values (lambda (i) (string-ref v i))
            (if (= step 1) add1 (lambda (x) (+ x step)))
            start
            (lambda (i) (< i stop))
            void
            void))

  (define in-bytes
    (case-lambda
     [(l) (in-bytes l 0 #f 1)]
     [(l start) (in-bytes l start #f 1)]
     [(l start stop) (in-bytes l start stop 1)]
     [(l start stop step)
      (unless (bytes? l) (raise-type-error 'in-bytes "bytes" l))
      (let ([stop (or stop (bytes-length l))])
        (check-ranges 'in-bytes start stop step)
        (make-do-sequence (lambda () (:bytes-gen l start stop step))))]))

  (define (:bytes-gen v start stop step)
    (values (lambda (i) (bytes-ref v i))
            (if (= step 1) add1 (lambda (x) (+ x step)))
            start
            (lambda (i) (< i stop))
            void
            void))

  (define (in-input-port-bytes p)
    (unless (input-port? p)
      (raise-type-error 'in-input-port-bytes "input-port" p))
    (make-do-sequence (lambda () (:input-port-gen p))))

  (define (:input-port-gen p)
    (values read-byte values p void
            (lambda (x) (not (eof-object? x)))
            void))

  (define (in-input-port-chars p)
    (unless (input-port? p)
      (raise-type-error 'in-input-port-chars "input-port" p))
    (in-producer (lambda () (read-char p)) eof))

  (define in-port
    (case-lambda
      [()  (in-port read (current-input-port))]
      [(r) (in-port r (current-input-port))]
      [(r p)
       (unless (and (procedure? r) (procedure-arity-includes? r 1))
         (raise-type-error 'in-port "procedure (arity 1)" r))
       (unless (input-port? p) (raise-type-error 'in-port "input-port" p))
       (in-producer (lambda () (r p)) eof)]))

  (define in-lines
    (case-lambda
      [()  (in-lines (current-input-port) 'any)]
      [(p) (in-lines p 'any)]
      [(p mode)
       (unless (input-port? p) (raise-type-error 'in-lines "input-port" p))
       (unless (memq mode '(linefeed return return-linefeed any any-one))
         (raise-type-error
          'in-lines
          "'linefeed, 'return, 'return-linefeed, 'any, or 'any-one"
          mode))
       (in-producer (lambda () (read-line p mode)) eof)]))
  
  (define in-bytes-lines
    (case-lambda
      [()  (in-bytes-lines (current-input-port) 'any)]
      [(p) (in-bytes-lines p 'any)]
      [(p mode)
       (unless (input-port? p) (raise-type-error 'in-bytes-lines "input-port" p))
       (unless (memq mode '(linefeed return return-linefeed any any-one))
         (raise-type-error
          'in-bytes-lines
          "'linefeed, 'return, 'return-linefeed, 'any, or 'any-one"
          mode))
       (in-producer (lambda () (read-bytes-line p mode)) eof)]))

  (define (in-hash ht)
    (unless (hash? ht) (raise-type-error 'in-hash "hash" ht))
    (make-do-sequence (lambda () (:hash-key+val-gen ht))))

  (define (:hash-key+val-gen ht)
    (:hash-gen ht (lambda (ht pos)
                    (values (hash-iterate-key ht pos)
                            (hash-iterate-value ht pos)))))

  (define (in-hash-keys ht)
    (unless (hash? ht) (raise-type-error 'in-hash-keys "hash" ht))
    (make-do-sequence (lambda () (:hash-gen ht hash-iterate-key))))
  (define (in-hash-values ht)
    (unless (hash? ht) (raise-type-error 'in-hash-values "hash" ht))
    (make-do-sequence (lambda () (:hash-gen ht hash-iterate-value))))
  (define (in-hash-pairs ht)
    (unless (hash? ht) (raise-type-error 'in-hash-values "hash" ht))
    (make-do-sequence (lambda ()
                        (:hash-gen ht (lambda (ht pos)
                                        (cons (hash-iterate-key ht pos)
                                              (hash-iterate-value ht pos)))))))

  (define (:hash-gen ht sel)
    (values (lambda (pos) (sel ht pos))
            (lambda (pos) (hash-iterate-next ht pos))
            (hash-iterate-first ht)
            (lambda (pos) pos) ; #f position means stop
            void
            void))

  (define (stop-before g pred)
    (unless (sequence? g) (raise-type-error 'stop-before "sequence" g))
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (raise-type-error 'stop-before "procedure (arity 1)" pred))
    (make-do-sequence (lambda ()
                             (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                           (make-sequence #f g)])
                               (values pos->val
                                       pos-next
                                       init
                                       pos-cont?
                                       (case-lambda
                                         [(val) (and (pre-cont? val)
                                                     (not (pred val)))]
                                         [vals (and (apply pre-cont? vals)
                                                    (not (apply pred vals)))])
                                       post-cont?)))))

  (define (stop-after g pred)
    (unless (sequence? g) (raise-type-error 'stop-after "sequence" g))
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (raise-type-error 'stop-after "procedure (arity 1)" pred))
    (make-do-sequence (lambda ()
                             (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                           (make-sequence #f g)])
                               (values pos->val
                                       pos-next
                                       init
                                       pos-cont?
                                       pre-cont?
                                       (case-lambda
                                         [(pos val) (and (post-cont? pos val)
                                                         (not (pred val)))]
                                         [(pos . vals) (and (apply pos-cont? pos vals)
                                                            (not (apply pred vals)))]))))))

  (define (in-indexed g)
    (unless (sequence? g) (raise-type-error 'in-indexed "sequence" g))
    (make-do-sequence (lambda ()
                             (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                           (make-sequence #f g)])
                               (values (lambda (pos) (values (pos->val (car pos)) (cdr pos)))
                                       (lambda (pos) (cons (pos-next (car pos)) (add1 (cdr pos))))
                                       (cons init 0)
                                       (lambda (pos) (pos-cont? (car pos)))
                                       (lambda (val idx) (pre-cont? val))
                                       (lambda (pos val idx) (post-cont? pos val)))))))

  (define (in-value v)
    (make-do-sequence (lambda ()
                        (values (lambda (pos) v)
                                (lambda (pos) #f)
                                #t
                                (lambda (pos) pos)
                                void
                                void))))

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
               void
               void))))

  (define (check-sequences who sequences)
    (for-each (lambda (g)
                (unless (sequence? g) (raise-type-error who "sequence" g)))
              sequences))

  (define (in-sequences sequence . sequences)
    (let ([all (cons sequence sequences)])
      (check-sequences 'in-sequences all)
      (if (null? sequences) sequence (append-sequences all #f))))
  (define (in-cycle sequence . sequences)
    (let ([all (cons sequence sequences)])
      (check-sequences 'in-cycle all)
      (append-sequences all #t)))

  (define (in-parallel . sequences)
    (check-sequences 'in-parallel sequences)
    (if (= 1 (length sequences))
        (car sequences)
        (make-do-sequence
         (lambda ()
           (let-values ([(pos->vals pos-nexts inits pos-cont?s pre-cont?s post-cont?s)
                         (for/lists (p->v p-s i ps? pr? po?) ([g sequences])
                           (make-sequence #f g))])
             (values
              (lambda (poses) (apply values (map (lambda (pos->val pos) (pos->val pos))
                                                 pos->vals
                                                 poses)))
              (lambda (poses) (map (lambda (pos-next pos) (pos-next pos))
                                   pos-nexts
                                   poses))
              inits
              (lambda (poses) (andmap (lambda (pos-cont? pos) (pos-cont? pos))
                                      pos-cont?s
                                      poses))
              (lambda vals (andmap (lambda (pre-cont? val) (pre-cont? val))
                                   pre-cont?s
                                   vals))
              (lambda (poses . vals) (andmap (lambda (post-cont? pos val) (post-cont? pos val))
                                             post-cont?s
                                             poses
                                             vals))))))))

  (define (in-producer producer stop . more)
    (make-do-sequence
     (lambda ()
       (values (if (null? more)
                 (lambda (_) (producer))
                 (lambda (_) (apply producer more)))
               void
               (void)
               void
               (if (procedure? stop)
                 (if (equal? 1 (procedure-arity stop))
                   (lambda (x) (not (stop x)))
                   (lambda xs (not (apply stop xs))))
                 (lambda (x) (not (eq? x stop))))
               void))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  running sequences outside of a loop:

  (define (sequence-generate g)
    (unless (sequence? g)
      (raise-type-error 'sequence-generate "sequence" g))
    (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                  (make-sequence #f g)])
      (let ([pos init])
        (letrec ([more? #f]
                 [prep-val! #f]
                 [next #f])
          (letrec ([no-more (lambda ()
                              (error "sequence has no more values"))]
                   [init-more?
                    (lambda () (prep-val!) (more?))]
                   [init-next
                    (lambda () (prep-val!) (next))]
                   [init-prep-val!
                    (lambda ()
                      (if (pos-cont? pos)
                          (call-with-values
                           (lambda () (pos->val pos))
                           (lambda vals
                             (if (apply pre-cont? vals)
                                 (begin
                                   (set! more? (lambda () #t))
                                   (set! next
                                         (lambda ()
                                           (let ([v vals])
                                             (set! prep-val!
                                                   (lambda ()
                                                     (if (apply post-cont? pos vals)
                                                         (begin
                                                           (set! pos (pos-next pos))
                                                           (set! prep-val! init-prep-val!)
                                                           (prep-val!))
                                                         (begin
                                                           (set! more? (lambda () #f))
                                                           (set! next no-more)))))
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  core `for/fold' syntax

  (define-syntax values*
    (syntax-rules ()
      [(_ x) x]
      [(_ x ...) (values x ...)]))

  (define-syntax (for/foldX/derived stx)
    (syntax-case stx ()
      ;; Done case (no more clauses, and no generated clauses to emit):
      [(_ [orig-stx nested? emit? ()] ([fold-var fold-init] ...) ()
          expr1 expr ...)
       #`(let ([fold-var fold-init] ...) (let () expr1 expr ...))]
      ;; Switch-to-emit case (no more clauses to generate):
      [(_ [orig-stx nested? #f binds] ([fold-var fold-init] ...) () . body)
       #`(for/foldX/derived [orig-stx nested? #t binds]
                            ([fold-var fold-init] ...) () . body)]
      ;; Emit case:
      [(_ [orig-stx nested? #t binds] ([fold-var fold-init] ...) rest expr1 . body)
       (with-syntax ([(([outer-binding ...]
                        outer-check
                        [loop-binding ...]
                        pos-guard
                        [inner-binding ...]
                        pre-guard
                        post-guard
                        [loop-arg ...]) ...) (reverse (syntax->list #'binds))])
         #`(let-values (outer-binding ... ...)
             outer-check ...
             #,(syntax/loc #'orig-stx
                 (let for-loop ([fold-var fold-init] ...
                                loop-binding ... ...)
                   (if (and pos-guard ...)
                       (let-values (inner-binding ... ...)
                         (if (and pre-guard ...)
                             (let-values ([(fold-var ...)
                                           (for/foldX/derived [orig-stx nested? #f ()] ([fold-var fold-var] ...) rest expr1 . body)])
                               (if (and post-guard ...)
                                   (for-loop fold-var ... loop-arg ... ...)
                                   (values* fold-var ...)))
                             (values* fold-var ...)))
                       (values* fold-var ...))))))]
      ;; Bad body cases:
      [(_ [orig-stx . _] fold-bind ())
       (raise-syntax-error
        #f "missing body expression after sequence bindings" #'orig-stx)]
      [(_ [orig-stx . _] fold-bind () . rest)
       (raise-syntax-error
        #f "bad syntax (illegal use of `.') after sequence bindings" #'orig-stx)]
      ;; Guard case, no pending emits:
      [(_ [orig-stx nested? #f ()] ([fold-var fold-init] ...) (#:when expr . rest) . body)
       #'(let ([fold-var fold-init] ...)
           (if expr
             (for/foldX/derived [orig-stx nested? #f ()]
                                ([fold-var fold-var] ...) rest . body)
             (values* fold-var ...)))]
      ;; Guard case, pending emits need to be flushed first
      [(frm [orig-stx nested? #f binds] ([fold-var fold-init] ...)
            (#:when expr . rest) . body)
       #'(frm [orig-stx nested? #t binds] ([fold-var fold-init] ...)
              (#:when expr . rest) . body)]
      ;; Convert single-value form to multi-value form:
      [(_ [orig-stx nested? #f binds] fold-bind ([id rhs] . rest) . body)
       (identifier? #'id)
       #'(for/foldX/derived [orig-stx nested? #f binds] fold-bind
                            ([(id) rhs] . rest) . body)]
      ;; If we get here in single-value mode, then it's a bad clause:
      [(_ [orig-stx #f #f nested? #f binds] fold-bind (clause . rest) . body)
       (raise-syntax-error
        #f "bad sequence binding clause" #'orig-stx #'clause)]
      ;; Expand one multi-value clause, and push it into the results to emit:
      [(frm [orig-stx nested? #f binds] ([fold-var fold-init] ...)
            (clause . rest) . body)
       (with-syntax ([bind (expand-clause #'orig-stx #'clause)])
         #`(frm [orig-stx nested? nested? (bind . binds)]
                ([fold-var fold-init] ...) rest . body))]
      [(_ [orig-stx . _] . _)
       (raise-syntax-error #f "bad syntax" #'orig-stx)]))

  (define-syntax for/fold/derived
    (syntax-rules ()
      [(_ orig-stx . rest)
       (for/foldX/derived [orig-stx #f #f ()] . rest)]))

  (define-syntax for*/fold/derived
    (syntax-rules ()
      [(_ orig-stx . rest)
       (for/foldX/derived [orig-stx #t #f ()] . rest)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  derived `for' syntax

  (define-for-syntax (for-variant-stx stx derived-id-stx fold-bind-stx wrap rhs-wrap combine)
    (with-syntax ([derived-id derived-id-stx]
                  [fold-bind fold-bind-stx])
      (syntax-case stx ()
        ;; When there's a bindings clause...
        [(_ (bind ...) expr1 expr ...)
         (with-syntax ([(bind ...) (let loop ([bs (syntax->list #'(bind ...))])
                                     (if (null? bs)
                                         null
                                         (syntax-case (car bs) ()
                                           [[ids rhs]
                                            (or (identifier? #'ids)
                                                (andmap identifier? (or (syntax->list #'ids) '(#f))))
                                            (cons #`[ids #,(rhs-wrap #'rhs)]
                                                  (loop (cdr bs)))]
                                           [#:when (cons (car bs)
                                                          (if (null? (cdr bs))
                                                              null
                                                              (cons (cadr bs) (loop (cddr bs)))))]
                                           [_
                                            ;; a syntax error; let the /derived form handle it, and
                                            ;; no need to wrap any more:
                                            bs])))])
           (quasisyntax/loc stx
             #,(wrap (quasisyntax/loc stx
                       (derived-id #,stx fold-bind (bind ...) #,(combine #'(let () expr1 expr ...)))))))]
        ;; Let `derived-id' complain about the missing bindings and body expression:
        [(_ . rest)
         #`(derived-id #,stx fold-bind . rest)])))

  (define-syntax define-syntax-via-derived
    (syntax-rules ()
      [(_ id derived-id fold-bind wrap rhs-wrap combine)
       (define-syntax (id stx)
         (for-variant-stx stx #'derived-id #'fold-bind wrap rhs-wrap combine))]))

  (define-syntax define-for-variants
    (syntax-rules ()
      [(_ (for for*) fold-bind wrap rhs-wrap combine)
       (begin
         (define-syntax-via-derived for for/fold/derived fold-bind wrap rhs-wrap combine)
         (define-syntax-via-derived for* for*/fold/derived fold-bind wrap rhs-wrap combine))]))

  (define-syntax (for/fold stx)
    (syntax-case stx ()
      [(_ . rest) (quasisyntax/loc stx (for/fold/derived #,stx . rest))]))
  (define-syntax (for*/fold stx)
    (syntax-case stx ()
      [(_ . rest) (quasisyntax/loc stx (for*/fold/derived #,stx . rest))]))

  (define-for-variants (for for*)
    ([fold-var (void)])
    (lambda (x) x)
    (lambda (x) x)
    (lambda (x) `(,#'begin ,x ,#'(void))))

  (define-for-variants (for/list for*/list)
    ([fold-var null])
    (lambda (x) `(,#'reverse ,x))
    (lambda (x) x)
    (lambda (x) `(,#'cons ,x ,#'fold-var)))

  (define-for-syntax (do-for/lists for/fold-id stx)
    (syntax-case stx ()
      [(_ (id ...) bindings expr1 expr ...)
       (let ([ids (syntax->list #'(id ...))])
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error #f
                                           "not an identifier"
                                           stx
                                           id)))
                   ids)
         (with-syntax ([(id2 ...) (generate-temporaries ids)]
                       [for/fold for/fold-id]
                       [orig-stx stx])
           #'(let-values ([(id ...)
                           (for/fold orig-stx ([id null] ...) bindings
                                     (let-values ([(id2 ...) (let ()
                                                               expr1
                                                               expr ...)])
                                       (values* (cons id2 id) ...)))])
               (values* (reverse id) ...))))]))

  (define-syntax (for/lists stx) (do-for/lists #'for/fold/derived stx))
  (define-syntax (for*/lists stx) (do-for/lists #'for*/fold/derived stx))

  (define-for-variants (for/and for*/and)
    ([result #t])
    (lambda (x) x)
    (lambda (rhs) #`(stop-after #,rhs (lambda x (not result))))
    (lambda (x) x))

  (define-for-variants (for/or for*/or)
    ([result #f])
    (lambda (x) x)
    (lambda (rhs) #`(stop-after #,rhs (lambda x result)))
    (lambda (x) x))

  (define-for-variants (for/first for*/first)
    ([val #f][stop? #f])
    (lambda (x) #`(let-values ([(val _) #,x]) val))
    (lambda (rhs) #`(stop-after #,rhs (lambda x stop?)))
    (lambda (x) #`(values #,x #t)))

  (define-for-variants (for/last for*/last)
    ([result #f])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x) x))

  (define-for-variants (for/hash for*/hash)
    ([table #hash()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val))))

  (define-for-variants (for/hasheq for*/hasheq)
    ([table #hasheq()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val))))

  (define-for-variants (for/hasheqv for*/hasheqv)
    ([table #hasheqv()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hash-set table key val))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  specific sequences

  (define-sequence-syntax *in-range
    (lambda () #'in-range)
    (lambda (stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ a b step)]
           (let ([all-fx? (and (fixnum? (syntax-e #'a))
                               (fixnum? (syntax-e #'b))
                               (memq (syntax-e #'step) '(1 -1)))])
             #`[(id)
                (:do-in
                 ;; outer bindings:
                 ([(start) a] [(end) b] [(inc) step])
                 ;; outer check:
                 (unless (and (real? start) (real? end) (real? inc))
                   ;; let `in-range' report the error:
                   (in-range start end inc))
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
                          #`(if (step . >= . 0) (< pos end) (> pos end))]
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
                 ((#,(if all-fx? #'unsafe-fx+ #'+) pos inc)))])]
          [[(id) (_ a b)] (loop #'[(id) (_ a b 1)])]
          [[(id) (_ b)] (loop #'[(id) (_ 0 b 1)])]
          [_ #f]))))

  (define-sequence-syntax *in-naturals
    (lambda () #'in-naturals)
    (lambda (stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ start-expr)]
           #`[(id)
              (:do-in
               ;; outer bindings:
               ([(start) start-expr])
               ;; outer check:
               (unless (exact-nonnegative-integer? start)
                 ;; let `in-naturals' report the error:
                 (in-naturals start))
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
               ((+ pos 1)))]]
          [[(id) (_)]
           (loop #'[(id) (_ 0)])]
          [_ #f]))))

  (define-sequence-syntax *in-list
    (lambda () #'in-list)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ lst-expr)]
         #'[(id)
            (:do-in
             ;;outer bindings
             ([(lst) lst-expr])
             ;; outer check
             (void) ; (unless (list? lst) (in-list lst))
             ;; loop bindings
             ([lst lst])
             ;; pos check
             (not (null? lst))
             ;; inner bindings
             ([(id) (car lst)])
             ;; pre guard
             #t
             ;; post guard
             #t
             ;; loop args -- ok to use unsafe-cdr, since car passed
             ((unsafe-cdr lst)))]]
        [_ #f])))

  (define-for-syntax (vector-like-gen vector?-id
                                      unsafe-vector-length-id
                                      in-vector-id
                                      unsafe-vector-ref-id)
     (define (in-vector-like stx)
       (with-syntax ([vector? vector?-id]
                     [in-vector in-vector-id]
                     [unsafe-vector-length unsafe-vector-length-id]
                     [unsafe-vector-ref unsafe-vector-ref-id])
         (syntax-case stx ()
           ;; Fast case
           [[(id) (_ vec-expr)]
            #'[(id)
               (:do-in
                ;;outer bindings
                ([(vec len) (let ([vec vec-expr])
                              (unless (vector? vec)
                                (in-vector vec))
                              (values vec (unsafe-vector-length vec)))])
                ;; outer check
                #f
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
                ((unsafe-fx+ 1 pos)))]]
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
                  ;; Prevent multiple evaluation
                  ([(v* stop*) (let ([vec vec-expr]
                                     [stop* stop])
                                 (if (and (not stop*) (vector? vec))
                                     (values vec (unsafe-vector-length vec))
                                     (values vec stop*)))]
                   [(start*) start]
                   [(step*) step])
                  ;; Outer check
                  (when (or (not (vector? v*))
                            (not (exact-integer? start*))
                            (not (exact-integer? stop*))
                            (not (exact-integer? step*))
                            (zero? step*)
                            (and (< start* stop*) (< step* 0))
                            (and (> start* stop*) (> step* 0)))
                    ;; Let in-vector report the error
                    (in-vector v* start* stop* step*))
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

  (define-sequence-syntax *in-vector
    (lambda () #'in-vector)
    (vector-like-gen #'vector?
                     #'unsafe-vector*-length
                     #'in-vector
                     #'unsafe-vector*-ref))

  (define-sequence-syntax *in-string
    (lambda () #'in-string)
    (vector-like-gen #'string?
                      #'string-length
                      #'in-string
                      #'string-ref))

  (define-sequence-syntax *in-bytes
    (lambda () #'in-bytes)
    (vector-like-gen #'bytes?
                     #'bytes-length
                     #'in-bytes
                     #'bytes-ref))

  (define-sequence-syntax *in-indexed
    (lambda () #'in-indexed)
    (lambda (stx)
      (syntax-case stx ()
        [[(id1 id2) (_ gen-expr)]
         #'[(id1 id2) (in-parallel gen-expr (*in-naturals))]])))

  (define-sequence-syntax *in-value
    (lambda () #'in-value)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ expr)]
         #'[(id) (:do-in ([(id) expr]) #t () #t () #t #f ())]])))

  (define-sequence-syntax *in-producer
    (lambda () #'in-producer)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ producer stop more ...)]
         (with-syntax ([(more* ...) (generate-temporaries #'(more ...))])
           #'[(id)
              (:do-in
               ;;outer bindings
               ([(producer*) producer] [(more*) more] ...
                [(stop?) (let ([s stop])
                           (if (procedure? s) s (lambda (x) (eq? x s))))])
               ;; outer check
               #t
               ;; loop bindings
               ()
               ;; pos check
               #t
               ;; inner bindings
               ([(id) (producer* more* ...)])
               ;; pre guard
               (not (stop? id))
               ;; post guard
               #t
               ;; loop args
               ())])]
        ;; multiple-values version
        [[(id ...) (_ producer stop more ...)]
         (with-syntax ([(more* ...) (generate-temporaries #'(more ...))])
           #'[(id ...)
              (:do-in
               ;;outer bindings
               ([(producer*) producer] [(more*) more] ...
                [(stop?) (let ([s stop])
                           (if (procedure? s)
                             s
                             (error 'in-producer
                                    "stop condition for ~a, got: ~e"
                                    "multiple values must be a predicate" s)))])
               ;; outer check
               #t
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
               ())])])))

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
                    (unless (and (procedure? r*)
                                 (procedure-arity-includes? r* 1))
                      (raise-type-error 'in-port "procedure (arity 1)" r*))
                    (unless (input-port? p*)
                      (raise-type-error 'in-port "input-port" p*))
                    (lambda () (r* p*)))
                  eof)]])))

  (define-sequence-syntax *in-lines
    (lambda () #'in-lines)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_)]   #'[(id) (*in-lines (current-input-port) 'any)]]
        [[(id) (_ p)] #'[(id) (*in-lines p 'any)]]
        [[(id) (_ p mode)]
         #'[(id) (*in-producer
                  (let ([p* p] [mode* mode])
                    (unless (input-port? p*)
                      (raise-type-error 'in-lines "input-port" p*))
                    (unless (memq mode* '(linefeed return return-linefeed any
                                          any-one))
                      (raise-type-error
                       'in-lines
                       "'linefeed, 'return, 'return-linefeed, 'any, or 'any-one"
                       mode*))
                    (lambda () (read-line p* mode*)))
                  eof)]])))
  
  (define-sequence-syntax *in-bytes-lines
    (lambda () #'in-bytes-lines)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_)]   #'[(id) (*in-bytes-lines (current-input-port) 'any)]]
        [[(id) (_ p)] #'[(id) (*in-bytes-lines p 'any)]]
        [[(id) (_ p mode)]
         #'[(id) (*in-producer
                  (let ([p* p] [mode* mode])
                    (unless (input-port? p*)
                      (raise-type-error 'in-bytes-lines "input-port" p*))
                    (unless (memq mode* '(linefeed return return-linefeed any
                                          any-one))
                      (raise-type-error
                       'in-bytes-lines
                       "'linefeed, 'return, 'return-linefeed, 'any, or 'any-one"
                       mode*))
                    (lambda () (read-bytes-line p* mode*)))
                  eof)]])))

  (define-sequence-syntax *in-input-port-bytes
    (lambda () #'in-input-port-bytes)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ p)]
         #'[(id) (*in-producer
                  (let ([p* p])
                    (unless (input-port? p*)
                      (raise-type-error 'in-input-port-bytes "input-port" p*))
                    (lambda () (read-byte p*)))
                  eof)]])))

  (define-sequence-syntax *in-input-port-chars
    (lambda () #'in-input-port-chars)
    (lambda (stx)
      (syntax-case stx ()
        [[(id) (_ p)]
         #'[(id) (*in-producer
                  (let ([p* p])
                    (unless (input-port? p*)
                      (raise-type-error 'in-input-port-chars "input-port" p*))
                    (lambda () (read-char p*)))
                  eof)]])))
  
  (define in-directory
    (case-lambda
     [(dir)
      (when dir
        (unless (path-string? dir)
          (raise-type-error 'in-directory "#f, path, or path string" dir)))
      (let ([make-gen (lambda ()
                        (call-with-continuation-prompt
                         (lambda ()
                           (define (reply v)
                             (let/cc k
                               (abort-current-continuation
                                (default-continuation-prompt-tag)
                                (lambda () (cons (lambda () v) k)))))
                           (let loop ([dir (path->complete-path (or dir (current-directory)))]
                                      [prefix dir])
                             (for ([i (in-list (directory-list dir))])
                               (let ([p (if prefix (build-path prefix i) i)]
                                     [fp (build-path dir i)])
                                 (reply p)
                                 (when (directory-exists? fp)
                                   (loop fp p)))))
                           (reply eof))))])
        (make-do-sequence 
         (lambda ()
           (values
            (lambda (gen) ((car gen)))
            (lambda (gen) (call-with-continuation-prompt
                           (lambda ()
                             ((cdr gen)))))
            (make-gen)
            (lambda (gen) (not (eof-object? ((car gen)))))
            (lambda (val) #t)
            (lambda (gen val) #t)))))]
     [() (in-directory #f)]))

  )
