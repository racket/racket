(module for mzscheme

  (provide for/fold for*/fold
           for for*
           for/list for*/list
           for/lists for*/lists
           for/and for*/and
           for/or for*/or
           for/first for*/first
           for/last for*/last

           for/fold/derived for*/fold/derived
           
           (rename *in-range in-range)
           (rename *in-naturals in-naturals)
           (rename *in-list in-list)
           (rename *in-vector in-vector)
           (rename *in-string in-string)
           (rename *in-bytes in-bytes)
           in-input-port-bytes
           in-input-port-chars
           in-hash-table
           in-hash-table-keys
           in-hash-table-values
           in-hash-table-pairs
           
           in-parallel
           stop-before
           stop-after
           (rename *in-indexed in-indexed)
           
           sequence?
           sequence-generate
           
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
      (unless (or (identifier? proc1)
                  (and (procedure? proc1)
                       (procedure-arity-includes? proc1 1)))
        (raise-type-error 'define-sequence-syntax
                          "identifier of procedure (arity 1)"
                          0
                          proc1 proc2))
      (unless (and (procedure? proc2)
                   (procedure-arity-includes? proc2 2))
        (raise-type-error 'define-sequence-syntax
                          "procedure (arity 1)"
                          1
                          proc1 proc2))
      (make-sequence-transformer (if (identifier? proc1)
                                      (lambda (stx)
                                        (if (identifier? stx)
                                            proc1
                                            (datum->syntax-object stx
                                                                  #`(#,proc1 . #,(cdr (syntax-e stx)))
                                                                  stx
                                                                  stx)))
                                      proc1)
                                  proc2
                                  cert))
    
    (define (certify-clause clause certifier introducer)
      ;; This is slightly painful. The painsion into `:do-in' involves a lot of pieces
      ;; that are no treated as sub-expressions. We have to push the certificates
      ;; down to all the relevant identifiers and expressions:
      (define (cert s) (certifier s #f introducer))
      (define (map-cert s) (map (lambda (s) (certifier s #f #;introducer))
                                (syntax->list s)))

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
                        (map map-cert
                             (syntax->list #'((inner-id ...) ...)))]
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
        [_else
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
                 (raise-syntax-error
                  #f
                  "duplicate identifier as sequence binding"
                  orig-stx
                  dup)))
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
               (let ([xformed (xformer orig-stx (introducer (syntax-local-introduce clause)))])
                 (if xformed
                     (expand-clause orig-stx (certify-clause (syntax-local-introduce (introducer xformed)) 
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
          [_else
           (raise-syntax-error
            #f
            "bad sequence binding clause"
            orig-stx
            clause)]))))

  (define-syntax (:do-in stx)
    (raise-syntax-error #f "illegal outside of a loop or comprehension binding" stx))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  sequences
  
  (define-values (prop:sequence :sequence? :sequence-ref)
    (make-struct-type-property 'sequence
                               (lambda (v sinfo)
                                 (unless (and (procedure? v)
                                              (procedure-arity-includes? v 1))
                                   (raise-type-error
                                    'sequence-property-guard
                                    "procedure (arity 1)"
                                    v))
                                 v)))
  
  (define-values (struct:do-sequence
                  make-do-sequence
                  do-sequence?
                  do-sequence-ref
                  do-sequence-set!)
    (make-struct-type 'sequence #f
                      1 0 #f
                      (list (cons prop:sequence (lambda (v)
                                                   ((do-sequence-ref v 0)))))))

  (define-syntax define-sequence-syntax
    (syntax-rules ()
      [(_ id expr-transformer-expr clause-transformer-expr)
       (define-syntax id (create-sequence-transformer
                          expr-transformer-expr
                          clause-transformer-expr
                          (syntax-local-certifier #f)))]))

  (define (sequence? v)
    (or (:sequence? v)
        (list? v)
        (vector? v)
        (string? v)
        (bytes? v)
        (input-port? v)
        (hash-table? v)))
  
  (define (make-sequence who v)
    (cond
      [(:sequence? v) ((:sequence-ref v) v)]
      [(list? v) (:list-gen v)]
      [(vector? v) (:vector-gen v)]
      [(string? v) (:string-gen v)]
      [(bytes? v) (:bytes-gen v)]
      [(input-port? v) (:input-port-gen v)]
      [(hash-table? v) (:hash-table-gen v cons (lambda (p) (values (car p) (cdr p))))]
      [else (raise
             (make-exn:fail:contract
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
                                 (lambda (x) #t)
                                 (lambda (x y) #t))))]))

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
      (make-do-sequence (lambda ()
                          (values values
                                  add1
                                  n
                                  (lambda (x) #t)
                                  (lambda (x) #t)
                                  (lambda (x y) #t))))]))

  (define (in-list l)
    ; (unless (list? l) (raise-type-error 'in-list "list" l))
    (make-do-sequence (lambda () (:list-gen l))))
  
  (define (:list-gen l)
    (values car cdr l pair? (lambda (x) #t) (lambda (p x) #t)))

  (define (in-vector l)
    (unless (vector? l) (raise-type-error 'in-vector "vector" l))
    (make-do-sequence (lambda () (:vector-gen l))))

  (define (:vector-gen v)
    (let ([len (vector-length v)])
      (values (lambda (i)
                (vector-ref v i))
              add1
              0
              (lambda (i) (< i len))
              (lambda (x) #t)
              (lambda (x y) #t))))

  (define (in-string l)
    (unless (string? l) (raise-type-error 'in-string "string" l))
    (make-do-sequence (lambda () (:string-gen l))))

  (define (:string-gen v)
    (let ([len (string-length v)])
      (values (lambda (i)
                (string-ref v i))
              add1
              0
              (lambda (i) (< i len))
              (lambda (x) #t)
              (lambda (x y) #t))))

  (define (in-bytes l)
    (unless (bytes? l) (raise-type-error 'in-bytes "bytes" l))
    (make-do-sequence (lambda () (:bytes-gen l))))

  (define (:bytes-gen v)
    (let ([len (bytes-length v)])
      (values (lambda (i)
                (bytes-ref v i))
              add1
              0
              (lambda (i) (< i len))
              (lambda (x) #t)
              (lambda (x y) #t))))
  
  (define (in-input-port-bytes l)
    (unless (input-port? l) (raise-type-error 'in-input-port-bytes "input-port" l))
    (make-do-sequence (lambda () (:input-port-gen l))))

  (define (:input-port-gen v)
    (values (lambda (v) (read-byte v))
            (lambda (v) v)
            v
            (lambda (v) #t)
            (lambda (x) (not (eof-object? x)))
            (lambda (x v) #t)))

  (define (in-input-port-chars v)
    (unless (input-port? v) (raise-type-error 'in-input-port-chars "input-port" v))
    (make-do-sequence (lambda ()
                             (values (lambda (v) (read-char v))
                                     (lambda (v) v)
                                     v
                                     (lambda (v) #t)
                                     (lambda (x) (not (eof-object? x)))
                                     (lambda (x v) #t)))))
  
  (define (in-hash-table ht)
    (unless (hash-table? ht) (raise-type-error 'in-hash-table "hash-table" ht))
    (make-do-sequence (lambda () (:hash-table-gen ht cons (lambda (p) (values (car p) (cdr p)))))))
  (define (in-hash-table-keys ht)
    (unless (hash-table? ht) (raise-type-error 'in-hash-table-keys "hash-table" ht))
    (make-do-sequence (lambda () (:hash-table-gen ht (lambda (k v) k) values))))
  (define (in-hash-table-values ht)
    (unless (hash-table? ht) (raise-type-error 'in-hash-table-values "hash-table" ht))
    (make-do-sequence (lambda () (:hash-table-gen ht (lambda (k v) v) values))))
  (define (in-hash-table-pairs ht)
    (unless (hash-table? ht) (raise-type-error 'in-hash-table-values "hash-table" ht))
    (make-do-sequence (lambda () (:hash-table-gen ht cons values))))

  (define (:hash-table-gen ht sel f)
    (let ([l (hash-table-map ht sel)])
      (values (lambda (l) (f (car l))) cdr l pair? (lambda args #t) (lambda args #t))))

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

  ;; ----------------------------------------

  (define (in-parallel . sequences)
    (for-each (lambda (g)
                (unless (sequence? g)
                  (raise-type-error 'in-parallel "sequence" g)))
              sequences)
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
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  runnign sequences outside of a loop:

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
      [(_ [orig-stx nested? emit? ()] ([fold-var fold-init] ...) () expr1 expr ...)
       #`(let ([fold-var fold-init] ...) (let () expr1 expr ...))]
      ;; Switch-to-emit case (no more clauses to generate):
      [(_ [orig-stx nested? #f binds] ([fold-var fold-init] ...) () . body)
       #`(for/foldX/derived [orig-stx nested? #t binds] ([fold-var fold-init] ...) () . body)]
      ;; Emit case:
      [(_ [orig-stx nested? #t binds] ([fold-var fold-init] ...) rest . body)
       (with-syntax ([(([outer-binding ...]
                        outer-check
                        [loop-binding ...]
                        pos-guard
                        [inner-binding ...]
                        pre-guard
                        post-guard
                        [loop-arg ...]) ...) (reverse (syntax->list #'binds))])
         #'(let-values (outer-binding ... ...)
             outer-check ...
             (let comp-loop ([fold-var fold-init] ...
                             loop-binding ... ...)
               (if (and pos-guard ...)
                   (let-values (inner-binding ... ...)
                     (if (and pre-guard ...)
                         (let-values ([(fold-var ...)
                                       (for/foldX/derived [orig-stx nested? #f ()] ([fold-var fold-var] ...) rest . body)])
                           (if (and post-guard ...)
                               (comp-loop fold-var ... loop-arg ... ...)
                               (values* fold-var ...)))
                         (values* fold-var ...)))
                   (values* fold-var ...)))))]
      ;; Bad body cases:
      [(_ [orig-stx . _rest] fold-bind ())
       (raise-syntax-error
        #f
        "missing body expression after sequence bindings"
        #'orig-stx)]
      [(_ [orig-stx . _rest] fold-bind () . rest)
       (raise-syntax-error
        #f
        "bad syntax (illegal use of `.') after sequence bindings"
        #'orig-stx)]
      ;; Guard case, no pending emits:
      [(_ [orig-stx nested? #f ()] ([fold-var fold-init] ...) (#:when expr . rest) . body)
       #'(if expr
             (for/foldX/derived [orig-stx nested? #f ()] ([fold-var fold-init] ...) rest . body)
             (values* fold-init ...))]
      ;; Guard case, pending emits need to be flushed first
      [(_ [orig-stx nested? #f binds] ([fold-var fold-init] ...) (#:when expr . rest) . body)
       #'(_ [orig-stx nested? #t binds] ([fold-var fold-init] ...) (#:when expr . rest) . body)]
      ;; Convert single-value form to multi-value form:
      [(_ [orig-stx nested? #f binds] fold-bind ([id rhs] . rest) . body)
       (identifier? #'id)
       #'(for/foldX/derived [orig-stx nested? #f binds] fold-bind ([(id) rhs] . rest) . body)]
      ;; If we get here in single-value mode, then it's a bad clause:
      [(_ [orig-stx #f #f nested? #f binds] fold-bind (clause . rest) . body)
       (raise-syntax-error
        #f
        "bad sequence binding clause"
        #'orig-stx
        #'clause)]
      ;; Expand one multi-value clause, and push it into the results to emit: 
      [(_ [orig-stx nested? #f binds] ([fold-var fold-init] ...) (clause . rest) . body)
       (with-syntax ([bind (expand-clause #'orig-stx #'clause)])
         #`(_ [orig-stx nested? nested? (bind . binds)] ([fold-var fold-init] ...) rest . body))]
      [(_ [orig-stx . _rest] . _rest2)
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

  (define-for-syntax (for-variant-stx stx derived-id-stx fold-bind-stx wrap rhs-wrap combine multi?)
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
                                            (if multi?
                                                (andmap identifier? (or (syntax->list #'ids) '(#f)))
                                                (identifier? #'ids))
                                            (cons #`[ids #,(rhs-wrap #'rhs)]
                                                  (loop (cdr bs)))]
                                           [#:when (cons (car bs)
                                                          (if (null? (cdr bs))
                                                              null
                                                              (cons (cadr bs) (loop (cddr bs)))))]
                                           [_else 
                                            ;; a syntax error; les the /derived form handle it, and 
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
      [(_ id derived-id fold-bind wrap rhs-wrap combine multi?)
       (define-syntax (id stx) (for-variant-stx stx #'derived-id #'fold-bind wrap rhs-wrap combine multi?))]))
  
  (define-syntax define-for-variants
    (syntax-rules ()
      [(_ (for for*) fold-bind wrap rhs-wrap combine)
       (begin
         (define-syntax-via-derived for for/fold/derived fold-bind wrap rhs-wrap combine #f)
         (define-syntax-via-derived for* for*/fold/derived fold-bind wrap rhs-wrap combine #f))]))

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
  
  (define-for-syntax (make-for/lists for/fold-id)
    (lambda (stx)
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
                 (values* (reverse id) ...))))])))
  
  (define-syntax for/lists (make-for/lists #'for/fold/derived))
  (define-syntax for*/lists (make-for/lists #'for*/fold/derived))
  
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  specific sequences

  (define-sequence-syntax *in-range
    #'in-range
    (lambda (orig-stx stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ a b step)] #`[(id)
                                  (:do-in
                                   ;; outer bindings:
                                   ([(start) a] [(end) b] [(inc) step])
                                   ;; outer check:
                                   (unless (and (real? a) (real? b) (real? inc))
                                     ;; let `in-range' report the error:
                                     (in-range start end inc))
                                   ;; loop bindings:
                                   ([pos start])
                                   ;; pos check
                                   #,(cond
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
                                   ((+ pos inc)))]]
          [[(id) (_ a b)] (loop #'[(id) (_ a b 1)])]
          [[(id) (_ b)] (loop #'[(id) (_ 0 b 1)])]
          [_else #f]))))

  (define-sequence-syntax *in-naturals
    #'in-naturals
    (lambda (orig-stx stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ start)]
           (and (integer? (syntax-e #'start))
                (exact? (syntax-e #'start))
                ((syntax-e #'start) . >= . 0))
           #`[(id)
              (:do-in
               ;; outer bindings:
               ()
               ;; outer check:
               (void)
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
          [_else #f]))))

  (define-sequence-syntax *in-list
    #'in-list
    (lambda (orig-stx stx)
      (syntax-case stx ()
        [((id) (_ lst-expr))
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
             ;; loop args
             ((cdr lst)))]]
        [_else #f])))

  (define-for-syntax (vector-like-gen vector?-id
                                      vector-length-id
                                      in-vector-id
                                      vector-ref-id)
     (lambda (orig-stx stx)
       (with-syntax ([vector? vector?-id]
                     [in-vector in-vector-id]
                     [vector-length vector-length-id]
                     [vector-ref vector-ref-id])
         (syntax-case stx ()
           [((id) (_ vec-expr))
            #'[(id)
               (:do-in
                ;;outer bindings
                ([(vec len) (let ([vec vec-expr])
                              (unless (vector? vec)
                                (in-vector vec))
                              (values vec (vector-length vec)))])
                ;; outer check
                #f
                ;; loop bindings
                ([pos 0])
                ;; pos check
                (pos . < . len)
                ;; inner bindings
                ([(id) (vector-ref vec pos)])
                ;; pre guard
                #t
                ;; post guard
                #t
                ;; loop args
                ((add1 pos)))]]
           [_else #f]))))
  
  (define-sequence-syntax *in-vector
    #'in-vector
    (vector-like-gen #'vector?
                     #'vector-length
                     #'in-vector
                     #'vector-ref))
  
  (define-sequence-syntax *in-string
    #'in-string
    (vector-like-gen #'string?
                      #'string-length
                      #'in-string
                      #'string-ref))
  
  (define-sequence-syntax *in-bytes
    #'in-bytes
    (vector-like-gen #'bytes?
                     #'bytes-length
                     #'in-bytes
                     #'bytes-ref))
  
  (define-sequence-syntax *in-indexed
    #'in-indexed
    (lambda (orig-stx stx)
      (syntax-case stx ()
        [((id1 id2) (_ gen-expr))
         #'[(id1 id2) (in-parallel gen-expr (*in-naturals))]]))))
