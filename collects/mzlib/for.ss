(module for mzscheme

  (provide fold-for fold-for-values fold-for* fold-for*-values
           for for-values for* for*-values
           list-for list-for-values list-for* list-for*-values 
           lists-for lists-for-values lists-for* lists-for*-values
           and-for and-for-values and-for* and-for*-values
           or-for or-for-values or-for* or-for*-values
           first-for first-for-values first-for* first-for*-values
           last-for last-for-values last-for* last-for*-values
           
           (rename *range range)
           (rename *nat-gen nat-gen)
           (rename *list->gen list->gen)
           (rename *vector->gen vector->gen)
           (rename *string->gen string->gen)
           (rename *bytes->gen bytes->gen)
           input-port->byte-gen
           input-port->char-gen
           
           parallel-gen
           stop-before-gen
           stop-after-gen
           (rename *index-gen index-gen)
           
           generator-run
           
           define-generator-syntax
           make-do-generator
           :do-gen)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; generator transformers:
  
  (begin-for-syntax
    (define-values (struct:generator-transformer
                    make-generator-transformer
                    generator-transformer?
                    generator-transformer-ref
                    generator-transformer-set!)
      (make-struct-type 'generator-transformer #f
                        3 0 #f
                        null (current-inspector)
                        0))

    (define (create-generator-transformer proc1 proc2 cert)
      (unless (if (identifier? proc1)
                  (and (procedure? proc1)
                       (procedure-arity-includes? proc1 1))
        (raise-type-error 'define-generator-syntax
                          "identifier of procedure (arity 1)"
                          0
                          proc1 proc2))
      (unless (and (procedure? proc2)
                   (procedure-arity-includes? proc2 2))
        (raise-type-error 'define-generator-syntax
                          "procedure (arity 1)"
                          1
                          proc1 proc2))
      (make-generator-transformer (if (identifier? proc1)
                                      (lambda (stx)
                                        (if (identifier? stx)
                                            proc1
                                            (datum->syntax-object stx
                                                                  #`(#,proc1 . #,(cdr (syntax-e stx)))
                                                                  stx
                                                                  stx)))
                                      proc1)
                                  proc2
                                  cert)))
    
    (define (certify-clause clause certifier introducer)
      ;; This is slightly painful. The painsion into `:do-gen' involves a lot of pieces
      ;; that are no treated as sub-expressions. We have to push the certificates
      ;; down to all the relevant identifiers and expressions:
      (define (cert s) (certifier s #f introducer))
      (define (map-cert s) (map (lambda (s) (certifier s #f #;introducer))
                                (syntax->list s)))

      (syntax-case clause (:do-gen)
        [[(id ...) (:do-gen ([(outer-id ...) outer-expr] ...)
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
           #`[(id ...) (:do-gen ([(outer-id ...) outer-expr] ...)
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
        (syntax-case clause (values parallel-gen stop-before-gen stop-after-gen :do-gen)
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
                  "duplicate identifier as generator binding"
                  orig-stx
                  dup)))
             #f)
           'just-checking]
          [[(id ...) (form . rest)]
           (and use-transformer?
                (identifier? #'form)
                (generator-transformer? (syntax-local-value #'form (lambda () #f))))
           (let ([m (syntax-local-value #'form)])
             (let ([xformer (generator-transformer-ref m 1)]
                   [introducer (make-syntax-introducer)]
                   [certifier (generator-transformer-ref m 2)])
               (let ([xformed (xformer orig-stx (introducer (syntax-local-introduce clause)))])
                 (if xformed
                     (expand-clause orig-stx (certify-clause (syntax-local-introduce (introducer xformed)) 
                                                             certifier
                                                             introducer))
                     (eloop #f)))))]
          [[(id ...) (:do-gen . body)]
           (syntax-case #'body ()
             [(([(outer-id ...) outer-rhs] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-rhs] ...)
               pre-guard
               post-guard
               (loop-arg ...)) #'body]
             [else (raise-syntax-error #f "bad :do-gen clause" orig-stx clause)])]
          [[(id) (values rhs)]
           (expand-clause orig-stx #'[(id) rhs])]
          [[(id ...) (parallel-gen rhs ...)]
           (and (= (length (syntax->list #'(id ...)))
                   (length (syntax->list #'(rhs ...)))))
           ;; flatten parallel-gen iterations:
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
          [[(id ...) (stop-before-gen gen-expr pred)]
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
          [[(id ...) (stop-after-gen gen-expr pred)]
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
                      (#,((syntax-local-certifier #f) #'make-generator) '(id ...) rhs)])
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
            "bad generator binding clause"
            orig-stx
            clause)]))))

  (define-syntax (:do-gen stx)
    (raise-syntax-error #f "illegal outside of a loop or comprehension binding" stx))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  generators
  
  (define-values (prop:generator :generator? :generator-ref)
    (make-struct-type-property 'generator
                               (lambda (v sinfo)
                                 (unless (and (procedure? v)
                                              (procedure-arity-includes? v 1))
                                   (raise-type-error
                                    'generator-property-guard
                                    "procedure (arity 1)"
                                    v))
                                 v)))
  
  (define-values (struct:do-generator
                  make-do-generator
                  do-generator?
                  do-generator-ref
                  do-generator-set!)
    (make-struct-type 'generator #f
                      1 0 #f
                      (list (cons prop:generator (lambda (v)
                                                   ((do-generator-ref v 0)))))))

  (define-syntax define-generator-syntax
    (syntax-rules ()
      [(_ id expr-transformer-expr clause-transformer-expr)
       (define-syntax id (create-generator-transformer
                          expr-transformer-expr
                          clause-transformer-expr
                          (syntax-local-certifier #f)))]))

  (define (generator? v)
    (or (:generator? v)
        (list? v)
        (vector? v)
        (string? v)
        (bytes? v)
        (input-port? v)))
  
  (define (make-generator who v)
    (cond
      [(:generator? v) ((:generator-ref v) v)]
      [(list? v) (:list-gen v)]
      [(vector? v) (:vector-gen v)]
      [(string? v) (:string-gen v)]
      [(bytes? v) (:bytes-gen v)]
      [(input-port? v) (:input-port-gen v)]
      [else (raise
             (make-exn:fail:contract
              (format "for: expected a generator for ~a, got something else: ~v"
                      (if (= 1 (length who))
                          (car who)
                          who)
                      v)
              (current-continuation-marks)))]))
  
  (define range
    (case-lambda
      [(b) (range 0 b 1)]
      [(a b) (range a b 1)]
      [(a b step)
       (unless (real? a) (raise-type-error 'range "real-number" a))
       (unless (real? b) (raise-type-error 'range "real-number" b))
       (unless (real? step) (raise-type-error 'range "real-number" step))
       (make-do-generator (lambda ()
                                (values
                                 (lambda (x) x)
                                 (lambda (x) (+ x step))
                                 a
                                 (if (step . >= . 0)
                                     (lambda (x) (< x b))
                                     (lambda (x) (> x b)))
                                 (lambda (x) #t)
                                 (lambda (x y) #t))))]))

  (define (nat-gen)
    (make-do-generator (lambda ()
                             (values values
                                     add1
                                     0
                                     (lambda (x) #t)
                                     (lambda (x) #t)
                                     (lambda (x y) #t)))))

  (define (list->gen l)
    (unless (list? l) (raise-type-error 'list->gen "list" l))
    (make-do-generator (lambda () (:list-gen l))))
  
  (define (:list-gen l)
    (values car cdr l pair? (lambda (x) #t) (lambda (p x) #t)))

  (define (vector->gen l)
    (unless (vector? l) (raise-type-error 'vector->gen "vector" l))
    (make-do-generator (lambda () (:vector-gen l))))

  (define (:vector-gen v)
    (let ([len (vector-length v)])
      (values (lambda (i)
                (vector-ref v i))
              add1
              0
              (lambda (i) (< i len))
              (lambda (x) #t)
              (lambda (x y) #t))))

  (define (string->gen l)
    (unless (string? l) (raise-type-error 'string->gen "string" l))
    (make-do-generator (lambda () (:string-gen l))))

  (define (:string-gen v)
    (let ([len (string-length v)])
      (values (lambda (i)
                (string-ref v i))
              add1
              0
              (lambda (i) (< i len))
              (lambda (x) #t)
              (lambda (x y) #t))))

  (define (bytes->gen l)
    (unless (bytes? l) (raise-type-error 'bytes->gen "bytes" l))
    (make-do-generator (lambda () (:bytes-gen l))))

  (define (:bytes-gen v)
    (let ([len (bytes-length v)])
      (values (lambda (i)
                (bytes-ref v i))
              add1
              0
              (lambda (i) (< i len))
              (lambda (x) #t)
              (lambda (x y) #t))))
  
  (define (input-port->byte-gen l)
    (unless (input-port? l) (raise-type-error 'input-port->byte-gen "input-port" l))
    (make-do-generator (lambda () (:input-port-gen l))))

  (define (:input-port-gen v)
    (values (lambda (v) (read-byte v))
            (lambda (v) v)
            v
            (lambda (v) #t)
            (lambda (x) (not (eof-object? x)))
            (lambda (x v) #t)))

  (define (input-port->char-gen v)
    (unless (input-port? v) (raise-type-error 'input-port->char-gen "input-port" v))
    (make-do-generator (lambda ()
                             (values (lambda (v) (read-char v))
                                     (lambda (v) v)
                                     v
                                     (lambda (v) #t)
                                     (lambda (x) (not (eof-object? x)))
                                     (lambda (x v) #t)))))
  
  (define (stop-before-gen g pred)
    (unless (generator? g) (raise-type-error 'stop-before-gen "generator" g))
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (raise-type-error 'stop-before-gen "procedure (arity 1)" pred))
    (make-do-generator (lambda ()
                             (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                           (make-generator #f g)])
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

  (define (stop-after-gen g pred)
    (unless (generator? g) (raise-type-error 'stop-after-gen "generator" g))
    (unless (and (procedure? pred)
                 (procedure-arity-includes? pred 1))
      (raise-type-error 'stop-after-gen "procedure (arity 1)" pred))
    (make-do-generator (lambda ()
                             (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                           (make-generator #f g)])
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

  (define (index-gen g)
    (unless (generator? g) (raise-type-error 'index-gen "generator" g))
    (make-do-generator (lambda ()
                             (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                           (make-generator #f g)])
                               (values (lambda (pos) (values (pos->val (car pos)) (cdr pos)))
                                       (lambda (pos) (cons (pos-next (car pos)) (add1 (cdr pos))))
                                       (cons init 0)
                                       (lambda (pos) (pos-cont? (car pos)))
                                       (lambda (val idx) (pre-cont? val))
                                       (lambda (pos val idx) (post-cont? pos val)))))))

  ;; ----------------------------------------

  (define (parallel-gen . generators)
    (for-each (lambda (g)
                (unless (generator? g)
                  (raise-type-error 'parallel-gen "generator" g)))
              generators)
    (if (= 1 (length generators))
        (car generators)
        (make-do-generator
         (lambda ()
           (let-values ([(pos->vals pos-nexts inits pos-cont?s pre-cont?s post-cont?s)
                         (lists-for (p->v p-s i ps? pr? po?) ([g generators])
                           (make-generator #f g))])
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
  ;;  runnign generators outside of a loop:

  (define (generator-run g)
    (unless (generator? g)
      (raise-type-error 'generator-run "generator" g))
    (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                  (make-generator #f g)])
      (let ([pos init])
        (letrec ([more? #f]
                 [prep-val! #f]
                 [next #f])
          (letrec ([no-more (lambda ()
                              (error "generator has no more values"))]
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
            (let ([generator-more? (lambda () (more?))]
                  [generator-next (lambda () (next))])
              (values generator-more?
                      generator-next)))))))
      
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  core `fold-for' syntax
  
  (define-syntax values*
    (syntax-rules ()
      [(_ x) x]
      [(_ x ...) (values x ...)]))

  (define-syntax (fold-forX/derived stx)
    (syntax-case stx ()
      ;; Done case (no more clauses, and no generated clauses to emit):
      [(_ [orig-stx multi? first-multi? nested? emit? ()] ([fold-var fold-init] ...) () expr1 expr ...)
       #`(let ([fold-var fold-init] ...) (let () expr1 expr ...))]
      ;; Switch-to-emit case (no more clauses to generate):
      [(_ [orig-stx multi? first-multi? nested? #f binds] ([fold-var fold-init] ...) () . body)
       #`(fold-forX/derived [orig-stx multi? first-multi? nested? #t binds] ([fold-var fold-init] ...) () . body)]
      ;; Emit case:
      [(_ [orig-stx multi? first-multi? nested? #t binds] ([fold-var fold-init] ...) rest . body)
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
                                       (fold-forX/derived [orig-stx multi? first-multi? nested? #f ()] ([fold-var fold-var] ...) rest . body)])
                           (if (and post-guard ...)
                               (comp-loop fold-var ... loop-arg ... ...)
                               (values* fold-var ...)))
                         (values* fold-var ...)))
                   (values* fold-var ...)))))]
      ;; Bad body cases:
      [(_ [orig-stx . _rest] fold-bind ())
       (raise-syntax-error
        #f
        "missing body expression after generator bindings"
        #'orig-stx)]
      [(_ [orig-stx . _rest] fold-bind () . rest)
       (raise-syntax-error
        #f
        "bad syntax (illegal use of `.') after generator bindings"
        #'orig-stx)]
      ;; Guard case, no pending emits:
      [(_ [orig-stx multi? first-multi? nested? #f ()] ([fold-var fold-init] ...) (#:when expr . rest) . body)
       #'(if expr
             (fold-forX/derived [orig-stx multi? first-multi? nested? #f ()] ([fold-var fold-init] ...) rest . body)
             (values* fold-init ...))]
      ;; Guard case, pending emits need to be flushed first
      [(_ [orig-stx multi? first-multi? nested? #f binds] ([fold-var fold-init] ...) (#:when expr . rest) . body)
       #'(_ [orig-stx multi? first-multi? nested? #t binds] ([fold-var fold-init] ...) (#:when expr . rest) . body)]
      ;; Convert single-value form to multi-value form:
      [(_ [orig-stx #f #f nested? #f binds] fold-bind ([id rhs] . rest) . body)
       (identifier? #'id)
       #'(fold-forX/derived [orig-stx #f #t nested? #f binds] fold-bind ([(id) rhs] . rest) . body)]
      ;; If we get here in single-value mode, then it's a bad clause:
      [(_ [orig-stx #f #f nested? #f binds] fold-bind (clause . rest) . body)
       (raise-syntax-error
        #f
        "bad generator binding clause"
        #'orig-stx
        #'clause)]
      ;; Expand one multi-value clause, and push it into the results to emit: 
      [(_ [orig-stx multi? #t nested? #f binds] ([fold-var fold-init] ...) (clause . rest) . body)
       (with-syntax ([bind (expand-clause #'orig-stx #'clause)])
         #`(_ [orig-stx multi? multi? nested? nested? (bind . binds)] ([fold-var fold-init] ...) rest . body))]
      [(_ [orig-stx . _rest] . _rest2)
       (raise-syntax-error #f "bad syntax" #'orig-stx)]))

  (define-syntax fold-for/derived
    (syntax-rules ()
      [(_ orig-stx . rest)
       (fold-forX/derived [orig-stx #f #f #f #f ()] . rest)]))

  (define-syntax fold-for-values/derived
    (syntax-rules ()
      [(_ orig-stx . rest)
       (fold-forX/derived [orig-stx #t #t #f #f ()] . rest)]))

  (define-syntax fold-for*/derived
    (syntax-rules ()
      [(_ orig-stx . rest)
       (fold-forX/derived [orig-stx #f #f #t #f ()] . rest)]))

  (define-syntax fold-for*-values/derived
    (syntax-rules ()
      [(_ orig-stx . rest)
       (fold-forX/derived [orig-stx #t #t #t #f ()] . rest)]))

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
      [(_ (for for-values for* for*-values) fold-bind wrap rhs-wrap combine)
       (begin
         (define-syntax-via-derived for fold-for/derived fold-bind wrap rhs-wrap combine #f)
         (define-syntax-via-derived for-values fold-for-values/derived fold-bind wrap rhs-wrap combine #t)
         (define-syntax-via-derived for* fold-for*/derived fold-bind wrap rhs-wrap combine #f)
         (define-syntax-via-derived for*-values fold-for*-values/derived fold-bind wrap rhs-wrap combine #t))]))

  (define-syntax (fold-for stx)
    (syntax-case stx ()
      [(_ . rest) (quasisyntax/loc stx (fold-for/derived #,stx . rest))]))
  (define-syntax (fold-for-values stx)
    (syntax-case stx ()
      [(_ . rest) (quasisyntax/loc stx (fold-for-values/derived #,stx . rest))]))
  (define-syntax (fold-for* stx)
    (syntax-case stx ()
      [(_ . rest) (quasisyntax/loc stx (fold-for*/derived #,stx . rest))]))
  (define-syntax (fold-for*-values stx)
    (syntax-case stx ()
      [(_ . rest) (quasisyntax/loc stx (fold-for*-values/derived #,stx . rest))]))

  (define-for-variants (for for-values for* for*-values)
    ([fold-var (void)]) 
    (lambda (x) x)
    (lambda (x) x)
    (lambda (x) `(,#'begin ,x ,#'(void))))
  
  (define-for-variants (list-for list-for-values list-for* list-for*-values)
    ([fold-var null])
    (lambda (x) `(,#'reverse ,x))
    (lambda (x) x)
    (lambda (x) `(,#'cons ,x ,#'fold-var)))
  
  (define-for-syntax (make-lists-for-values fold-for-id)
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
                         [fold-for fold-for-id]
                         [orig-stx stx])
             #'(let-values ([(id ...)
                             (fold-for orig-stx ([id null] ...) bindings
                               (let-values ([(id2 ...) (let ()
                                                         expr1
                                                         expr ...)])
                                 (values* (cons id2 id) ...)))])
                 (values* (reverse id) ...))))])))
  
  (define-syntax lists-for (make-lists-for-values #'fold-for/derived))
  (define-syntax lists-for-values (make-lists-for-values #'fold-for-values/derived))
  (define-syntax lists-for* (make-lists-for-values #'fold-for*/derived))
  (define-syntax lists-for*-values (make-lists-for-values #'fold-for*-values/derived))
  
  (define-for-variants (and-for and-for-values and-for* and-for*-values)
    ([result #t])
    (lambda (x) x)
    (lambda (rhs) #`(stop-after-gen #,rhs (lambda x (not result))))
    (lambda (x) x))
  
  (define-for-variants (or-for or-for-values or-for* or-for*-values)
    ([result #f])
    (lambda (x) x)
    (lambda (rhs) #`(stop-after-gen #,rhs (lambda x result)))
    (lambda (x) x))

  (define-for-variants (first-for first-for-values first-for* first-for*-values)
    ([val #f][stop? #f])
    (lambda (x) #`(let-values ([(val _) #,x]) val))
    (lambda (rhs) #`(stop-after-gen #,rhs (lambda x stop?)))
    (lambda (x) #`(values #,x #t)))
  
  (define-for-variants (last-for last-for-values last-for* last-for*-values)
    ([result #f])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x) x))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  specific generators

  (define-generator-syntax *range
    #'range
    (lambda (orig-stx stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ a b step)] #`[(id)
                                  (:do-gen
                                   ;; outer bindings:
                                   ([(start) a] [(end) b] [(inc) step])
                                   ;; outer check:
                                   (void)
#;
                                   (unless (and (real? a) (real? b) (real? inc))
                                     ;; let `range' report the error:
                                     (range start end inc))
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

  (define-generator-syntax *nat-gen
    #'nat-gen
    (lambda (orig-stx stx)
      (syntax-case stx ()
        [[(id) (_)] #`[(id)
                       (:do-gen
                        ;; outer bindings:
                        ()
                        ;; outer check:
                        (void)
                        ;; loop bindings:
                        ([pos 0])
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
          [_else #f])))

  (define-generator-syntax *list->gen
    #'list->gen
    (lambda (orig-stx stx)
      (syntax-case stx ()
        [((id) (_ lst-expr))
         #'[(id)
            (:do-gen
             ;;outer bindings
             ([(lst) lst-expr])
             ;; outer check
             (unless (list? lst) (list->gen lst))
             ;; loop bindings
             ([lst lst])
             ;; pos check
             (pair? lst)
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
                                      vector->gen-id
                                      vector-ref-id)
     (lambda (orig-stx stx)
       (with-syntax ([vector? vector?-id]
                     [vector->gen vector->gen-id]
                     [vector-length vector-length-id]
                     [vector-ref vector-ref-id])
         (syntax-case stx ()
           [((id) (_ vec-expr))
            #'[(id)
               (:do-gen
                ;;outer bindings
                ([(vec len) (let ([vec vec-expr])
                              (unless (vector? vec)
                                (vector->gen vec))
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
  
  (define-generator-syntax *vector->gen
    #'vector->gen
    (vector-like-gen #'vector?
                     #'vector-length
                     #'vector->gen
                     #'vector-ref))
  
  (define-generator-syntax *string->gen
    #'string->gen
    (vector-like-gen #'string?
                      #'string-length
                      #'string->gen
                      #'string-ref))
  
  (define-generator-syntax *bytes->gen
    #'bytes->gen
    (vector-like-gen #'bytes?
                     #'bytes-length
                     #'bytes->gen
                     #'bytes-ref))
  
  (define-generator-syntax *index-gen
    #'index-gen
    (lambda (orig-stx stx)
      (syntax-case stx ()
        [((id1 id2) (_ gen-expr))
         #'[(id1 id2) (parallel-gen gen-expr (*nat-gen))]]))))
