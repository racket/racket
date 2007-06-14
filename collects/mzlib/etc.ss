
(module etc mzscheme

  (require (lib "main-collects.ss" "setup")
           "kw.ss")

  (require-for-syntax (lib "kerncase.ss" "syntax")
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax")
                      (lib "context.ss" "syntax")
                      (lib "main-collects.ss" "setup")
                      "private/stxset.ss")


  (provide true false
           boolean=? symbol=?
           identity
           compose

           build-string
           build-vector
           build-list

           loop-until

           opt-lambda

           local
           recur
           rec
           evcase
           nor
           nand
           let+

           namespace-defined?
           this-expression-source-directory
           this-expression-file-name
           define-syntax-set

           hash-table

           begin-with-definitions

           begin-lifted)

  (define true #t)
  (define false #f)

  (define identity (lambda (x) x))

  (define compose
    (case-lambda
      [(f) (if (procedure? f) f (raise-type-error 'compose "procedure" f))]
      [(f g)
       (let ([f (compose f)]
             [g (compose g)])
         (if (eqv? 1 (procedure-arity f)) ; optimize: don't use call-w-values
             (if (eqv? 1 (procedure-arity g)) ; optimize: single arity everywhere
                 (lambda (x) (f (g x)))
                 (lambda args (f (apply g args))))
             (if (eqv? 1 (procedure-arity g)) ; optimize: single input
                 (lambda (a)
                   (call-with-values
                    (lambda () (g a))
                    f))
                 (lambda args
                   (call-with-values
                    (lambda () (apply g args))
                    f)))))]
      [(f . more)
       (let ([m (apply compose more)])
         (compose f m))]))


  (define (build-string n fcn)
    (unless (and (integer? n) (exact? n) (>= n 0))
      (error 'build-string  "~s must be an exact integer >= 0"  n))
    (unless (procedure? fcn)
      (error 'build-string  "~s must be a procedure"  fcn))
      (let ([str (make-string n)])
        (let loop ((i 0))
          (if (= i n)
            str
            (begin (string-set! str i (fcn i)) (loop (add1 i)))))))

  ;; (build-vector n f) returns a vector 0..n-1 where the ith element is (f i).
  ;; The eval order is guaranteed to be: 0, 1, 2, ..., n-1.
  ;; eg: (build-vector 4 (lambda (i) i)) ==> #4(0 1 2 3)

  (define (build-vector n fcn)
    (unless (and (integer? n) (exact? n) (>= n 0))
      (error 'build-vector  "~s must be an exact integer >= 0"  n))
    (unless (procedure? fcn)
      (error 'build-vector  "~s must be a procedure"  fcn))
    (let ([vec (make-vector n)])
      (let loop ((i 0))
        (if (= i n)
          vec
          (begin (vector-set! vec i (fcn i)) (loop (add1 i)))))))

  (define (build-list n fcn)
    (unless (and (integer? n) (exact? n) (>= n 0))
      (error 'build-list  "~s must be an exact integer >= 0"  n))
    (unless (procedure? fcn)
      (error 'build-list  "~s must be a procedure"  fcn))
    (if (zero? n)
      '()
      (let ([head (list (fcn 0))])
        (let loop ([i 1] [p head])
          (if (= i n)
            head
            (begin (set-cdr! p (list (fcn i)))
                   (loop (add1 i) (cdr p))))))))

  (define (loop-until start done? next body)
    (let loop ([i start])
      (unless (done? i)
        (body i)
        (loop (next i)))))

  (define (boolean=? x y)
    (unless (and (boolean? x) (boolean? y))
      (raise-type-error 'boolean=? "boolean" (if (boolean? x) y x)))
    (eq? x y))

  (define (symbol=? x y)
    (unless (and (symbol? x) (symbol? y))
      (raise-type-error 'symbol=? "symbol" (if (symbol? x) y x)))
    (eq? x y))

 (define-syntax (opt-lambda stx)
   (with-syntax ([name (or (syntax-local-infer-name stx)
                           (quote-syntax opt-lambda-proc))])
     (syntax-case stx ()
       [(_ args body1 body ...)
        (let ([clauses (let loop ([pre-args null]
                                  [args (syntax args)]
                                  [needs-default? #f])
                         (syntax-case args ()
                           [id
                            (identifier? (syntax id))
                            (with-syntax ([(pre-arg ...) pre-args])
                              (syntax ([(pre-arg ... . id)
                                        body1 body ...])))]
                           [()
                            (with-syntax ([(pre-arg ...) pre-args])
                              (syntax ([(pre-arg ...)
                                        body1 body ...])))]
                           [(id . rest)
                            (identifier? (syntax id))
                            (begin
                              (when needs-default?
                                (raise-syntax-error
                                 #f "default value missing" stx (syntax id)))
                              (loop (append pre-args (list (syntax id)))
                                    (syntax rest)
                                    #f))]
                           [([id default] . rest)
                            (identifier? (syntax id))
                            (with-syntax ([rest (loop (append pre-args (list (syntax id)))
                                                      (syntax rest)
                                                      #t)]
                                          [(pre-arg ...) pre-args])
                              (syntax ([(pre-arg ...) (name pre-arg ... default)]
                                       . rest)))]
                           [(bad . rest)
                            (raise-syntax-error
                             #f
                             "not an identifier or identifier with default"
                             stx
                             (syntax bad))]
                           [else
                            (raise-syntax-error
                             #f "bad identifier sequence" stx (syntax args))]))])
          (with-syntax ([clauses clauses])
            (syntax/loc stx
              (letrec ([name (case-lambda . clauses)]) name))))])))

 (define-syntax (local stx)
   (syntax-case stx ()
     [(_ (defn ...) body1 body ...)
      (let ([defs (let ([expand-context (generate-expand-context)])
                    (let loop ([defns (syntax->list (syntax (defn ...)))])
                      (apply
                       append
                       (map
                        (lambda (defn)
                          (let ([d (local-expand
                                    defn
                                    expand-context
                                    (kernel-form-identifier-list
                                     (quote-syntax here)))]
                                [check-ids (lambda (ids)
                                             (for-each
                                              (lambda (id)
                                                (unless (identifier? id)
                                                  (raise-syntax-error
                                                   #f
                                                   "not an identifier for definition"
                                                   stx
                                                   id)))
                                              ids))])
                            (syntax-case d (define-values define-syntaxes begin)
                              [(begin defn ...)
                               (loop (syntax->list (syntax (defn ...))))]
                              [(define-values (id ...) body)
                               (begin
                                 (check-ids (syntax->list (syntax (id ...))))
                                 (list d))]
                              [(define-values . rest)
                               (raise-syntax-error
                                #f "ill-formed definition" stx d)]
                              [(define-syntaxes (id ...) body)
                               (begin
                                 (check-ids (syntax->list (syntax (id ...))))
                                 (list d))]
                              [(define-syntaxes . rest)
                               (raise-syntax-error
                                #f "ill-formed definition" stx d)]
                              [_else
                               (raise-syntax-error
                                #f "not a definition" stx defn)])))
                        defns))))])
        (let ([ids (apply append
                          (map
                           (lambda (d)
                             (syntax-case d ()
                               [(_ ids . __) (syntax->list (syntax ids))]))
                           defs))])
          (let ([dup (check-duplicate-identifier ids)])
            (when dup
              (raise-syntax-error #f "duplicate identifier" stx dup)))
          (with-syntax ([(def ...) defs])
            (syntax/loc stx
              (let () def ... (let () body1 body ...))))))]
     [(_ x body1 body ...)
      (raise-syntax-error #f "not a definition sequence" stx (syntax x))]))

  ;; recur is another name for 'let' in a named let
  (define-syntax (recur stx)
    (syntax-case stx ()
      [(_ . rest) (syntax/loc stx (let . rest))]))

  ;; define a recursive value
  ;; implementation by Jens Axel Soegaard
  (define-syntax (rec stx)
    (syntax-case stx ()
      [(rec id expr)
       (identifier? #'id)
       #`(letrec ((id expr))
           #,(syntax-property #'id 'inferred-name (syntax-e #'id)))]
      [(rec (name id ...) body ...)
       (andmap identifier? (syntax->list #'(name id ...)))
       #`(letrec ((name (lambda (id ...) body ...)))
           #,(syntax-property #'name 'inferred-name (syntax-e #'name)))]
      [(rec (name id ... . did) body ...)
       (andmap identifier? (syntax->list #'(name id ...)))
       #`(letrec ((name (lambda (id ... . did) body ...)))
           #,(syntax-property #'name 'inferred-name (syntax-e #'name)))]
      [_
       (raise-syntax-error
        #f "expects either a variable followed by an expresion, or a (possibly dotted) sequence of variables followed by a body" stx)]))

  (define-syntax (evcase stx)
    (syntax-case stx ()
      [(_ val [test body ...] ...)
       (let ([tests (syntax->list (syntax (test ...)))])
         (with-syntax ([(a-test ...)
                        (map (lambda (t)
                               (syntax-case t (else)
                                 [else (syntax #t)]
                                 [_else (with-syntax ([t t])
                                          (syntax (eqv? evcase-v t)))]))
                             tests)])
           ;; Make sure else is last:
           (unless (null? tests)
             (let loop ([tests tests])
               (unless (null? (cdr tests))
                 (when (and (identifier? (car tests))
                            (module-identifier=? (quote-syntax else)
                                                 (car tests)))
                   (raise-syntax-error
                    #f "else is not in last clause" stx (car tests)))
                 (loop (cdr tests)))))
           (syntax/loc stx
             (let ([evcase-v val])
               (cond [a-test (begin body ...)]
                     ...)))))]
      [(_ val something ...)
       ;; Provide a good error message:
       (for-each
        (lambda (s)
          (syntax-case s ()
            [(t a ...) (raise-syntax-error #f "invalid clause" stx s)]))
        (syntax->list (syntax (something ...))))]))

  (define-syntax (nor stx)
    (syntax-case stx ()
      [(_ expr ...) (syntax/loc stx (not (or expr ...)))]))

  (define-syntax (nand stx)
    (syntax-case stx ()
      [(_ expr ...) (syntax/loc stx (not (and expr ...)))]))

  (define-syntax (let+ stx)
    (syntax-case stx ()
      [(_ [clause ...] body1 body ...)
       (let ([clauses (syntax->list (syntax (clause ...)))]
             [bad (lambda (c n)
                    (raise-syntax-error
                     #f (format "illegal use of ~a for a clause" n) stx c))]
             [var? (lambda (x)
                     (or (identifier? x)
                         (let ([l (syntax->list x)])
                           (and l
                                (pair? l)
                                (eq? (syntax-e (car l)) 'values)
                                (andmap identifier? (cdr l))))))]
             [normal-var (lambda (x)
                           (if (identifier? x)
                             (list x)
                             (cdr (syntax-e x))))])
         ;; syntax checks
         (for-each
          (lambda (clause)
            (syntax-case* clause (val rec vals recs _)
                          (lambda (a b) (eq? (syntax-e b) (syntax-e a)))
              [(val var expr)
               (var? (syntax var))
               'ok]
              [(rec var expr)
               (var? (syntax var))
               'ok]
              [(vals (var expr) ...)
               (andmap var? (syntax->list (syntax (var ...))))
               'ok]
              [(recs (var expr) ...)
               (andmap var? (syntax->list (syntax (var ...))))
               'ok]
              [(_ expr0 expr ...)
               'ok]
              [(val . __) (bad clause "val")]
              [(rec . __) (bad clause "rec")]
              [(vals . __) (bad clause "vals")]
              [(recs . __) (bad  clause"recs")]
              [(_ . __) (bad clause "_")]
              [_else (raise-syntax-error #f "bad clause" stx clause)]))
          clauses)
         ;; result
         (let loop ([clauses clauses])
           (if (null? clauses)
             (syntax (let () body1 body ...))
             (with-syntax ([rest (loop (cdr clauses))])
               (syntax-case* (car clauses) (val rec vals recs _)
                             (lambda (a b) (eq? (syntax-e b) (syntax-e a)))
                 [(val var expr)
                  (with-syntax ([vars (normal-var (syntax var))])
                    (syntax (let-values ([vars expr]) rest)))]
                 [(rec var expr)
                  (with-syntax ([vars (normal-var (syntax var))])
                    (syntax (letrec-values ([vars expr]) rest)))]
                 [(vals (var expr) ...)
                  (with-syntax ([(vars ...)
                                 (map normal-var
                                      (syntax->list (syntax (var ...))))])
                    (syntax (let-values ([vars expr] ...) rest)))]
                 [(recs (var expr) ...)
                  (with-syntax ([(vars ...)
                                 (map normal-var
                                      (syntax->list (syntax (var ...))))])
                    (syntax (letrec-values ([vars expr] ...) rest)))]
                 [(_ expr0 expr ...)
                  (syntax (begin expr0 expr ... rest))])))))]))

  (define ns-undefined (gensym))

  (define (namespace-defined? n)
    (unless (symbol? n)
      (raise-type-error 'namespace-defined? "symbol" n))
    (not (eq? (namespace-variable-value n #t (lambda () ns-undefined))
              ns-undefined)))

  (define (extract-module-directory stx)
    (let ([srcmod (let ([mpi (syntax-source-module stx)])
                    (if (module-path-index? mpi)
                        (module-path-index-resolve mpi)
                        mpi))])
      (let ([str (symbol->string srcmod)])
        (and ((string-length str) . > . 1)
             (char=? #\, (string-ref str 0))
             (let ([path (bytes->path (string->bytes/latin-1 (substring str 1)))])
               (let-values ([(base name dir?) (split-path path)])
                 (and (path? base)
                      base)))))))

  (define-syntax (this-expression-source-directory stx)
    (syntax-case stx ()
      [(_)
       (let ([source-path
              (let* ([source (syntax-source stx)]
                     [source (and (path? source) source)]
                     [local (or (current-load-relative-directory) (current-directory))]
                     [dir (path->main-collects-relative
                           (or (and source (file-exists? source)
                                    (let-values ([(base file dir?)
                                                  (split-path source)])
                                      (and (path? base)
                                           (path->complete-path base local))))
                               local))])
                (if (and (pair? dir) (eq? 'collects (car dir)))
                    (with-syntax ([d dir])
                      (syntax/loc stx (main-collects-relative->path 'd)))
                    (with-syntax ([d (if (bytes? dir) dir (path->bytes dir))])
                      (syntax/loc stx (bytes->path d)))))])
         (let ([mpi (syntax-source-module stx)])
           (if mpi
               (quasisyntax/loc stx
                 (or (extract-module-directory (quote-syntax #,stx))
                     #,source-path))
               source-path)))]))

  (define-syntax (this-expression-file-name stx)
    (syntax-case stx ()
      [(_)
       (let* ([f (syntax-source stx)]
              [f (and f (path? f) (file-exists? f)
                      (let-values ([(base file dir?) (split-path f)]) file))])
         (if f
           (with-syntax ([f (path->bytes f)]) #'(bytes->path f))
           #'#f))]))

  ;; This is a macro-generating macro that wants to expand
  ;; expressions used in the generated macro. So it's weird,
  ;; and we put much of the work in a helper macro,
  ;; `finish-syntax-set'.
  (define-syntax (define-syntax-set stx)
    (syntax-case stx ()
      [(_ (id ...) defn ...)
       (let ([ids (syntax->list (syntax (id ...)))])
         ;; Check ids ------------------------------
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error
                        #f
                        "not an identifier or two identifier in parentheses"
                        stx
                        id)))
                   ids)
         (let ([dup (check-duplicate-identifier ids)])
           (when dup
             (raise-syntax-error
              #f
              "duplicate identifier"
              stx
              dup)))

         ;; We'd like to check the `defns', but that requires
         ;; and expansion in a different phase. So we move
         ;; into that phase using `finish-syntax-set':
         (with-syntax ([orig-stx stx])
           (syntax/loc stx
             (define-syntaxes (id ...)
               (finish-syntax-set orig-stx)))))]))

  (define-syntax (hash-table stx)
    (syntax-case stx (quote)
      [(_ x ...)
       (let loop ([xs #'(x ...)] [flags '()])
         (syntax-case xs (quote)
           [('flag x ...) (loop #'(x ...) (cons #''flag flags))]
           [([key val] ...)
            (with-syntax ([(flag ...) (reverse flags)])
              (syntax/loc stx
                (let ([ht (make-hash-table flag ...)])
                  (hash-table-put! ht key val) ...
                  ht)))]
           [_else (raise-syntax-error 'hash-table "bad syntax" stx)]))]))

  (define-syntax (begin-with-definitions stx)
    ;; Body can have mixed exprs and defns. Wrap expressions with
    ;; `(define-values () ... (values))' as needed, and add a (void)
    ;; at the end if needed.
    (let* ([def-ctx (syntax-local-make-definition-context)]
           [ctx (list (gensym 'intdef))]
           [kernel-forms (kernel-form-identifier-list
                          (quote-syntax here))]
           [init-exprs (let ([v (syntax->list stx)])
                         (unless v
                           (raise-syntax-error #f "bad syntax" stx))
                         (cdr v))]
           [exprs (let loop ([exprs init-exprs])
                    (apply
                     append
                     (map (lambda (expr)
                            (let ([expr (local-expand
                                         expr
                                         ctx
                                         kernel-forms
                                         def-ctx)])
                              (syntax-case expr (begin define-syntaxes define-values)
                                [(begin . rest)
                                 (loop (syntax->list #'rest))]
                                [(define-syntaxes (id ...) rhs)
                                 (andmap identifier? (syntax->list #'(id ...)))
                                 (with-syntax ([rhs (local-transformer-expand
                                                     #'rhs
                                                     'expression
                                                     null)])
                                   (syntax-local-bind-syntaxes
                                    (syntax->list #'(id ...))
                                    #'rhs def-ctx)
                                   (list #'(define-syntaxes (id ...) rhs)))]
                                [(define-values (id ...) rhs)
                                 (andmap identifier? (syntax->list #'(id ...)))
                                 (let ([ids (syntax->list #'(id ...))])
                                   (syntax-local-bind-syntaxes ids #f def-ctx)
                                   (list expr))]
                                [else
                                 (list expr)])))
                          exprs)))])
      (let loop ([exprs exprs]
                 [prev-stx-defns null]
                 [prev-defns null]
                 [prev-exprs null])
        (cond
          [(null? exprs)
           #`(letrec-syntaxes+values
              #,(map stx-cdr (reverse prev-stx-defns))
              #,(map stx-cdr (reverse prev-defns))
              #,@(if (null? prev-exprs)
                   (list #'(void))
                   (reverse prev-exprs)))]
          [(and (stx-pair? (car exprs))
                (identifier? (stx-car (car exprs)))
                (module-identifier=? #'define-syntaxes (stx-car (car exprs))))
           (loop (cdr exprs)
                 (cons (car exprs) prev-stx-defns)
                 prev-defns
                 prev-exprs)]
          [(and (stx-pair? (car exprs))
                (identifier? (stx-car (car exprs)))
                (module-identifier=? #'define-values (stx-car (car exprs))))
           (loop (cdr exprs)
                 prev-stx-defns
                 (cons (car exprs)
                       (append
                        (map (lambda (expr)
                               #`(define-values () (begin #,expr (values))))
                             prev-exprs)
                        prev-defns))
                 null)]
          [else (loop (cdr exprs)
                      prev-stx-defns
                      prev-defns
                      (cons (car exprs) prev-exprs))]))))

  (define-syntax (begin-lifted stx)
    (syntax-case stx ()
      [(_ expr0 expr ...)
       (let ([name (syntax-local-name)])
         (if name
           (with-syntax ([name name])
             (syntax-local-lift-expression
              #'(let ([name (begin expr0 expr ...)])
                  name)))
           (syntax-local-lift-expression
            #'(begin expr0 expr ...))))])))
