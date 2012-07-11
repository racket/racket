#lang mzscheme
(require setup/main-collects
         racket/local
         racket/bool
         racket/block
         racket/private/this-expression-source-directory
         (only racket/function
               identity)
         (only racket/base
               build-string
               build-list
               build-vector
               compose)
         (rename racket/base base-else else))

(require-for-syntax syntax/name
                    setup/main-collects
                    "private/stxset.rkt")

(provide boolean=? symbol=?
         identity
         compose

         true false

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

         (rename block begin-with-definitions)

         begin-lifted)

(define (loop-until start done? next body)
  (let loop ([i start])
    (unless (done? i)
      (body i)
      (loop (next i)))))

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
     (andmap identifier? (syntax->list #'(name did id ...)))
     #`(letrec ((name (lambda (id ... . did) body ...)))
         #,(syntax-property #'name 'inferred-name (syntax-e #'name)))]
    [_
     (raise-syntax-error
      #f "expects either an identifier followed by an expresion, or a (possibly dotted) sequence of identifiers followed by a body" stx)]))

(define-syntax (evcase stx)
  (syntax-case stx ()
    [(_ val [test body ...] ...)
     (let ([tests (syntax->list (syntax (test ...)))])
       (with-syntax ([(a-test ...)
                      (map (lambda (t)
                             (syntax-case t (else base-else)
                               [else (syntax #t)]
                               [base-else (syntax #t)]
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
    (raise-argument-error 'namespace-defined? "symbol?" n))
  (not (eq? (namespace-variable-value n #t (lambda () ns-undefined))
            ns-undefined)))

(define-syntax (this-expression-file-name stx)
  (syntax-case stx ()
    [(_ sub)
     (let ([stx #'sub])
       (let* ([f (syntax-source stx)]
              [f (and f (path? f) (file-exists? f)
                      (let-values ([(base file dir?) (split-path f)]) file))])
         (if f
             (with-syntax ([f (path->bytes f)]) #'(bytes->path f))
             #'#f)))]
    [(_) #`(this-expression-file-name #,stx)]))

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
           (raise-syntax-error #f "duplicate identifier" stx dup)))

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
          #'(begin expr0 expr ...))))]))
