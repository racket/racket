#lang racket/base
(require racket/cmdline
         racket/pretty
         racket/match
         racket/file
         racket/extflonum
         "../schemify/schemify.rkt"
         "../schemify/known.rkt"
         "../schemify/lift.rkt"
         "../schemify/reinfer-name.rkt"
         "../schemify/wrap.rkt"
         "known.rkt")

(define skip-export? #f)
(define unsafe-mode? #f)

(define-values (in-file out-file)
  (command-line
   #:once-each
   [("--skip-export") "Don't generate an `export` form"
    (set! skip-export? #t)]
   [("--unsafe") "Compile for unsafe mode"
    (set! unsafe-mode? #t)]
   #:args
   (in-file out-file)
   (values in-file out-file)))

(define content (call-with-input-file* in-file read))
(define exports (caddr content))
(define l (cdddr content))

(let loop ([l l])
  (cond
    [(eq? l 'make-optional-keyword-procedure)
     (error "keyword residual `make-optional-keyword-procedure` appears in .rktl")]
    [(pair? l)
     (loop (car l))
     (loop (cdr l))]))

(define lifts (make-hash))
(define ordered-lifts null)

(define (lift-set! k v)
  (unless (hash-ref lifts k #f)
    (hash-set! lifts k v)
    (set! ordered-lifts (cons k ordered-lifts))))

;; Ad hoc patterns to deal with a special case in "expander.rktl":
(define (quote? v)
  (and (pair? v)
       (eq? (car v) 'quote)
       (pair? (cdr v))
       (null? (cddr v))))
(define (nested-hash? v)
  (and (pair? v)
       (eq? #f (car v))
       (hash? (cdr v))))
(define (list-of-keywords? v)
  (and (pair? v)
       (list? v)
       (andmap keyword? v)))

(define generated-names (make-hasheq))

;; To improve determinism, base a generated name for a lifted literal
;; on the printed form of the literal:
(define (generate-name v type #:d [d 0])
  (define o (open-output-bytes))
  (fprintf o "~s" v)
  (define sum (for/sum ([i (in-bytes (sha1-bytes (get-output-bytes o)))])
                i))
  (define name (string->symbol (format "~a~a" type (+ sum d))))
  (if (hash-ref generated-names name #f)
      (generate-name v type #:d (add1 d))
      (begin
        (hash-set! generated-names name #t)
        name)))

;; Gather all literal regexps and hash tables
(define (lift v)
  (cond
   [(or (regexp? v) (byte-regexp? v))
    (define s (generate-name v 'rx))
    (lift-set! v s)]
   [(or (pregexp? v) (byte-pregexp? v))
    (define s (generate-name v 'px))
    (lift-set! v s)]
   [(hash? v)
    (define s (generate-name v 'hash))
    (lift-set! v s)]
   [(and (quote? v)
         (nested-hash? (cadr v)))
    (define s (generate-name v 'nhash))
    (lift-set! (cadr v) s)]
   [(keyword? v)
    (define s (generate-name v 'kw))
    (lift-set! v s)]
   [(and (quote? v)
         (list-of-keywords? (cadr v)))
    (define s (generate-name v 'kws))
    (lift-set! (cadr v) s)]
   [(and (quote? v)
         (extflonum? (cadr v)))
    (define s (generate-name v 'extfl))
    (lift-set! (cadr v) s)]
   [(pair? v)
    (lift (car v))
    (lift (cdr v))]))

(lift l)

(define prim-knowns (get-prim-knowns))
(define primitives (get-primitives))
(check-known-values prim-knowns primitives)

;; Convert:
(define schemified-body
  (let ()
    (define bodys (recognize-inferred-names l))
    (printf "Schemify...\n")
    (define body
      (time
       (schemify-body bodys prim-knowns primitives
                      #hasheq()
                      ;; map exports to #f to indicate which are exported
                      ;; without triggering most export machinery:
                      (for/hasheq ([ex exports])
                        (if (pair? ex)
                            (values (car ex) #f)
                            (values ex #f)))
                      'system ; target
                      unsafe-mode?
                      #t    ; no-prompt?
                      #f))) ; explicit-unnamed?
    body))

;; ----------------------------------------

;; Set a hook to redirect literal regexps and
;; hash tables to lifted bindings
(pretty-print-size-hook
 (lambda (v display? out)
   (cond
     [(and (pair? v)
           (pair? (cdr v))
           (eq? 'quote (car v))
           (or (regexp? (cadr v))
               (byte-regexp? (cadr v))
               (pregexp? (cadr v))
               (byte-pregexp? (cadr v))
               (hash? (cadr v))
               (nested-hash? (cadr v))
               (keyword? (cadr v))
               (list-of-keywords? (cadr v))
               (extflonum? (cadr v))))
      10]
     [(and (pair? v)
           (pair? (cdr v))
           (eq? 'quote (car v))
           (void? (cadr v)))
      6]
     [(bytes? v) (* 3 (bytes-length v))]
     [(and (symbol? v) (regexp-match? #rx"#" (symbol->string v)))
      (+ 2 (string-length (symbol->string v)))]
     [(char? v) 5]
     [(single-flonum? v) 5]
     [(or (keyword? v)
          (regexp? v)
          (pregexp? v)
          (hash? v))
      (error 'lift "value that needs lifting is in an unrecognized context: ~v" v)]
     [else #f])))

;; This hook goes with `pretty-print-size-hook`
(pretty-print-print-hook
 (lambda (v display? out)
   (cond
     [(and (pair? v)
           (eq? 'quote (car v))
           (or (regexp? (cadr v))
               (byte-regexp? (cadr v))
               (pregexp? (cadr v))
               (byte-pregexp? (cadr v))
               (hash? (cadr v))
               (nested-hash? (cadr v))
               (keyword? (cadr v))
               (list-of-keywords? (cadr v))
               (extflonum? (cadr v))))
      (write (hash-ref lifts (cadr v)) out)]
     [(and (pair? v)
           (pair? (cdr v))
           (eq? 'quote (car v))
           (void? (cadr v)))
      (write '(void) out)]
     [(bytes? v)
      (display "#vu8")
      (write (bytes->list v) out)]
     [(symbol? v)
      (write-string (format "|~a|" v) out)]
     [(char? v)
      (write-string (format "#\\x~x" (char->integer v)) out)]
     [(single-flonum? v)
      (write (real->double-flonum v) out)]
     [else #f])))

;; ----------------------------------------

(define-struct env (locals used))

(define (add-new-names s env #:top? [top? #f] #:count-from [count-from 0])
  (cond
    [(symbol? s)
     (define str (symbol->string s))
     (cond
       [(regexp-match-positions (if top? #rx"(?<![.$])[0-9]+$" #rx"(_[0-9]+)+$") str)
        => (lambda (m)
             (define base (substring str 0 (caar m)))
             (let loop ([i count-from])
               (define sym (string->symbol (format "~a_~a" base i)))
               (if (hash-ref (env-used env) sym #f)
                   (loop (add1 i))
                   (make-env (hash-set (env-locals env) s sym)
                             (hash-set (env-used env) sym #t)))))]
       [else (make-env (hash-set (env-locals env) s s) (env-used env))])]
    [(pair? s) (add-new-names (cdr s) (add-new-names (car s) env))]
    [(null? s) env]
    [else (error 'convert "unexpected vars ~s" s)]))

(define (rename s env)
  (cond
    [(symbol? s) (hash-ref (env-locals env) s s)]
    [(pair? s) (cons (rename (car s) env) (rename (cdr s) env))]
    [(null? s) '()]
    [else (error 'convert "unexpected vars ~s" s)]))

;; Try renaming top-level `define`s that have numbers at the end,
;; since those are likely to be generated from the macro-expansion
;; counter. Replace the number part with one based on the shape of
;; the right-hand side, so that numbers don't just shift up and
;; down with changes.
(define (get-top-env exports es)
  (define (count-symbols e ht)
    (let loop ([e e] [ht ht])
      (cond
        [(wrap? e) (loop (unwrap e) ht)]
        [(symbol? e) (hash-set ht e (add1 (hash-ref ht e 0)))]
        [(pair? e) (loop (cdr e) (loop (car e) ht))]
        [else ht])))

  (define export-counts (count-symbols exports #hasheq()))
  (define counts (count-symbols es export-counts))

  (define (expression-shape e)
    (define o (open-output-bytes))
    (define reshaped-e
      (let loop ([e e])
        (cond
          [(wrap? e) (loop (unwrap e))]
          [(pair? e) (cons (loop (car e)) (loop (cdr e)))]
          [(symbol? e) (string->symbol (regexp-replace #rx"[0-9]+$" (symbol->string e) ""))]
          [else e])))
    (write reshaped-e o)
    (for/sum ([i (in-bytes (sha1-bytes (get-output-bytes o)))])
      i))

  (for/fold ([env (make-env #hasheq() #hasheq())]) ([e (in-list es)])
    (let loop ([e e] [env env])
      (match e
        [(? wrap?) (loop (unwrap e) env)]
        [`(define ,id ,rhs)
         (if (or (eqv? 1 (hash-ref counts id))
                 (and (wrap-property rhs 'inferred-name)
                      (not (hash-ref export-counts id #f))))
             (add-new-names id env #:top? #t #:count-from (expression-shape rhs))
             env)]
        [`(define-values ,ids ,rhs)
         (define count-from (expression-shape rhs))
         (let loop ([ids ids] [env env])
           (cond
             [(null? ids) env]
             [else
              (define id (car ids))
              (loop (cdr ids)
                    (if (eqv? 1 (hash-ref counts id))
                        (add-new-names id env #:top? #t #:count-from count-from)
                        env))]))]
        [`(begin ,es ...)
         (for/fold ([env env]) ([e (in-list es)])
           (loop e env))]
        [else env]))))

;; Simplify local variables (which had been made globally unique at
;; one point) to improve consistency after small changes
(define (rename-locals e top-env)
  (define (loop e env)
    (match e
      [(? wrap?) (reannotate e (loop (unwrap e) env))]
      [`(define ,id ,rhs)
       `(define ,(rename id env) ,(loop rhs env))]
      [`(define-values ,ids ,rhs)
       `(define-values ,(rename ids env) ,(loop rhs env))]
      [`(lambda ,formals ,body ...)
       (define new-env (add-new-names formals env))
       `(lambda ,(rename formals new-env) ,@(rename-body body new-env))]
      [`(lambda . ,_) (error 'convert "unexpected: ~s" e)]
      [`(case-lambda [,formalss ,bodys ...] ...)
       `(case-lambda
          ,@(for/list ([formals (in-list formalss)]
                       [body (in-list bodys)])
              (define new-env (add-new-names formals env))
              `[,(rename formals new-env) ,@(rename-body body new-env)]))]
      [`(case-lambda . ,_) (error 'convert "unexpected: ~s" e)]
      [`(let . ,_) (rename-let #f e env)]
      [`(letrec . ,_) (rename-let #t e env)]
      [`(letrec* . ,_) (rename-let #t e env)]
      [`(quote ,_) e]
      [`(,es ...)
       (for/list ([e (in-list es)])
         (loop e env))]
      [_ (hash-ref (env-locals env) e e)]))

  (define (rename-body body new-env)
    (for/list ([e (in-list body)])
      (loop e new-env)))

  (define (rename-let rec? e env)
    (match e
      [`(,form ([,vars ,rhss] ...) ,body ...)
       (define new-env (add-new-names vars env))
       `(,form ,(for/list ([var (in-list vars)]
                           [rhs (in-list rhss)])
                  `[,(rename var new-env) ,(loop rhs (if rec? new-env env))])
               ,@(rename-body body new-env))]
      [_ (error 'convert "unexpected: ~s" e)]))

  (loop e top-env))

;; ----------------------------------------

;; Convert 'inferred-name properties to `#%name` forms
(define (rename-functions e)
  (cond
    [(wrap? e)
     (cond
       [(wrap-property e 'inferred-name)
        => (lambda (name)
             `(#%name ,name ,(rename-functions (unwrap e))))]
       [else
        (rename-functions (unwrap e))])]
    [(not (pair? e)) e]
    [else (cons (rename-functions (car e))
                (rename-functions (cdr e)))]))

;; ----------------------------------------

(make-parent-directory* out-file)

(with-handlers ([void (lambda (exn)
                        (when (file-exists? out-file)
                          (with-handlers ([void (lambda (exn)
                                                  (log-error "delete failed: ~s" exn))])
                            (delete-file out-file)))
                        (raise exn))])
  (with-output-to-file
   out-file
   #:exists 'truncate
   (lambda ()
     (unless skip-export?
       ;; Write out exports
       (pretty-write
        `(export (rename ,@exports))))
     ;; Write out lifted regexp and hash-table literals
     (for ([k (in-list (reverse ordered-lifts))])
       (define v (hash-ref lifts k))
       (pretty-write
        `(define ,v
          ,(let loop ([k k])
             (cond
              [(or (regexp? k)
                   (byte-regexp? k))
               `(,(cond [(byte-regexp? k)  'byte-regexp]
                        [(byte-pregexp? k) 'byte-pregexp]
                        [(pregexp? k)      'pregexp]
                        [else              'regexp])
                 ,(object-name k))]
              [(hash? k)
               `(,(cond
                   [(hash-equal? k) 'hash]
                   [(hash-eqv? k) 'hasheqv]
                   [else 'hasheq])
                 ,@(apply append
                          (hash-map k
                                    (lambda (k v)
                                      (list `(quote ,k)
                                            `(quote ,v)))
                                    #t)))]
              [(pair? k)
               `(cons ,(loop (car k)) ,(loop (cdr k)))]
              [(keyword? k)
               `(string->keyword ,(keyword->string k))]
              [(null? k) ''()]
              [(extflonum? k) `(string->number ,(format "~a" k) 10 'read)]
              [else k])))))

     ;; Write out converted forms
     (let ([top-env (get-top-env exports schemified-body)])
       (for ([v (in-list schemified-body)])
         (unless (equal? v '(void))
           (let loop ([v v])
             (match v
               [`(begin ,vs ...)
                (for-each loop vs)]
               [_
                (pretty-write (rename-functions (rename-locals v top-env)))]))))))))
