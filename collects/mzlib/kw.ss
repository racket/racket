(module kw mzscheme

(require-for-syntax (lib "name.ss" "syntax"))

(begin-for-syntax ; -> configuration for lambda/kw
  ;; must appear at the end, each with exactly one variable
  (define rest-like-kwds '(#:rest #:body #:rest-keys #:all-keys #:other-keys))
  ;; also in the end, without variable, cannot have contradictions
  (define allow-other-keys-kwds '(#:allow-other-keys #:forbid-other-keys))
  (define allow-body-kwds '(#:allow-body #:forbid-body))
  ;; using any of these allows access to additional keys or to body, making the
  ;; default be to allow other keys or body
  (define other-keys-accessing '(#:rest #:rest-keys #:all-keys #:other-keys))
  (define body-accessing '(#:rest #:body #:rest-keys)))

(provide lambda/kw)
(define-syntax (lambda/kw stx)
  ;; --------------------------------------------------------------------------
  ;; easy syntax errors
  (define (serror sub fmt . args)
    (raise-syntax-error #f (apply format fmt args) stx sub))
  ;; contents of syntax
  (define (syntax-e* x) (if (syntax? x) (syntax-e x) x))
  ;; split a list of syntax objects based on syntax keywords:
  ;;   (x ... #:k1 ... #:k2 ... ...) --> ((x ...) (#:k1 ...) (#:k2 ...) ...)
  (define (split-by-keywords xs)
    (let loop ([xs xs] [cur '()] [r '()])
      (if (null? xs)
        (reverse! (cons (reverse! cur) r))
        (let ([x (car xs)])
          (if (keyword? (syntax-e* x))
            (loop (cdr xs) (list x) (cons (reverse! cur) r))
            (loop (cdr xs) (cons x cur) r))))))
  ;; process an optional argument spec, returns (<id> <default-expr>)
  (define (process-opt o)
    (syntax-case o ()
      [(var default) (identifier? #'var) (list #'var #'default)]
      [(var) (identifier? #'var) (list #'var #'#f)]
      [var (identifier? #'var) (list #'var #'#f)]
      [var (serror #'var "not a valid ~a spec" #:optional)]))
  ;; process a key argument spec, returns (<id> <key-stx> <default-expr>)
  (define (process-key k)
    (define (key var)
      (datum->syntax-object
       k (string->keyword (symbol->string (syntax-e var))) k k))
    (syntax-case k ()
      [(var key default)
       (and (identifier? #'var) (keyword? (syntax-e #'key)))
       (list #'var #'key #'default)]
      [(var default) (identifier? #'var) (list #'var (key #'var) #'default)]
      [(var) (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var (serror #'var "not a valid ~a spec" #:key)]))
  ;; helper for parse-formals
  (define (get-mode mode k k-stx formals keys)
    (cond [(null? keys)
           (serror k-stx "cannot use without #:key arguments")]
          [(pair? (cdar formals))
           (serror (cadar formals) "identifier following mode keyword ~a" k)]
          [(and mode (not (eq? k mode)))
           (serror k-stx "contradicting mode keywords")]
          [else k]))
  ;; helper for parse-formals
  (define (process-mode mode rests enablers)
    (if mode
      (eq? mode #:allow-other-keys)
      (ormap (lambda (k) (and (assq k rests) #t)) enablers)))
  ;; --------------------------------------------------------------------------
  ;; parses formals, returns list of normal vars, optional var specs, key var
  ;; specs, an alist of rest-like kw+vars, and a mode for allowing other keys
  ;; or not; no duplicate names
  (define (parse-formals formals)
    (let* ([formals (split-by-keywords formals)]
           [vars (car formals)]
           [formals (cdr formals)]
           [pop-formals
            (lambda (key)
              (if (and (pair? formals) (eq? key (syntax-e* (caar formals))))
                (begin0 (cdar formals) (set! formals (cdr formals)))
                '()))]
           [opts (pop-formals #:optional)]
           [keys (pop-formals #:key)])
      ;; now get all rest-like vars
      (let loop ([formals formals]
                 [rests '()]
                 [other-keys-mode #f]
                 [body-mode #f])
        (if (null? formals)
          (let ([opts (map process-opt opts)]
                [keys (map process-key keys)]
                [other-keys-mode
                 (process-mode other-keys-mode rests other-keys-accessing)]
                [body-mode (process-mode body-mode rests body-accessing)]
                [getr (lambda (k) (cond [(assq k rests) => cdr] [else #f]))])
            (values vars opts keys
                    (getr #:rest) (getr #:body) (getr #:rest-keys)
                    (getr #:all-keys) (getr #:other-keys)
                    other-keys-mode body-mode))
          (let* ([k-stx (caar formals)]
                 [k (syntax-e* k-stx)])
            (cond [(memq k '(#:optional #:key))
                   (serror k-stx "misplaced ~a" k)]
                  [(memq k allow-other-keys-kwds)
                   (loop (cdr formals) rests
                         (get-mode other-keys-mode k k-stx formals keys)
                         body-mode)]
                  [(memq k allow-body-kwds)
                   (loop (cdr formals) rests other-keys-mode
                         (get-mode body-mode k k-stx formals keys))]
                  [(not (memq k rest-like-kwds))
                   (serror k-stx "unknown meta keyword")]
                  [(assq k rests)
                   (serror k-stx "duplicate ~a" k)]
                  [(null? (cdar formals))
                   (serror k-stx "missing variable name")]
                  [(not (null? (cddar formals)))
                   (serror k-stx "too many variable names")]
                  [(and (null? keys) (not (eq? #:rest k)))
                   (serror k-stx "cannot use without #:key arguments")]
                  [else (loop (cdr formals)
                              (cons (cons k (cadar formals)) rests)
                              other-keys-mode body-mode)]))))))
  ;; --------------------------------------------------------------------------
  ;; make case-lambda clauses for a procedure with optionals
  ;; vars is all identifiers, each opt is (<id> <default-expr>)
  (define (make-opt-clauses name vars opts rest exprs)
    (let loop ([vars (reverse vars)]
               [opts opts]
               [clauses '()])
      (if (null? opts)
        ;; fast order: first the all-variable section, then from vars up
        (cons (with-syntax ([vars (append! (reverse vars) (or rest '()))]
                            [(expr ...) exprs])
                #'[vars expr ...])
              (reverse clauses))
        (loop (cons (caar opts) vars) (cdr opts)
              (cons (with-syntax ([(var ...) (reverse vars)]
                                  [((opt default) ...) opts]
                                  [name name])
                      #'[(var ...)
                         (let* ([opt default] ...) (name var ... opt ...))])
                    clauses)))))
  ;; --------------------------------------------------------------------------
  ;; generates the actual body
  (define (generate-body formals exprs)
    ;; relations:
    ;;   rest = (append all-keys body)
    ;;   rest-keys = (append other-keys body)
    (define-values (vars        ; plain variables
                    opts        ; optionals, each is (id default)
                    keys        ; keywords, each is (id key default)
                    rest        ; rest variable (no optionals)
                    body        ; rest after all keyword-vals
                    rest-keys   ; rest without specified keys
                    all-keys    ; keyword-vals without body
                    other-keys  ; unprocessed keyword-vals
                    other-keys-mode ; allowing other keys?
                    body-mode)      ; allowing body after keys?
      (parse-formals formals))
    (let (; use identifiers from here if none given, so the tests work
          [ids `(,@vars ,@(map car opts) ,@(map car keys) ,(or rest #'rest)
                 ,(or body #'body) ,(or rest-keys #'rest-keys)
                 ,(or all-keys #'all-keys) ,(or other-keys #'other-keys))])
      (cond [(ormap (lambda (x) (and (not (identifier? x)) x)) ids)
             => (lambda (d) (serror d "not an identifier"))]
            [(check-duplicate-identifier ids)
             => (lambda (d) (serror d "duplicate argument name"))]))
    (cond
     ;; no optionals or keys => plain lambda
     [(and (null? opts) (null? keys))
      (with-syntax ([vars (append! vars (or rest '()))]
                    [(expr ...) exprs])
        (syntax/loc stx (lambda vars expr ...)))]
     ;; no keys => just a lambda with optionals
     [(null? keys)
      (let* ([name (or (syntax-local-infer-name stx)
                       (quote-syntax lambda/kw-proc))]
             [clauses (make-opt-clauses name vars opts rest exprs)])
        (with-syntax ([name name] [clauses clauses])
          (syntax/loc stx (letrec ([name (case-lambda . clauses)]) name))))]
     [else (error "BOOM")]))
  (syntax-case stx ()
    [(_ (formal ... . rest) expr0 expr ...) ; dot is exactly like #:rest
     #'(_ (formal ... #:rest rest) expr0 expr ...)]
    [(_ (formal ...) expr0 expr ...)
     (generate-body (syntax->list #'(formal ...)) #'(expr0 expr ...))
     #;
     (let ()
       (when (and (or rest-keys body all-keys other-keys) (not rest))
         (set! rest #'rest))
       (cond
        ;; non-trivial case -- full processing
        [(or (pair? opts) (pair? keys) rest-keys body all-keys other-keys)
         (unless rest (set! rest #'rest))
         ;; other-keys is computed from all-keys
         (when (and other-keys (not all-keys)) (set! all-keys #'all-keys))
         (quasisyntax/loc stx
           (lambda (#,@vars . #,rest)
             (let*-values
                 (#,@(map (lambda (o)
                            #`[(#,(car o))
                               (if (pair? #,rest)
                                 (begin0 (car #,rest)
                                   (set! #,rest (cdr #,rest)))
                                 #,(cadr o))])
                          opts)
                  #,@(map (lambda (k)
                            #`[(#,(car k))
                               (getarg #,rest #,(cadr k)
                                       (lambda () #,(caddr k)))])
                          keys)
                  #,@(if rest-keys
                       #`([(#,rest-keys)
                           (filter-out-keys '#,(map cadr keys) #,rest)])
                       #'())
                  #,@(cond
                      ;; At most one scan for body, all-keys, other-keys.  This
                      ;; could be much shorter by always using keys/args, but a
                      ;; function call is not a place to spend time on.
                      [(and body all-keys)
                       #`([(#,all-keys #,body)
                           ;; inlined keys/args
                           (let loop ([args #,rest] [keys '()])
                             (cond [(or (null? args)
                                        (null? (cdr args))
                                        (not (keyword? (car args))))
                                    (values (reverse! keys) args)]
                                   [else (loop (cddr args)
                                               (list* (cadr args) (car args)
                                                      keys))]))])]
                      [body
                       #`([(#,body)
                           (let loop ([args #,rest])
                             (if (or (null? args)
                                     (null? (cdr args))
                                     (not (keyword? (car args))))
                               args
                               (loop (cddr args))))])]
                      [all-keys
                       #`([(#,all-keys)
                           ;; inlined keys/args, not returning args
                           (let loop ([args #,rest] [keys '()])
                             (cond [(or (null? args)
                                        (null? (cdr args))
                                        (not (keyword? (car args))))
                                    (reverse! keys)]
                                   [else (loop (cddr args)
                                               (list* (cadr args) (car args)
                                                      keys))]))])]
                      [else #'()])
                  #,@(if other-keys
                       #`([(#,other-keys) ; use all-keys (see above)
                           (filter-out-keys '#,(map cadr keys) #,all-keys)])
                       #'()))
               expr0 expr ...)))]
        ;; common cases: no optional, keyword, or other fancy stuff
        [(null? vars)
         (quasisyntax/loc stx
           (lambda #,(or rest #'()) expr0 expr ...))]
        [else
         (quasisyntax/loc stx
           (lambda (#,@vars . #,(or rest #'())) expr0 expr ...))]))]))

(provide define/kw)
(define-syntax (define/kw stx)
  (syntax-case stx ()
    [(_ name val) (identifier? #'name) #'(define name val)]
    [(_ (name . args) body0 body ...)
     (syntax/loc stx (_ name (lambda/kw args body0 body ...)))]))

;; Keyword searching utilities (note: no errors for odd length)
(provide getarg getargs keys/args filter-out-keys)

(define (getarg args keyword . not-found)
  (let loop ([args args])
    (cond [(or (null? args) (null? (cdr args)))
           (and (pair? not-found)
                (let ([x (car not-found)])
                  (cond [(procedure? x) (x)]
                        [(promise? x) (force x)]
                        [else x])))]
          [(eq? (car args) keyword) (cadr args)]
          [else (loop (cddr args))])))

(define (getargs initargs keyword)
  (define (scan tail)
    (cond [(null? tail) '()]
          [(null? (cdr tail)) (error 'getargs "keyword list not balanced.")]
          [(eq? (car tail) keyword) (cons (cadr tail) (scan (cddr tail)))]
          [else (scan (cddr tail))]))
  (scan initargs))

(define (keys/args args)
  (let loop ([args args] [keys '()])
    (cond [(or (null? args) (null? (cdr args)) (not (keyword? (car args))))
           (values (reverse! keys) args)]
          [else (loop (cddr args) (list* (cadr args) (car args) keys))])))

(define (filter-out-keys outs args)
  (let loop ([as args] [r '()])
    (cond [(null? as) (reverse! r)]
          [(null? (cdr as)) (reverse! (cons (car as) r))]
          [else
           (loop (cddr as)
                 (if (memq (car as) outs) r (list* (cadr as) (car as) r)))])))

)
