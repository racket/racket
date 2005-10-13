(module kw mzscheme

(require-for-syntax (lib "define.ss" "syntax"))

(provide define/kw)
(define-syntax (define/kw stx)
  (let-values ([(id val) (normalize-definition stx #'lambda/kw)])
    (with-syntax ([id id] [val val]) #'(define id val))))

(provide lambda/kw)
(define-syntax (lambda/kw stx)
  (define (process-optional-arg o)
    (syntax-case o ()
      [(var default) (identifier? #'var) (list #'var #'default)]
      [(var) (identifier? #'var) (list #'var #'#f)]
      [var (identifier? #'var) (list #'var #'#f)]
      [var (raise-syntax-error #f "not a valid #:optional spec" stx #'var)]))
  (define (process-keyword-arg k)
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
      [var (raise-syntax-error #f "not a valid #:key spec" stx #'var)]))
  (syntax-case stx ()
    [(_ formals expr0 expr ...)
     (let ([vars '()]
           [opts '()]
           [keys '()]
           [rest       #f]  ; keys and all (no optionals)
           [rest-keys  #f]  ; like the above, minus specified keys
           [body       #f]  ; stuff that follows all keywords
           [all-keys   #f]  ; all keys, excluding body
           [other-keys #f]) ; unprocessed keys, excluding body
       ;; relations:
       ;;   rest = (append all-keys body)
       ;;   rest-keys = (append other-keys body)
       (let loop ([state #f] [args #'formals])
         (syntax-case args ()
           [() #f]
           [(v . xs)
            (let* ([v #'v]
                   [k (if (syntax? v) (syntax-e v) v)]
                   [k (and (keyword? k) k)])
              (define (serror sub fmt . args)
                (raise-syntax-error #f (apply format fmt args) stx sub))
              (cond
               [k (case k
                    [(#:optional)
                     (if state
                       (serror #'formals "misplaced ~a argument" k)
                       (loop 'o #'xs))]
                    [(#:key)
                     (if (memq state '(#f o r!))
                       (loop 'k #'xs)
                       (serror #'formals "misplaced ~a argument" k))]
                    [(#:rest)
                     (if (pair? (syntax-e #'xs))
                       (loop 'r #'xs)
                       (serror #'formals "no name for ~a argument" k))]
                    [(#:rest-keys)
                     (if (pair? (syntax-e #'xs))
                       (loop 'rk #'xs)
                       (serror #'formals "no name for ~a argument" k))]
                    [(#:body)
                     (if (pair? (syntax-e #'xs))
                       (loop 'b #'xs)
                       (serror #'formals "no name for ~a argument" k))]
                    [(#:all-keys)
                     (if (pair? (syntax-e #'xs))
                       (loop 'ak #'xs)
                       (serror #'formals "no name for ~a argument" k))]
                    [(#:other-keys)
                     (if (pair? (syntax-e #'xs))
                       (loop 'ok #'xs)
                       (serror #'formals "no name for ~a argument" k))]
                    [else (serror v "unknown lambda meta-keyword")])]
               [(not (or (identifier? #'v) (memq state '(o k))))
                (serror v "not an identifier")]
               [else
                (let ([test (lambda (var name)
                              (if var
                                (serror #'formals "too many ~a arguments" name)
                                (set! state 'r!)))])
                  (case state
                    [(#f) (set! vars (cons v vars))]
                    [(o)  (set! opts (cons v opts))]
                    [(k)  (set! keys (cons v keys))]
                    [(r!) (serror v "second identifier after a ~a or similar"
                                  #:rest)]
                    [(r)  (test rest       #:rest      ) (set! rest v)]
                    [(rk) (test rest-keys  #:rest-keys ) (set! rest-keys v)]
                    [(b)  (test body       #:body      ) (set! body v)]
                    [(ak) (test all-keys   #:all-keys  ) (set! all-keys v)]
                    [(ok) (test other-keys #:other-keys) (set! other-keys v)]
                    [else (serror v "bad lambda formals")])
                  (loop state #'xs))]))]
           [v (loop state #'(#:rest v))]))
       (set! vars (reverse! vars))
       (set! opts (map process-optional-arg (reverse! opts)))
       (set! keys (map process-keyword-arg  (reverse! keys)))
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

;; Keyword searching utilities (note: no errors for odd length)
(provide getarg syntax-getarg getargs keys/args filter-out-keys)

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

(define (syntax-getarg syntax-args keyword . not-found)
  (when (syntax? keyword) (set! keyword (syntax-e keyword)))
  (let loop ([args syntax-args])
    (syntax-case args ()
      [(key arg . more)
       (if (eq? (syntax-e #'key) keyword) #'arg (loop #'more))]
      [_ (and (pair? not-found)
              (let ([x (car not-found)])
                (cond [(procedure? x) (x)]
                      [(promise? x) (force x)]
                      [else x])))])))

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
