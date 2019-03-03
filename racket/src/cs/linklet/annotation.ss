(define (correlated->annotation v)
  (let-values ([(e stripped-e) (correlated->annotation* v)])
    e))

(define (correlated->annotation* v)
  (cond
   [(pair? v) (let-values ([(a stripped-a) (correlated->annotation* (car v))]
                           [(d stripped-d) (correlated->annotation* (cdr v))])
                (cond
                 [(and (eq? a (car v))
                       (eq? d (cdr v)))
                  (values v v)]
                 [(and (eq? stripped-a 'letrec*)
                       (pair? (cdr v)))
                  (let ([names (let loop ([clauses (cadr v)])
                                 (cond
                                  [(null? clauses) '()]
                                  [else
                                   (let ([id (caar clauses)])
                                     (cons (or (and (correlated? id)
                                                    (correlated-property id 'undefined-error-name))
                                               (if (correlated? id)
                                                   (correlated-e id)
                                                   id))
                                           (loop (cdr clauses))))]))])
                    (values (list* 'letrec*/names names d)
                            (list* 'letrec*/names names stripped-d)))]
                 [else (values (cons a d)
                               (cons stripped-a stripped-d))]))]
   [(correlated? v) (let-values ([(e stripped-e) (correlated->annotation* (correlated-e v))])
                      (let ([name (correlated-property v 'inferred-name)]
                            [method-arity-error (correlated-property v 'method-arity-error)])
                        (define (add-name e)
                          (if (and name (not (void? name)))
                              `(|#%name| ,name ,e)
                              e))
                        (define (add-method-arity-error e)
                          (if method-arity-error
                              `(|#%method-arity| ,e)
                              e))
                        (values (add-method-arity-error
                                 (add-name (transfer-srcloc v e stripped-e)))
                                (add-method-arity-error
                                 (add-name stripped-e)))))]
   ;; correlated will be nested only in pairs with current expander
   [else (values v v)]))

(define (transfer-srcloc v e stripped-e)
  (let ([src (correlated-source v)]
        [pos (correlated-position v)]
        [line (correlated-line v)]
        [column (correlated-column v)]
        [span (correlated-span v)])
    (if (and pos span (or (path? src) (string? src)))
        (let ([pos (sub1 pos)]) ; Racket positions are 1-based; host Scheme positions are 0-based
          (make-annotation e
                           (if (and line column)
                               ;; Racket columns are 0-based; host-Scheme columns are 1-based
                               (make-source-object (source->sfd src) pos (+ pos span) line (add1 column))
                               (make-source-object (source->sfd src) pos (+ pos span)))
                           stripped-e))
        e)))

(define sfd-cache-box (unsafe-make-place-local #f))

(define (source->sfd src)
  (let ([sfd-cache (unsafe-place-local-ref sfd-cache-box)])
    (cond
     [sfd-cache
      (or (hash-ref sfd-cache src #f)
          (let ([str (if (path? src)
                         (path->string src)
                         src)])
            ;; We'll use a file-position object in source objects, so
            ;; the sfd checksum doesn't matter
            (let ([sfd (source-file-descriptor str 0)])
              (hash-set! sfd-cache src sfd)
              sfd)))]
     [else
      ;; There's a race here at the level of Racket threads,
      ;; but that seems ok for setting up a cache
      (unsafe-place-local-set! sfd-cache-box (make-weak-hash))
      (source->sfd src)])))

;; --------------------------------------------------

(define (strip-nested-annotations s)
  (cond
   [(annotation? s) (annotation-stripped s)]
   [(pair? s)
    (let ([a (strip-nested-annotations (car s))]
          [d (strip-nested-annotations (cdr s))])
      (if (and (eq? a (car s)) (eq? d (cdr s)))
          s
          (cons a d)))]
   [else s]))
