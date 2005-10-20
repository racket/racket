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
  ;; turns formals into a syntax list
  (define (formals->list formals)
    (syntax-case formals ()
      [(formal ... . rest) ; dot is exactly like #:rest
       (formals->list #'(formal ... #:rest rest))]
      [(formal ...) (syntax->list formals)]))
  ;; is an expression simple? (=> evaluating cannot have side effects)
  (define (simple-expr? expr)
    (let ([expr (local-expand expr 'expression null)]) ; expand id macros
      (syntax-case expr (#%datum #%top quote)
        [(#%datum . _) #t]
        [(#%top . _) #t]
        [(quote . _) #t]
        [_ (identifier? expr)])))
  ;; split a list of syntax objects based on syntax keywords:
  ;;   (x ... #:k1 ... #:k2 ... ...) --> ((x ...) (#:k1 ...) (#:k2 ...) ...)
  (define (split-by-keywords xs)
    (let loop ([xs (if (syntax? xs) (formals->list xs) xs)] [cur '()] [r '()])
      (if (null? xs)
        (reverse! (cons (reverse! cur) r))
        (let ([x (car xs)])
          (if (keyword? (syntax-e* x))
            (loop (cdr xs) (list x) (cons (reverse! cur) r))
            (loop (cdr xs) (cons x cur) r))))))
  ;; --------------------------------------------------------------------------
  ;; process an optional argument spec, returns (<id> <default-expr>)
  (define (process-opt o)
    (syntax-case o ()
      [(var default) (identifier? #'var) (list #'var #'default)]
      [(var) (identifier? #'var) (list #'var #'#f)]
      [var (identifier? #'var) (list #'var #'#f)]
      [var (serror #'var "not a valid ~a spec" #:optional)]))
  ;; --------------------------------------------------------------------------
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
  ;; --------------------------------------------------------------------------
  ;; helpers for process-vars
  (define (get-mode mode k k-stx formals keys)
    (cond [(null? keys)
           (serror k-stx "cannot use without #:key arguments")]
          [(pair? (cdar formals))
           (serror (cadar formals) "identifier following mode keyword ~a" k)]
          [(and mode (not (eq? k mode)))
           (serror k-stx "contradicting mode keywords")]
          [else k]))
  (define (process-mode mode rests allow enablers)
    (if mode
      (eq? mode allow)
      (ormap (lambda (k) (and (assq k rests) #t)) enablers)))
  ;; --------------------------------------------------------------------------
  ;; test variables
  (define (process-vars vars opts keys0 rests other-keys-mode body-mode
                        . only-vars?)
    (define (gensym x)
      (car (generate-temporaries (list x))))
    (let*-values
        ([(only-vars?) (and (pair? only-vars?) (car only-vars?))]
         [(opts keys0) (values (map process-opt opts) (map process-key keys0))]
         [(other-keys-mode body-mode)
          (values (process-mode other-keys-mode
                                rests #:allow-other-keys other-keys-accessing)
                  (process-mode body-mode
                                rests #:allow-body body-accessing))]
         [(rest body rest-keys all-keys other-keys)
          (apply values
                 (map (lambda (k) (cond [(assq k rests) => cdr] [else #f]))
                      '(#:rest #:body #:rest-keys #:all-keys #:other-keys)))]
         [(body-spec body)
          (if (identifier? body)
            (values #f body)
            (values body (gensym #'body)))]
         [(rest* body* other-keys*)
          (values (or rest (gensym #'rest))
                  (or body (gensym #'body))
                  (or other-keys (gensym #'other-keys)))]
         ;; turn (<id> <key> <default>) keys to (<id> <default>)
         [(keys)
          (with-syntax ([r rest*])
            (map (lambda (k)
                   (list (car k)
                         (if (simple-expr? (caddr k))
                           ;; simple case => no closure
                           #`(getarg* r #,(cadr k) #,(caddr k))
                           #`(getarg r #,(cadr k) (lambda () #,(caddr k))))))
                 keys0))]
         [(all-ids)
          `(,@vars ,@(map car opts) ,@(map car keys) ,rest* ,body*
            ;; make up names if not specified, to make checking easy
            ,(or rest-keys (gensym #'rest-keys))
            ,(or all-keys (gensym #'all-keys))
            ,(or other-keys (gensym #'other-keys))
            ,@(if body-spec (parse-formals body-spec #t) '()))])
      (cond [only-vars? all-ids]
            [(ormap (lambda (x) (and (not (identifier? x)) x)) all-ids)
             => (lambda (d) (serror d "not an identifier"))]
            [(check-duplicate-identifier all-ids)
             => (lambda (d) (serror d "duplicate argument name"))]
            [else (values vars opts keys rest rest* body body* body-spec
                          rest-keys all-keys other-keys other-keys*
                          other-keys-mode body-mode (map cadr keys0))])))
  ;; --------------------------------------------------------------------------
  ;; parses formals, returns list of normal vars, optional var specs, key var
  ;; specs, an alist of rest-like kw+vars, and a mode for allowing other keys
  ;; or not; no duplicate names
  (define (parse-formals formals . only-vars?)
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
          (apply process-vars vars opts keys rests other-keys-mode body-mode
                 only-vars?)
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
  ;; generates the actual body
  (define (generate-body formals expr)
    ;; relations:
    ;;   rest = (append all-keys body)
    ;;   rest-keys = (append other-keys body)
    (define-values (vars        ; plain variables
                    opts        ; optionals, each is (id default)
                    keys        ; keywords, each is (id key default)
                    rest        ; rest variable (no optionals)
                    rest*       ;   always an id
                    body        ; rest after all keyword-vals
                    body*       ;   always an id
                    body-spec   ; syntax of body with sub-formals
                    rest-keys   ; rest without specified keys
                    all-keys    ; keyword-vals without body
                    other-keys  ; unprocessed keyword-vals
                    other-keys* ;   always an id
                    allow-other-keys? ; allowing other keys?
                    allow-body?       ; allowing body after keys?
                    keywords)   ; list of mentioned keywords
      (parse-formals formals))
    (define name
      (or (syntax-local-infer-name stx) (quote-syntax lambda/kw-proc)))
    ;; ------------------------------------------------------------------------
    ;; make case-lambda clauses for a procedure with optionals
    (define (make-opt-clauses expr rest)
      (let loop ([vars (reverse vars)]
                 [opts opts]
                 [clauses '()])
        (if (null? opts)
          ;; fast order: first the all-variable section, then from vars up
          (cons (with-syntax ([vars (append! (reverse vars) rest)]
                              [expr expr])
                  #'[vars expr])
                (reverse clauses))
          (loop (cons (caar opts) vars) (cdr opts)
                (cons (with-syntax ([(var ...) (reverse vars)]
                                    [((ovar default) ...) opts]
                                    [name name])
                        #'[(var ...)
                           (let* ([ovar default] ...)
                             (name var ... ovar ...))])
                      clauses)))))
    ;; ------------------------------------------------------------------------
    ;; generates the part of the body that deals with rest-related stuff
    (define (make-rest-body expr)
      (define others? (or other-keys rest-keys))
      (with-syntax ([name        name]
                    [rest*       rest*]
                    [body*       body*]
                    [keywords    keywords]
                    [expr        expr]
                    [all-keys*   all-keys]
                    [other-keys* other-keys*]
                    [rest-keys*  rest-keys])
        (with-syntax ([loop-vars
                       #`([body* rest*]
                          #,@(if all-keys #`([all-keys* '()]) '())
                          #,@(if others? #`([other-keys* '()]) '()))]
                      [next-loop
                       #`(loop (cddr body*)
                               #,@(if all-keys
                                    #`((list* (cadr body*) (car body*)
                                              all-keys*))
                                    '())
                               #,@(if others?
                                    #`((if (memq (car body*) 'keywords)
                                         other-keys*
                                         (list* (cadr body*) (car body*)
                                                other-keys*)))
                                    '()))]
                      [expr
                       (if (or all-keys others?)
                         #`(let* (#,@(if all-keys
                                       #'([all-keys* (reverse! all-keys*)])
                                       '())
                                  #,@(if others?
                                       #'([other-keys* (reverse! other-keys*)])
                                       '())
                                  #,@(cond [(and other-keys rest-keys)
                                            #'([rest-keys*
                                                (append other-keys* body*)])]
                                           [rest-keys ; can destroy other-keys
                                            #'([rest-keys*
                                                (append! other-keys* body*)])]
                                           [else '()]))
                             expr)
                         #'expr)])
          (with-syntax ([next-loop
                         (if allow-other-keys?
                           #'next-loop
                           #'(if (memq (car body*) 'keywords)
                               next-loop
                               (error* 'name "unknown keyword: ~e"
                                       (car body*))))])
            #`(let loop loop-vars
                (if (and (pair? body*) (keyword? (car body*)))
                  (if (pair? (cdr body*))
                    next-loop
                    (error* 'name "keyword list not balanced: ~e" rest*))
                  #,(if allow-body?
                      (if body-spec
                        (with-syntax ([name (string->symbol
                                             (format "~a~~body"
                                                     (syntax-e* #'name)))])
                          (with-syntax ([subcall
                                         (quasisyntax/loc stx
                                           (let ([name (lambda/kw #,body-spec
                                                         expr)])
                                             name))])
                            #'(apply subcall body*)))
                        #'expr)
                      #'(if (null? body*)
                          expr
                          (error* 'name "expecting a ~s keyword got: ~e"
                                  'keywords (car body*))))))))))
    ;; ------------------------------------------------------------------------
    ;; generates the part of the body that deals with rest-related stuff
    (define (make-keys-body expr)
      (with-syntax ([body (make-rest-body expr)] [keys keys])
        #'(let* keys body)))
    ;; ------------------------------------------------------------------------
    ;; body generation starts here
    (cond
     ;; no optionals or keys => plain lambda
     [(and (null? opts) (null? keys))
      (with-syntax ([vars (append! vars (or rest '()))] [expr expr])
        (syntax/loc stx (lambda vars expr)))]
     ;; no keys => make a case-lambda for optionals
     [(null? keys)
      (let ([clauses (make-opt-clauses expr (or rest '()))])
        (with-syntax ([name name] [clauses clauses])
          (syntax/loc stx (letrec ([name (case-lambda . clauses)]) name))))]
     ;; no opts => normal processing of keywords etc
     [(null? opts)
      (with-syntax ([vars (append! vars rest*)]
                    [body (make-keys-body expr)])
        (syntax/loc stx (lambda vars body)))]
     ;; both opts and keys => combine the above two
     [else
      (let ([clauses (make-opt-clauses (make-keys-body expr) rest*)])
        (with-syntax ([name name] [clauses clauses])
          (syntax/loc stx (letrec ([name (case-lambda . clauses)]) name))))]))
  (syntax-case stx ()
    [(_ formals expr0 expr ...)
     (generate-body #'formals #'(begin expr0 expr ...))]))

(provide define/kw)
(define-syntax (define/kw stx)
  (syntax-case stx ()
    [(_ name val) (identifier? #'name) #'(define name val)]
    [(_ (name . args) body0 body ...)
     (syntax/loc stx (_ name (lambda/kw args body0 body ...)))]))

;; raise an proper exception
(define (error* who fmt . args)
  (raise (make-exn:fail:contract
          (string->immutable-string
           (apply format (string-append "~a: " fmt) who args))
          (current-continuation-marks))))

;; Keyword searching utility (note: no errors for odd length)
(provide getarg)
(define (getarg args keyword . not-found)
  (let loop ([args args])
    (cond [(or (null? args) (null? (cdr args)) (not (keyword? (car args))))
           (and (pair? not-found)
                (let ([x (car not-found)])
                  (if (procedure? x) (x) x)))]
          [(eq? (car args) keyword) (cadr args)]
          [else (loop (cddr args))])))

;; a private version of getarg that is always used with simple values
(define (getarg* args keyword . not-found)
  (let loop ([args args])
    (cond [(or (null? args) (null? (cdr args)) (not (keyword? (car args))))
           (and (pair? not-found) (car not-found))]
          [(eq? (car args) keyword) (cadr args)]
          [else (loop (cddr args))])))

)
