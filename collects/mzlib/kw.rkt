(module kw mzscheme

(require-for-syntax syntax/name)

(begin-for-syntax ; -> configuration for lambda/kw
  ;; must appear at the end, each with exactly one variable
  (define rest-like-kwds
    '(#:rest #:body #:all-keys #:other-keys #:other-keys+body))
  ;; mode keys are in the end, without variable, cannot have contradictions
  ;; each descriptor for #:allow-kwd and #:forbid-kwd is
  ;;   (kwd-sym (forcer ...) (enabler ...))
  ;; `forcer' is a rest-like keyword that forces the mode, `enabler' is a
  ;; rest-like keyword that makes it on by default
  (define mode-keyword-specs
    '((other-keys     (#:other-keys) (#:rest #:all-keys #:other-keys+body))
      (duplicate-keys ()             (#:rest #:all-keys))
      (body           (#:body)       (#:rest #:other-keys+body))
      (anything       ()             ())))
  ;; precomputed mode keyword stuff
  (define processed-keyword-specs
    (map (lambda (ks)
           (let* ([k (car ks)]
                  [make (lambda (str)
                          (string->keyword
                           (string-append str (symbol->string k))))])
             (list* k (make "allow-") (make "forbid-") (cdr ks))))
         mode-keyword-specs))
  (define mode-keywords
    (apply append (map (lambda (ks) (list (cadr ks) (caddr ks)))
                       processed-keyword-specs))))

(define true (list 'true)) ; used for flag values

(provide lambda/kw)
(define-syntax (lambda/kw stx)
  ;; --------------------------------------------------------------------------
  ;; easy syntax errors
  (define (serror sub fmt . args)
    (apply raise-syntax-error
           #f (apply format fmt args) stx (if sub (list sub) '())))
  ;; contents of syntax
  (define (syntax-e* x) (if (syntax? x) (syntax-e x) x))
  ;; turns formals into a syntax list
  (define (formals->list formals)
    (syntax-case formals ()
      [(formal ... . rest)
       (not (null? (syntax-e #'rest)))
       ;; dot is exactly like #:rest, but don't allow it with other
       ;; meta-keywords since its meaning is confusing
       (let* ([formals (syntax->list #'(formal ...))]
              [kwd (ormap (lambda (s) (and (keyword? (syntax-e* s)) s))
                          formals)])
         (if kwd
           (serror #'rest "use #:rest or #:body instead of dot notation"
                   ;; (syntax-e* kwd) <- confusing to show this
                   )
           (append formals (list #'#:rest #'rest))))]
      [(formal ...) (syntax->list formals)]))
  ;; split a list of syntax objects based on syntax keywords:
  ;;   (x ... #:k1 ... #:k2 ... ...) --> ((x ...) (#:k1 ...) (#:k2 ...) ...)
  (define (split-by-keywords xs)
    (let loop ([xs (if (syntax? xs) (formals->list xs) xs)] [cur '()] [r '()])
      (if (null? xs)
        (reverse (cons (reverse cur) r))
        (let ([x (car xs)])
          (if (keyword? (syntax-e* x))
            (loop (cdr xs) (list x) (cons (reverse cur) r))
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
      [var   (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var (serror #'var "not a valid ~a spec" #:key)]))
  ;; --------------------------------------------------------------------------
  ;; process a flag argument spec, returns (<id> <key-stx> <default-expr>)
  ;; so it can be used like keys
  (define (process-flag k)
    (define (key var)
      (datum->syntax-object
       k (string->keyword (symbol->string (syntax-e var))) k k))
    (syntax-case k ()
      [(var key)
       (and (identifier? #'var) (keyword? (syntax-e #'key)))
       (list #'var #'key #'#f)]
      [(var) (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var   (identifier? #'var) (list #'var (key #'var) #'#f)]
      [var (serror #'var "not a valid ~a spec" #:flag)]))
  ;; --------------------------------------------------------------------------
  ;; helpers for process-vars
  (define ((process-mode modes rests) processed-spec)
    (let ([allow      (memq (cadr processed-spec) modes)]
          [forbid     (memq (caddr processed-spec) modes)]
          [allow-any  (memq #:allow-anything modes)]
          [forbid-any (memq #:forbid-anything modes)])
      (cond
       [(and allow forbid)
        (serror #f "contradicting #:...-~a keywords" (car processed-spec))]
       [(and forbid allow-any)
        (serror #f "~a contradicts #:allow-anything" (caddr processed-spec))]
       [(and allow forbid-any)
        (serror #f "~a contradicts #:forbid-anything" (cadr processed-spec))]
       [(ormap (lambda (k) (assq k rests)) (cadddr processed-spec))
        => ; forced?
        (lambda (r)
          (when (or forbid forbid-any)
            (serror #f "cannot ~s with ~s"
                    (car (or forbid forbid-any)) (car r)))
          #t)]
       [(or allow allow-any) #t]
       [(or forbid forbid-any) #f]
       [else (ormap (lambda (k) (and (assq k rests) #t)) ; suggested?
                    (car (cddddr processed-spec)))])))
  (define (make-keyword-get-expr key rest default known-vars)
    ;; expand (for id macros) and check if it's a simple expression, because if
    ;; it is, evaluation cannot have side-effects and we can use keyword-get*
    (define default*
      (local-expand default 'expression (cons #'#%app known-vars)))
    (define simple?
      (syntax-case default* (#%datum #%top quote)
        [(#%datum . _) #t] [(#%top . _) #t] [(quote . _) #t]
        [_ (identifier? default*)]))
    (with-syntax ([getter  (if simple? #'keyword-get* #'keyword-get)]
                  [default (if simple? default* #`(lambda () #,default*))]
                  [rest rest] [key key])
      #'(getter rest key default)))
  ;; --------------------------------------------------------------------------
  ;; test variables
  (define (process-vars vars opts keys0 flags rests modes . only-vars?)
    (define (gensym x)
      (car (generate-temporaries (list x))))
    (let*-values
        ([(only-vars?) (and (pair? only-vars?) (car only-vars?))]
         [(opts)  (map process-opt opts)]
         [(keys0) (map process-key keys0)]
         [(flags) (map process-flag flags)]
         [(rest body all-keys other-keys other-keys+body)
          (apply values (map (lambda (k)
                               (cond [(assq k rests) => cdr] [else #f]))
                             rest-like-kwds))]
         [(rest*)       (or rest (gensym #'rest))]
         [(body*)       (if (and body (identifier? body)) body (gensym #'body))]
         [(other-keys*) (or other-keys (gensym #'other-keys))]
         [(other-keys-mode duplicate-keys-mode body-mode anything-mode)
          (apply values (map (process-mode modes rests)
                             processed-keyword-specs))]
         ;; turn (<id> <key> <default>) keys to (<id> <getter>)
         [(keys)
          (with-syntax ([rst rest*])
            (let loop ([ks (append keys0 flags)]  [r '()]
                       [known-vars (append vars (map car opts))])
              (if (null? ks)
                (reverse r)
                (let ([k (car ks)])
                  (loop (cdr ks)
                        (cons (list (car k)
                                    (make-keyword-get-expr
                                     (cadr k) rest* (caddr k) known-vars))
                              r)
                        (cons (car k) known-vars))))))]
         [(all-ids)
          `(,@vars ,@(map car opts) ,@(map car keys) ,rest* ,body*
            ;; make up names if not specified, to make checking easy
            ,(or all-keys        (gensym #'all-keys))
            ,(or other-keys      (gensym #'other-keys))
            ,(or other-keys+body (gensym #'other-keys+body))
            ,@(if (and body (not (identifier? body)))
                (parse-formals body #t) '()))])
      (cond [only-vars? all-ids]
            [(ormap (lambda (x) (and (not (identifier? x)) x)) all-ids)
             => (lambda (d) (serror d "not an identifier"))]
            [(check-duplicate-identifier all-ids)
             => (lambda (d) (serror d "duplicate argument name"))]
            [else (values
                   vars opts keys (map cadr flags) rest rest* body body*
                   all-keys other-keys other-keys* other-keys+body
                   other-keys-mode duplicate-keys-mode body-mode anything-mode
                   (append (map cadr keys0) (map cadr flags)))])))
  ;; --------------------------------------------------------------------------
  ;; parses formals, returns list of normal vars, optional var specs, key var
  ;; specs, an alist of rest-like kw+vars, and a mode for allowing other keys
  ;; or not; no duplicate names
  (define (parse-formals formals . only-vars?)
    (let* ([formals (split-by-keywords formals)]
           [vars (car formals)]
           [formals (cdr formals)]
           [opts  '()]
           [keys  '()]
           [flags '()])
      (when (and (pair? formals) (eq? #:optional (syntax-e* (caar formals))))
        (set! opts (cdar formals)) (set! formals (cdr formals)))
      (let loop ([last #f])
        (let* ([k-stx (and (pair? formals) (caar formals))]
               [k     (and k-stx (syntax-e* k-stx))])
          (when (and k (eq? k last)) (serror k-stx "two ~s sections" k))
          (case k
            [(#:key) (set! keys (append keys (cdar formals)))
             (set! formals (cdr formals)) (loop k)]
            [(#:flag) (set! flags (append flags (cdar formals)))
             (set! formals (cdr formals)) (loop k)]
            #| else continue below |#)))
      ;; now get all rest-like vars and modes
      (let loop ([formals formals] [rests '()] [modes '()])
        (if (null? formals)
          (apply process-vars vars opts keys flags rests modes only-vars?)
          (let* ([k-stx (caar formals)]
                 [k     (syntax-e* k-stx)])
            (cond [(memq k '(#:optional #:key #:flag))
                   (serror k-stx "misplaced ~a" k)]
                  [(memq k mode-keywords)
                   (cond
                    #; ;(*)
                    ;; don't throw an error here, it is still fine if used with
                    ;; #:allow-other-keys (explicit or implicit), also below
                    [(and (null? keys) (null? flags))
                     (serror k-stx "cannot use without #:key/#:flag arguments")]
                    [(pair? (cdar formals))
                     (serror (cadar formals)
                             "identifier following mode keyword ~a" k)]
                    [else (loop (cdr formals) rests (cons k modes))])]
                  [(not (memq k rest-like-kwds))
                   (serror k-stx "unknown meta keyword")]
                  [(assq k rests)
                   (serror k-stx "duplicate ~a" k)]
                  [(null? (cdar formals))
                   (serror k-stx "missing variable name")]
                  [(not (null? (cddar formals)))
                   (serror k-stx "too many variable names")]
                  #; ;(*)
                  ;; same as above: don't throw an error here, still fine if
                  ;; used with #:allow-other-keys (explicit or implicit)
                  [(and (null? keys) (not (eq? #:rest k)))
                   (serror k-stx "cannot use without #:key/#:flag arguments")]
                  [else (loop (cdr formals)
                              (cons (cons k (cadar formals)) rests)
                              modes)]))))))
  ;; --------------------------------------------------------------------------
  ;; generates the actual body
  (define (generate-body formals expr)
    ;; relations:
    ;;   rest = (append all-keys body)
    ;;   other-keys+body = (append other-keys body)
    (define-values (vars        ; plain variables
                    opts        ; optionals, each is (id default)
                    keys        ; keywords, each is (id key default)
                    flags       ; flag keyword syntaxes (args are part of keys)
                    rest        ; rest variable (no optionals)
                    rest*       ;   always an id
                    body        ; rest after all keyword-vals (id or formals)
                    body*       ;   always an id
                    all-keys    ; keyword-vals without body
                    other-keys  ; unprocessed keyword-vals
                    other-keys* ;   always an id
                    other-keys+body ; rest without specified keys
                    allow-other-keys?     ; allowing other keys?
                    allow-duplicate-keys? ; allowing duplicate keys?
                    allow-body?           ; allowing body after keys?
                    allow-anything?       ; allowing anything?
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
          (cons (with-syntax ([vars (append (reverse vars) rest)]
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
      (define others?     (or other-keys other-keys+body))
      (define track-seen? (or others? (not allow-duplicate-keys?)))
      (with-syntax ([name        name]
                    [rest*       rest*]
                    [body*       body*]
                    [keywords    keywords]
                    [expr        expr]
                    [all-keys*   all-keys]
                    [other-keys* other-keys*]
                    [other-keys+body* other-keys+body]
                    [seen-keys   #'seen-keys])
        (with-syntax
            ([loop-vars #`([body* rest*]
                           #,@(if all-keys    #`([all-keys* '()]) '())
                           #,@(if others?     #`([other-keys* '()]) '())
                           #,@(if track-seen? #`([seen-keys '()]) '()))]
             [next-loop
              (let ([nl #`(loop
                           (cddr body*)
                           #,@(if all-keys
                                #`((list* (cadr body*) (car body*) all-keys*))
                                '())
                           #,@(if others?
                                #`((if (and in-keys? (not in-seen?))
                                     other-keys*
                                     (list* (cadr body*) (car body*)
                                            other-keys*)))
                                '())
                           #,@(if track-seen?
                                #`((if (and in-seen? in-keys?)
                                     #,(if allow-duplicate-keys?
                                         #`seen-keys
                                         #`(error* 'name "duplicate keyword: ~.s"
                                                   (car body*)))
                                     (cons (car body*) seen-keys)))
                                '()))])
                (cond
                 [(or track-seen? others?)
                  #`(let ([in-keys? (memq (car body*) 'keywords)]
                          [in-seen? (memq (car body*) seen-keys)])
                      #,(if allow-other-keys?
                          nl
                          #`(if in-keys?
                              #,nl
                              (error* 'name "unknown keyword: ~.s"
                                      (car body*)))))]
                 [(not allow-other-keys?)
                  #`(if (memq (car body*) 'keywords)
                      #,nl
                      (error* 'name "unknown keyword: ~.s"
                              (car body*)))]
                 [else nl]))]
             [expr
              (if (or all-keys others?)
                #`(let* (#,@(if all-keys
                              #'([all-keys* (reverse all-keys*)])
                              '())
                         #,@(if others?
                              #'([other-keys* (reverse other-keys*)])
                              '())
                         #,@(cond [(and other-keys other-keys+body)
                                   #'([other-keys+body*
                                       (append other-keys* body*)])]
                                  [other-keys+body ; can destroy other-keys
                                   #'([other-keys+body*
                                       (append other-keys* body*)])]
                                  [else '()]))
                    expr)
                #'expr)])
          (if (and allow-anything? (not body)
                   (not other-keys+body) (not all-keys) (not other-keys)
                   (null? flags))
            ;; allowing anything and don't need special rests, so no loop
            #'expr
            ;; normal code
            #`(let loop loop-vars
                (if (and (pair? body*) (keyword? (car body*))
                         #,@(if allow-anything? #'((pair? (cdr body*))) '()))
                  #,(if allow-anything? ; already checked pair? above
                      #'next-loop
                      #'(if (pair? (cdr body*))
                          next-loop
                          (error* 'name "keyword list not balanced: ~.s" rest*)))
                  #,(if allow-body?
                      (if (and body (not (identifier? body)))
                        (with-syntax ([name (string->symbol
                                             (format "~a~~body"
                                                     (syntax-e* #'name)))])
                          (with-syntax ([subcall
                                         (quasisyntax/loc stx
                                           (let ([name (lambda/kw #,body expr)])
                                             name))])
                            #'(apply subcall body*)))
                        #'expr)
                      #'(if (null? body*)
                          expr
                          (error* 'name "expecting a ~s keyword got: ~.s"
                                  'keywords (car body*))))))))))
    ;; ------------------------------------------------------------------------
    ;; generates the loop that turns flags to #t's
    (define (make-flags-body) ; called only when there are flags
      (with-syntax ([flags flags] [rest* rest*])
        #'(let loop ([xs rest*])
            (if (and (pair? xs) (keyword? (car xs)))
                (if (memq (car xs) 'flags)
                    (if (null? (cdr xs))
                        (list (car xs) true)
                        (list* (car xs)
                               true
                               (loop (cddr xs))))
                    (if (pair? (cdr xs))
                        (list* (car xs)
                               (cadr xs)
                               (loop (cddr xs)))
                        xs))
                xs))))
    ;; ------------------------------------------------------------------------
    ;; generates the part of the body that deals with rest-related stuff
    (define (make-keys-body expr)
      (let ([kb (with-syntax ([body (make-rest-body expr)] [keys keys])
                  #'(let* keys body))])
        (if (null? flags)
          kb
          (with-syntax ([keys-body kb] [flag-tweaks (make-flags-body)] [rest* rest*])
            #'(let ([rest* flag-tweaks]) keys-body)))))
    ;; ------------------------------------------------------------------------
    ;; more sanity tests (see commented code above -- search for "(*)")
    (when (null? keys)
      (let ([r (or all-keys other-keys other-keys+body body rest)])
        (if allow-other-keys?
          ;; allow-other-keys? ==>
          (unless r
            (serror #f "cannout allow other keys ~a"
                    "without using them in some way"))
          ;; (not allow-other-keys?) ==>
          (begin
            ;; can use #:body with no keys to forbid all keywords
            (when (and r (not (eq? r body)))
              (serror r "cannot use without #:key, #:flag, or ~a"
                      "#:allow-other-keys"))
            (when allow-duplicate-keys?
              (serror #f "cannot allow duplicate keys without ~a"
                      "#:key, #:flag, or #:allow-other-keys"))))))
    ;; ------------------------------------------------------------------------
    ;; body generation starts here
    (cond
     ;; no optionals or keys (or other-keys) => plain lambda
     [(and (null? opts) (null? keys) (not allow-other-keys?))
      (if (not body)
        ;; really just a plain lambda
        (with-syntax ([vars (append vars (or rest '()))] [expr expr])
          (syntax/loc stx (lambda vars expr)))
        ;; has body => forbid keywords
        (with-syntax ([vars (append vars body)] [expr expr] [body body])
          (syntax/loc stx
            (lambda vars
              (if (and (pair? body) (keyword? (car body)))
                (error* 'name "unknown keyword: ~.s" (car body))
                expr)))))]
     ;; no keys => make a case-lambda for optionals
     [(and (null? keys) (not (or body allow-other-keys?)))
      ;; cannot write a special case for having `body' here, because it
      ;; requires the special pop-non-keywords-for-optionals that is done
      ;; below, and generalizing that is a hassle with little benefit
      (let ([clauses (make-opt-clauses expr (or rest '()))])
        (with-syntax ([name name] [clauses clauses])
          (syntax/loc stx (letrec ([name (case-lambda . clauses)]) name))))]
     ;; no opts => normal processing of keywords etc
     [(null? opts)
      (with-syntax ([vars (append vars rest*)]
                    [body (make-keys-body expr)])
        (syntax/loc stx (lambda vars body)))]
     ;; both opts and keys => combine the above two
     ;; (the problem with this is that things that follow the required
     ;; arguments are always taken as optionals, even if they're keywords, so
     ;; the next piece of code is used.)
     #;
     [else
      (let ([clauses (make-opt-clauses (make-keys-body expr) rest*)])
        (with-syntax ([name name] [clauses clauses])
          (syntax/loc stx (letrec ([name (case-lambda . clauses)]) name))))]
     ;; both opts and keys => pop optionals as long as they're not keywords
     [else
      (with-syntax
          ([rest rest*]
           [vars (append vars rest*)]
           [body (make-keys-body expr)]
           [((optvar optexpr) ...)
            (apply append
                   (map (lambda (opt)
                          (with-syntax ([(ovar odef) opt] [rest rest*])
                            (list #'[otmp (if (null? rest)
                                            #t (keyword? (car rest)))]
                                  #'[ovar (if otmp odef (car rest))]
                                  #'[rest (if otmp rest (cdr rest))])))
                        opts))])
        (syntax/loc stx (lambda vars (let* ([optvar optexpr] ...) body))))]))
  (syntax-case stx ()
    [(_ formals expr0 expr ...)
     ;; check if there are only identifiers, and save the whole mess if so
     (if (let loop ([xs #'formals])
           (cond [(syntax? xs) (loop (syntax-e xs))]
                 [(symbol? xs) #t]
                 [(null? xs)   #t]
                 [(not (pair? xs)) #f]
                 [(symbol? (if (syntax? (car xs)) (syntax-e (car xs)) (car xs)))
                  (loop (cdr xs))]
                 [else #f]))
       #'(lambda formals expr0 expr ...)
       (generate-body #'formals #'(let () expr0 expr ...)))]))

(provide define/kw)
(define-syntax (define/kw stx)
  (syntax-case stx ()
    [(_ name val) (identifier? #'name) #'(define name val)]
    [(d/kw (name . args) body0 body ...)
     (syntax/loc stx (d/kw name (lambda/kw args body0 body ...)))]))

;; raise an appropriate exception
(define (error* who fmt . args)
  (raise (make-exn:fail:contract
          (apply format (string-append "~a: " fmt) who args)
          (current-continuation-marks))))

;; keyword searching utility (note: no errors for odd length)
(provide keyword-get)
(define keyword-get
  (case-lambda
   [(args keyword not-found)
    (let loop ([args args])
      (cond [(or (null? args) (null? (cdr args)) (not (keyword? (car args))))
             (not-found)]
            [(eq? (car args) keyword) (cadr args)]
            [else (loop (cddr args))]))]
   ;; the following makes another function call, but the code that is generated
   ;; by this module never gets here
   [(args keyword) (keyword-get* args keyword #f)]))

;; a private version of keyword-get that is used with simple values
(define (keyword-get* args keyword not-found)
  (let loop ([args args])
    (cond [(or (null? args) (null? (cdr args)) (not (keyword? (car args))))
           not-found]
          [(eq? (car args) keyword) (cadr args)]
          [else (loop (cddr args))])))

)
