#lang racket/base
(require (for-syntax racket/base
                     racket/struct-info)
         racket/stxparam)

;; A class system that is somewhat similar to `racket/class`, but
;; completely first order, with its structure nature exposed, and
;; where the notion of "method" is flexible to allow non-procedures in
;; the vtable. The run-time componention of a full expansion is
;; efficient, but beware that there are various quadratic factors in
;; intermediate expansions and compile-time data. There should be more
;; checks to make sure that method declarations are distinct, etc.
;;
;;  <class-defn> = (class <class-id> <clause> ...)
;;               | (class <class-id> #:extends <class-id> <clause> ...)
;;  <clause> = #:field [<field-id> <duplicatable-init-expr>] ...
;;           | #:public [<method-id> <method>] ...
;;           | #:private [<method-id> <method>] ...   ; cannot `send`
;;           | #:override [<method-id> <method>] ...
;;           | #:static [<method-id> <method>] ...    ; cannot override
;;           | #:property [<property-expr> <val-expr>] ...
;;  <method> = #f
;;           | (lambda <formals> <expr> ...+)
;;           | (case-lambda [<formals> <expr> ...+] ...)
;;           | <expr> ; must have explicit `self`, etc.
;;
;; A <class-id> and its <field>s behave as if they are in a `struct`
;; declaration where `create-<class-id>` is the constructor, but an
;; extra `vtable` field is added to the start of a class's structure
;; if it has no superclass. The `#:authentic` option is added
;; implicitly. The `property` clause supplies additional structure
;; type properties.
;;
;; A `public` method is one that can be overridden with `override` or
;; called via `send`. A `private` or `static` method cannot be
;; overridden, and a `private` method cannot be called via `send`. Bot
;; `private` and `static` methods can be called directly like
;; functions within another method (but `public` methods cannot be
;; called that way, and that restriction is intended to discourange
;; unnecessary indirections through methods that can be overridden).
;;
;; Normally, use
;;   (new <class-id> #:field [<field-id> <expr] ...)
;; to create an instance of the class, where each unmentioned
;; <field-id> gets its default value. To override methods for just
;; this object, use
;;   (new <class-id>
;;        #:field [<field-id> <expr] ...
;;        #:override [<method-id> <method>] ...)
;; but beware that it involves allocating a new vtable each
;; time the `new` expression is evaluated.
;;
;; Use
;;    (send <class-id> <obj-expr> <method-id> <arg-expr> ...)
;; to call a method, or
;;    (method <class-id> <obj-expr> <method-id>)
;; to get a method that expects the object as its first argument.
;;
;; In a method, `field`s, `private`s, and `static`s can be accessed
;; directly by name, and `this` is bound to the current object. A
;; method overridden in `new` can only access `field`s.
;;
;; Use
;;   (with-object <class-id> <object-expr)
;;      <body> ...+)
;; to directly reference `field`s and `static`s in the <body>s.

(provide class
         this
         new
         send
         method
         with-object)

(define-syntax-parameter this
  (lambda (stx)
    (raise-syntax-error #f "illegal use outside of a method" stx)))

(begin-for-syntax
  (struct class-info (struct-info methods-id vtable-id vtable-accessor-id fields methods statics)
    #:property prop:struct-info (lambda (ci)
                                  (class-info-struct-info ci))))

(define-syntax (class stx)
  (define id (syntax-case stx ()
               [(_ id . _) #'id]))
  (define super-id (syntax-case stx ()
                     [(_ id #:extends super-id . _)
                      #'super-id]
                     [_ #f]))
  (define super-ci (and super-id
                        (syntax-local-value super-id)))
  (define (combine-ids ctx . elems)
    (datum->syntax ctx (string->symbol (apply string-append
                                              (for/list ([elem (in-list elems)])
                                                (if (string? elem)
                                                    elem
                                                    (symbol->string (syntax-e elem))))))))
  (define methods-id (combine-ids #'here id "-methods"))
  (define (add-procs base-id l what #:can-immutable? [can-immutable? #f])
    (for/list ([e (in-list l)])
      (syntax-case e ()
        [(id expr #:immutable)
         can-immutable?
         (list #'id #'expr (combine-ids base-id base-id "-" #'id) #f)]
        [(id expr)
         (list #'id #'expr (combine-ids base-id base-id "-" #'id) (combine-ids #'id "set-" base-id "-" #'id "!"))]
        [_ (raise-syntax-error #f (format "bad ~a clause" what) stx e)])))
  (define groups (extract-groups
                  stx
                  (syntax-case stx ()
                    [(_ _ #:extends _ . rest) #'rest]
                    [(_ _ . rest) #'rest])
                  '(#:field #:public #:override #:private #:static #:property)))
  (define (extract-group tag) (reverse (hash-ref groups tag '())))
  (define new-fields (add-procs id (extract-group '#:field) "field" #:can-immutable? #t))
  (define new-methods (add-procs methods-id (extract-group '#:public) "public"))
  (define override-methods (extract-group '#:override))
  (define locals (extract-group '#:private))
  (define statics (extract-group '#:static))
  (define properties (for/list ([prop (in-list (extract-group '#:property))])
                       (cons '#:property prop)))
  (define all-fields (if super-ci
                         (append (class-info-fields super-ci) new-fields)
                         new-fields))
  (for ([override (in-list override-methods)])
    (syntax-case override ()
      [(method-id _) (check-member stx #'method-id (if super-ci (class-info-methods super-ci) null) "method")]
      [_ (raise-syntax-error #f "bad override clause" stx override)]))
  (with-syntax ([((field-id field-init-expr field-accessor-id field-mutator-maybe-id) ...) all-fields]
                [((local-id local-expr) ...) locals]
                [((static-id static-expr) ...) statics]
                [(local-tmp-id ...) (generate-temporaries locals)]
                [(static-tmp-id ...) (generate-temporaries statics)]
                [((parent-static-id parent-static-tmp-id) ...) (if super-ci
                                                                   (class-info-statics super-ci)
                                                                   null)])
    (with-syntax ([local-bindings #'[([field-id field-accessor-id field-mutator-maybe-id] ...)
                                     ([local-id local-tmp-id] ... [static-id static-tmp-id] ...
                                                              [parent-static-id parent-static-tmp-id] ...)]])
      (define wrapped-new-methods
        (for/list ([new-method (in-list new-methods)])
          (syntax-case new-method ()
            [(method-id method-init-expr . rest)
             #'(method-id (let ([method-id (bind-locals-in-body local-bindings method-init-expr)])
                            method-id)
                          . rest)])))
      (define all-methods/vtable (if super-ci
                                     (append (for/list ([method (in-list (class-info-methods super-ci))])
                                               (syntax-case method ()
                                                 [(method-id method-init-expr . rest)
                                                  (or (for/or ([override (in-list override-methods)])
                                                        (syntax-case override ()
                                                          [(override-id override-init-expr . _)
                                                           (and (eq? (syntax-e #'method-id) (syntax-e #'override-id))
                                                                (list* #'method-id
                                                                       #'(let ([method-id
                                                                                (bind-locals-in-body
                                                                                 local-bindings
                                                                                 override-init-expr)])
                                                                           method-id)
                                                                       #'rest))]))
                                                      method)]))
                                             wrapped-new-methods)
                                     wrapped-new-methods))
      (define vtable-id (combine-ids #'here id "-vtable"))
      (define all-methods/next (for/list ([method (in-list all-methods/vtable)])
                                 (syntax-case method ()
                                   [(method-id method-init-expr method-accessor-id . _)
                                    (with-syntax ([vtable-id vtable-id])
                                      (list #'method-id
                                            #'(method-accessor-id vtable-id)
                                            #'method-accessor-id))])))
      (with-syntax ([id id]
                    [(super-ids ...) (if super-id
                                         (list super-id)
                                         null)]
                    [quoted-super-id (and super-id #`(quote-syntax #,super-id))]
                    [(vtable-ids ...) (if super-id
                                          null
                                          (list (datum->syntax id 'vtable)))]
                    [vtable-accessor-id (if super-ci
                                            (class-info-vtable-accessor-id super-ci)
                                            (combine-ids id id "-vtable"))]
                    [vtable-id vtable-id]
                    [struct:id (combine-ids id "struct:" id)]
                    [make-id (combine-ids id "create-" id)]
                    [id? (combine-ids id id "?")]
                    [methods-id methods-id]
                    [(super-methods-ids ...) (if super-ci
                                                 (list (class-info-methods-id super-ci))
                                                 null)]
                    [(new-field-id/annotated ...) (for/list ([new-field (in-list new-fields)])
                                                    (syntax-case new-field ()
                                                      [(id _ _ #f) #'id]
                                                      [(id . _) #'[id #:mutable]]))]
                    [((new-method-id . _) ...) new-methods]
                    [((_ _ rev-field-accessor-id . _) ...) (reverse all-fields)]
                    [((_ _ _ rev-field-mutator-maybe-id) ...) (reverse all-fields)]
                    [((method-id method-init-expr/vtable . _) ...) all-methods/vtable]
                    [((_ method-init-expr/next  method-accessor-id) ...) all-methods/next]
                    [((propss ...) ...) properties])
        #`(begin
            (struct id super-ids ... (vtable-ids ... new-field-id/annotated ...)
              #:omit-define-syntaxes
              #:constructor-name make-id
              #:authentic
              propss ... ...)
            (struct methods-id super-methods-ids ... (new-method-id ...))
            (define vtable-id (methods-id method-init-expr/vtable ...))
            (define static-tmp-id (let ([static-id (bind-locals-in-body local-bindings static-expr)])
                                    static-id))
            ...
            (define local-tmp-id (let ([local-id (bind-locals-in-body local-bindings local-expr)])
                                   local-id))
            ...
            (define-syntax id
              (class-info (list (quote-syntax struct:id)
                                (quote-syntax make-id)
                                (quote-syntax id?)
                                (list (quote-syntax rev-field-accessor-id) ... (quote-syntax vtable-accessor-id))
                                (list (maybe-quote-syntax rev-field-mutator-maybe-id) ... #f)
                                quoted-super-id)
                          (quote-syntax methods-id)
                          (quote-syntax vtable-id)
                          (quote-syntax vtable-accessor-id)
                          (list (list (quote-syntax field-id) (quote-syntax field-init-expr)
                                      (quote-syntax field-accessor-id) (maybe-quote-syntax field-mutator-maybe-id))
                                ...)
                          (list (list (quote-syntax method-id) (quote-syntax method-init-expr/next)
                                      (quote-syntax method-accessor-id))
                                ...)
                          (list (list (quote-syntax static-id) (quote-syntax static-tmp-id))
                                ...
                                (list (quote-syntax parent-static-id) (quote-syntax parent-static-tmp-id))
                                ...))))))))

(define-syntax (bind-locals-in-body stx)
  (syntax-case stx (lambda case-lambda)
    [(_ locals #f) #'#f]
    [(_ locals (form . rest))
     (with-syntax ([(_ _ orig) stx])
       #'(bind-locals-in-body locals form orig))]
    [(_ locals expr) #'expr]
    [(_ locals ctx (lambda args body0 body ...))
     #'(bind-locals-in-body locals ctx (case-lambda [args body0 body ...]))]
    [(_ locals ctx (case-lambda clause ...))
     (let ([new-clauses
            (for/list ([clause (in-list (syntax->list #'(clause ...)))])
              (syntax-case clause ()
                [[args body0 body ...]
                 (with-syntax ([(arg-id ...) (extract-arg-ids #'args)])
                   (with-syntax ([(arg-tmp ...) (generate-temporaries #'(arg-id ...))])
                     (with-syntax ([tmp-args (substitute-arg-ids #'args (syntax->list #'(arg-tmp ...))
                                                                 #'this-id #'locals #'ctx)])
                       #'[(this-id . tmp-args)
                          (syntax-parameterize ([this (make-rename-transformer #'this-id)])
                            (bind-locals
                             locals
                             this-id ctx
                             (let-syntax ([arg-id (make-rename-transformer #'arg-tmp)] ...)
                               body0 body ...)))])))]))])
       (define rhs (syntax-case stx () [(_ _ _ rhs) #'rhs]))
       (cond
         [(= 1 (length new-clauses))
          (with-syntax ([new-clause (car new-clauses)])
            (syntax/loc rhs
              (lambda . new-clause)))]
         [else
          (with-syntax ([(new-clause ...) new-clauses])
            (syntax/loc rhs
              (case-lambda new-clause ...)))]))]
    [(_ locals _ expr)
     #'expr]))

(define-syntax (bind-locals stx)
  (syntax-case stx ()
    [(_ [([field-id field-accessor-id field-mutator-maybe-id] ...)
         ([static-id static-tmp-id] ...)]
        this-id ctx body)
     (with-syntax ([(field-id ...) (for/list ([field-id (in-list (syntax->list #'(field-id ...)))])
                                     (datum->syntax #'ctx (syntax-e field-id)))]
                   [(static-id ...) (for/list ([static-id (in-list (syntax->list #'(static-id ...)))])
                                      (datum->syntax #'ctx (syntax-e static-id)))])
       #'(let-syntax ([field-id (make-set!-transformer
                                 (lambda (stx)
                                   (syntax-case stx (set!)
                                     [(set! _ rhs) (if (syntax-e (quote-syntax field-mutator-maybe-id))
                                                       (syntax/loc stx (field-mutator-maybe-id this-id rhs))
                                                       (raise-syntax-error #f "field is immutable" stx))]
                                     [(_ arg (... ...)) (syntax/loc stx ((field-accessor-id this-id) arg (... ...)))]
                                     [else (syntax/loc stx (field-accessor-id this-id))])))]
                      ...
                      [static-id (lambda (stx)
                                   (syntax-case stx ()
                                     [(_ arg (... ...))
                                      (syntax/loc stx (static-tmp-id this-id arg (... ...)))]))]
                      ...)
           body))]))

(define-syntax (new stx)
  (syntax-case stx ()
    [(_ class-id clause ...)
     (let ([ci (and (identifier? #'class-id)
                    (syntax-local-value #'class-id (lambda () #f)))])
       (unless (class-info? ci)
         (raise-syntax-error #f "not a class identifier" stx #'class-id))
       (define groups (extract-groups stx #'(clause ...) '(#:field #:override)))
       (define inits (reverse (hash-ref groups '#:field '())))
       (define overrides (reverse (hash-ref groups '#:override '())))
       (for ([init (in-list inits)])
         (syntax-case init ()
           [(field-id _) (check-member stx #'field-id (class-info-fields ci) "field")]
           [_ (raise-syntax-error #f "bad field-inialization clause" stx init)]))
       (for ([override (in-list overrides)])
         (syntax-case override ()
           [(method-id _) (check-member stx #'method-id (class-info-methods ci) "method")]
           [_ (raise-syntax-error #f "bad method-override clause" stx override)]))
       (define field-exprs (for/list ([field (in-list (class-info-fields ci))])
                             (syntax-case field ()
                               [(field-id field-expr . _)
                                (or (for/or ([init (in-list inits)])
                                      (syntax-case init ()
                                        [(id expr)
                                         (and (eq? (syntax-e #'id) (syntax-e #'field-id))
                                              #'expr)]))
                                    #'field-expr)])))
       (with-syntax ([make-id (cadr (class-info-struct-info ci))]
                     [vtable-id (class-info-vtable-id ci)]
                     [(field-expr ...) field-exprs])
         (cond
           [(null? overrides)
            (syntax/loc stx (make-id vtable-id field-expr ...))]
           [else
            (with-syntax ([methods-id (class-info-methods-id ci)]
                          [(method-expr ...)
                           (for/list ([method (in-list (class-info-methods ci))])
                             (syntax-case method ()
                               [(id _ selector-id . _)
                                (or (for/or ([override (in-list overrides)])
                                      (syntax-case override ()
                                        [(override-id expr)
                                         (and (eq? (syntax-e #'override-id) (syntax-e #'id))
                                              (with-syntax ([((field-id _ field-accessor-id field-mutator-maybe-id) ...)
                                                             (class-info-fields ci)])
                                                #'(bind-locals-in-body
                                                   [([field-id field-accessor-id field-mutator-maybe-id] ...)
                                                    ()]
                                                   expr)))]))
                                    #'(selector-id vtable-id))]))])
              (syntax/loc stx (make-id (methods-id method-expr ...)
                                       field-expr ...)))])))]
    [(_ class-id init ...)
     (syntax/loc stx (new class-id #:override () init ...))]))

(define-for-syntax (send-or-method stx call?)
  (syntax-case stx ()
    [(_ class-id obj method-id arg ...)
     (let ([ci (and (identifier? #'class-id)
                    (syntax-local-value #'class-id (lambda () #f)))])
       (unless (class-info? ci)
         (raise-syntax-error #f "not a class identifier" stx #'class-id))
       (define make-access
         (or (for/or ([method (in-list (class-info-methods ci))])
               (syntax-case method ()
                 [(id _ accessor-id)
                  (and (eq? (syntax-e #'id) (syntax-e #'method-id))
                       (lambda (o)
                         (with-syntax ([vtable-accessor-id (class-info-vtable-accessor-id ci)]
                                       [o o])
                           #'(accessor-id (vtable-accessor-id o)))))]))
             (and call?
                  (for/or ([static (in-list (class-info-statics ci))])
                    (syntax-case static ()
                      [(id tmp-id)
                       (and (eq? (syntax-e #'id) (syntax-e #'method-id))
                            (lambda (o)
                              #'tmp-id))])))
             (raise-syntax-error #f "cannot find method" stx #'method-id)))
       (if call?
           (with-syntax ([proc (make-access #'o)])
             #'(let ([o obj])
                 (proc o arg ...)))
           (make-access #'obj)))]))

(define-syntax (send stx)
  (send-or-method stx #t))

;; Gets a method to be called as a procedure, where the call must
;; include the "self" argument --- so, less safe than `send`, but
;; allows external handling for a method that is #f.
(define-syntax (method stx)
  (syntax-case stx ()
    [(_ class-id obj method-id)
     (send-or-method stx #f)]))

(define-syntax (with-object stx)
  (syntax-case stx ()
    [(_ class-id obj-expr body0 body ...)
     (let ([ci (and (identifier? #'class-id)
                    (syntax-local-value #'class-id (lambda () #f)))])
       (unless (class-info? ci)
         (raise-syntax-error #f "not a class identifier" stx #'class-id))
       (with-syntax ([((field-id _ field-accessor-id field-mutator-maybe-id) ...)
                      (class-info-fields ci)]
                     [((static-id static-tmp-id) ...) (class-info-statics ci)])
         (with-syntax ([local-bindings #'[([field-id field-accessor-id field-mutator-maybe-id] ...)
                                          ([static-id static-tmp-id] ...)]])
           #'(let ([o obj-expr])
               (bind-locals local-bindings o obj-expr (let () body0 body ...))))))]))

;; ----------------------------------------

(define-for-syntax (extract-groups stx l-stx ok-groups)
  (let loop ([l-stx l-stx] [current-group #f] [groups #hasheq()])
    (syntax-case l-stx ()
      [() groups]
      [(kw . rest)
       (memq (syntax-e #'kw) ok-groups)
       (loop #'rest (syntax-e #'kw) groups)]
      [(kw . rest)
       (keyword? (syntax-e #'kw))
       (raise-syntax-error #f "unrecognized section keyword" stx #'kw)]
      [(other . rest)
       (if current-group
           (loop #'rest
                 current-group
                 (hash-update groups current-group (lambda (l) (cons #'other l)) null))
           (raise-syntax-error #f "need an initial section keyword, such as `#:field`" stx #'other))])))

(define-for-syntax (check-member stx id l what)
  (or (for/or ([e (in-list l)])
        (syntax-case e ()
          [(e-id . _)
           (eq? (syntax-e #'e-id) (syntax-e id))]))
      (raise-syntax-error #f (format "no such ~a" what) stx id)))

(begin-for-syntax
  (define-syntax maybe-quote-syntax
    (syntax-rules ()
      [(_ #f) #f]
      [(_ e) (quote-syntax e)])))

(define-for-syntax (extract-arg-ids args)
  (let loop ([args args])
    (syntax-case args ()
      [() null]
      [id
       (identifier? #'id)
       (list #'id)]
      [(id . rest)
       (identifier? #'id)
       (cons #'id (loop #'rest))]
      [(kw . rest)
       (keyword? (syntax-e #'kw))
       (loop #'rest)]
      [([id val-expr] . rest)
       (cons #'id (loop #'rest))])))

(define-for-syntax (substitute-arg-ids args tmp-ids this-id locals ctx)
  (let loop ([args args] [tmp-ids tmp-ids] [done-ids '()] [done-tmp-ids '()])
    (syntax-case args ()
      [() null]
      [id
       (identifier? #'id)
       (car tmp-ids)]
      [(id . rest)
       (identifier? #'id)
       (cons (car tmp-ids) (loop #'rest (cdr tmp-ids)
                                 (cons #'id done-ids)
                                 (cons (car tmp-ids) done-tmp-ids)))]
      [(kw . rest)
       (keyword? (syntax-e #'kw))
       (cons #'kw (loop #'rest tmp-ids done-ids done-tmp-ids))]
      [([id val-expr] . rest)
       (let ([val-expr
              (with-syntax ([this-id this-id]
                            [locals locals]
                            [ctx ctx]
                            [(done-id ...) done-ids]
                            [(done-tmp-id ...) done-tmp-ids])
                #'(syntax-parameterize ([this (make-rename-transformer #'this-id)])
                    (bind-locals
                     locals
                     this-id ctx
                     (let-syntax ([done-id (make-rename-transformer #'done-tmp-id)] ...)
                               val-expr))))])
         (cons (list (car tmp-ids) val-expr)
               (loop #'rest (cdr tmp-ids)
                     (cons #'id done-ids)
                     (cons (car tmp-ids) done-tmp-ids))))])))

;; ----------------------------------------

(module+ main
  (class example
    #:field
    [a 1 #:immutable]
    [b 2]
    #:private
    [other (lambda (q) (list q this))]
    #:static
    [enbox (lambda (v #:opt [opt (vector v a)])
             (box (vector a v opt)))]
    #:public
    [q #f]
    [m (lambda (z #:maybe [maybe 9]) (list a (other b) maybe))]
    [n (lambda (x y z) (vector a b (enbox x) y z))])

  (class sub #:extends example
    #:override
    [m (lambda (z) 'other)]
    #:field
    [c 3]
    #:property
    [prop:custom-write (lambda (s o m)
                         (write 'sub: o)
                         (write (sub-d s) o))]
    #:field
    [d 4])

  (define ex (new example
                  #:field
                  [b 5]))

  (send example ex m 'ok #:maybe 'yep)
  (method example ex m)
  (new sub #:field [d 5])
  (send example (new sub) m 'more)
  (set-example-b! ex 6)
  (send example ex enbox 88)
  (send example ex enbox 88 #:opt 'given)

  (define ex2 (new example
                   #:override
                   [q (lambda (x y z)
                        (box (vector x y z a b)))]
                   #:field
                   [b 'b]
                   [a 'a]))
  (send example ex2 n 1 2 3)

  (with-object example ex
    (list a b (enbox 'c))))
