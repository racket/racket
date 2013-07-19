(module trait racket/base
  (require racket/class
           racket/list)
  (require (for-syntax racket/list racket/base
                      syntax/stx
                      syntax/boundmap
                      syntax/kerncase
                      ;; This should be part of a public expand-time API
                      ;;  exported by the class system:
                      (only-in "private/classidmap.rkt"
                               generate-class-expand-context)))
  
  (provide (rename-out [:trait trait])
           trait?
           trait->mixin
           trait-sum 
           trait-exclude trait-exclude-field 
           trait-alias 
           trait-rename trait-rename-field)
  
  ;; A trait is a list of supplied methods.
  ;; Each supplied method is:
  ;;   * an external name
  ;;   * supplies inherit?
  ;;   * supplies super?
  ;;   * supplies inner?
  ;;   * overrides?
  ;;   * augments?
  ;;   * list of required methods (external names) for inherit
  ;;   * list of required methods (external names) for super
  ;;   * list of required methods (external names) for inner
  ;;   * list of required methods (external names) for inherit-fields
  ;;   * a mixin patameterized by all external names
  ;;   * an indrection mixin for supers
  
  (define-struct trait (methods fields))
  
  (define-struct method (name inherit? super? inner?
                              override? augment?
                              need-inherit need-super need-inner need-field
                              make-mixin
                              make-super-indirection-mixin))

  (define-struct feeld (name make-mixin))
  
  (define-syntax (:trait stx)
    ;; The main compiler (helpers are below):
    (define (main stx)
      (syntax-case stx ()
        [(_ clause ...)
         (let* ([clauses (syntax->list #'(clause ...))]
                [expanded-clauses (expand-body clauses)])
           ;; Pull out declared names:
           (let-values ([(publics pubments
                                  overrides augments augrides overments
                                  inherits inherits/super inherits/inner
                                  inherit-fields)
                         (extract expanded-clauses
                                  (map syntax->list
                                       (syntax->list
                                        #'((public public-final) 
                                           (pubment)
                                           (override override-final) 
                                           (augment augment-final) 
                                           (augride)
                                           (overment)
                                           (inherit) (inherit/super) (inherit/inner)
                                           (inherit-field)))))]
                        [(fields)
                         (extract-fields expanded-clauses)])
             ;; Every declaration implies direct use for other declarations:
             (let* ([to-inherit
                     (append publics pubments
                             overrides augments augrides overments
                             inherits inherits/super inherits/inner)]
                    [to-inherit-fields 
                     (append fields inherit-fields)]
                    [decls
                     (append to-inherit-fields to-inherit)])
               ;; Check distinct delcarations:
               (check-distinct-external-names to-inherit)
               (check-distinct-external-names to-inherit-fields)
               (check-distinct-internal-names decls)
               
               ;; Some declarations imply use via `super' or `inner':
               (let ([to-super (append overrides inherits/super)]
                     [to-inner (append augments pubments inherits/inner)])
                 
                 (let ([to-inherit-only
                        (filter (lambda (n)
                                  (not (or (ormap (lambda (n2) (internal-identifier=? n n2))
                                                  to-super)
                                           (ormap (lambda (n2) (internal-identifier=? n n2))
                                                  to-inner))))
                                to-inherit)])
                   
                   ;; Current method-making function with respect to the
                   ;;  common part:
                   (let* ([bindings (make-bindings expanded-clauses)]
                          [compose-method (compose-method-with-requirements
                                           bindings
                                           to-inherit-only to-super to-inner
                                           to-inherit-fields)])
                     
                     ;; Build a mixin and `method' record for each declaration:
                     (with-syntax ([(method ...)
                                    (append
                                     (map (compose-method #'override #t #f #f  #f #f  #f) publics)
                                     (map (compose-method #'overment #t #t #f  #f #f  #f) pubments)
                                     (map (compose-method #'override #t #t #f  #t #f  #f) overrides)
                                     (map (compose-method #'overment #t #f #f  #t #f  #t) overments)
                                     (map (compose-method #'augment  #t #f #t  #f #t  #f) augments)
                                     (map (compose-method #'augride  #t #f #f  #f #t  #f) augrides))]
                                   [(field ...)
                                    (map (compose-field bindings) fields)])

                       (bound-identifier-mapping-for-each 
                        bindings
                        (lambda (key val)
                          (when val
                            (raise-syntax-error
                             #f
                             "definition has no corresponding declaration (e.g., public)"
                             stx
                             key))))

                       ;; Combine the result into a trait:
                       #'(make-trait (list method ...)
                                     (list field ...)))))))))]))

    (define (expand-body clauses)
      ;; For now, we expand naively: no support for internal define-syntax,
      ;; and no shadowing of syntax with method definitions.
      (let ([stop-forms
             (append
              (syntax->list
               #'(private 
                   public public-final pubment
                   override override-final augment augment-final augride overment
                   inherit inherit/super inherit/inner
                   this super inner
                   field inherit-field))
              (kernel-form-identifier-list))]
            [expand-context (generate-class-expand-context)])
        (let loop ([l clauses])
          (cond
            [(null? l) null]
            [else (let ([e (local-expand (car l)
                                         expand-context
                                         stop-forms)])
                    (syntax-case e (begin define-values)
                      [(begin expr ...)
                       (loop (append
                              (syntax->list (syntax (expr ...)))
                              (cdr l)))]
                      [(define-values (id) rhs)
                       (cons e (loop (cdr l)))]
                      [(field (id expr) ...)
                       (if (andmap (lambda (id)
                                     (or (identifier? id)
                                         (syntax-case id ()
                                           [(a b)
                                            (and (identifier? #'a)
                                                 (identifier? #'b))]
                                           [_else #f])))
                                   (syntax->list #'(id ...)))
                           (cons e (loop (cdr l)))
                           (raise-syntax-error
                            #f
                            "bad syntax"
                            e))]
                      [(id . rest)
                       (ormap (lambda (x) (free-identifier=? x #'id))
                              (syntax->list
                               #'(public public-final pubment
                                         override override-final augment augment-final augride overment
                                         inherit inherit/super inherit/inner
                                         inherit-field)))
                       (let ([l2 (syntax->list #'rest)])
                         (if (and l2
                                  (andmap (lambda (i)
                                            (or (identifier? i)
                                                (syntax-case i ()
                                                  [(a b)
                                                   (and (identifier? #'a)
                                                        (identifier? #'b))]
                                                  [_else #f])))
                                          l2))
                             (cons e (loop (cdr l)))
                             (raise-syntax-error
                              #f
                              "bad syntax (inside trait)"
                              e)))]
                      [(define-values . _)
                       (raise-syntax-error
                        #f
                        "bad syntax"
                        e)]
                      [(field . _)
                       (raise-syntax-error
                        #f
                        "bad syntax"
                        e)]
                      [else
                       (raise-syntax-error
                        #f
                        "not allowed in a trait"
                        e)]))]))))
    
    (define (extract expanded-clauses keyword-mapping)
      (let loop ([l expanded-clauses]
                 [results (map (lambda (x) null) keyword-mapping)])
        (cond
          [(null? l) (apply values results)]
          [else
           (let ([kw (stx-car (car l))])
             (if (or (free-identifier=? kw #'define-values)
                     (free-identifier=? kw #'field))
                 (loop (cdr l) results)
                 (loop (cdr l)
                       (let iloop ([mapping keyword-mapping]
                                   [results results])
                         (if (ormap (lambda (x) (free-identifier=? kw x))
                                    (car mapping))
                             (cons (append (stx->list (stx-cdr (car l)))
                                           (car results))
                                   (cdr results))
                             (cons (car results)
                                   (iloop (cdr mapping)
                                          (cdr results))))))))])))

    (define (extract-fields expanded-clauses)
      (apply
       append
       (map (lambda (clause)
              (syntax-case clause (field)
                [(field [id expr] ...)
                 (syntax->list #'(id ...))]
                [_else null]))
            expanded-clauses)))
    
    (define (make-bindings expanded-clauses)
      (let ([boundmap (make-bound-identifier-mapping)])
        (for-each (lambda (clause)
                    (syntax-case clause (define-values field)
                      [(define-values (id) rhs)
                       (bound-identifier-mapping-put! boundmap #'id #'rhs)]
                      [(field [id expr] ...)
                       (for-each (lambda (id expr)
                                   (bound-identifier-mapping-put! boundmap (internal-name id) expr))
                                 (syntax->list #'(id ...))
                                 (syntax->list #'(expr ...)))]
                      [_else (void)]))
                  expanded-clauses)
        boundmap))

    (define (internal-identifier=? a b)
      (bound-identifier=? (internal-name a) (internal-name b)))

    (define (internal-name decl)
      (if (identifier? decl)
          decl
          (stx-car decl)))

    (define (external-name decl)
      (if (identifier? decl)
          decl
          (stx-car (stx-cdr decl))))
    
    (define (check-distinct-names method-decls
                                  what which
                                  make-identifier-mapping
                                  identifier-mapping-get
                                  identifier-mapping-set!)
      (let ([idmap (make-identifier-mapping)])
        (for-each (lambda (decl)
                    (let ([ext-id (which decl)])
                      (when (identifier-mapping-get 
                             idmap ext-id
                             (lambda ()
                               (identifier-mapping-set!
                                idmap ext-id
                                #t)
                               #f))
                        (raise-syntax-error
                         #f
                         (format "duplicate definition of ~a name in trait"
                                 what)
                         ext-id))))
                  method-decls)))
    
    (define (check-distinct-external-names method-decls)
      (check-distinct-names method-decls
                            "external" external-name
                            make-module-identifier-mapping
                            module-identifier-mapping-get
                            module-identifier-mapping-put!))
    
    (define (check-distinct-internal-names method-decls)
      (check-distinct-names method-decls
                            "internal" internal-name
                            make-bound-identifier-mapping
                            bound-identifier-mapping-get
                            bound-identifier-mapping-put!))
    
    (define (((compose-method-with-requirements binding-map
                                                to-inherit to-super to-inner
                                                to-inherit-field)
              keyword inherit? super? inner?  override? augment? always-deep?)
             name)
      (let ([impl (bound-identifier-mapping-get binding-map (internal-name name))]
            [to-inherit (if always-deep?
                            (filter (lambda (n) (not (internal-identifier=? n name)))
                                    to-inherit)
                            to-inherit)])
        (with-syntax ([(to-inherit ...) to-inherit]
                      [(to-super ...) to-super]
                      [(to-inner ...) to-inner]
                      [(to-inherit-field ...) to-inherit-field]
                      [(to-inherit-ext ...) (map external-name to-inherit)]
                      [(to-super-ext ...) (map external-name to-super)]
                      [(to-inner-ext ...) (map external-name to-inner)]
                      [(to-inherit-field-ext ...) (map external-name to-inherit-field)]
                      [(to-inherit-arg ...) (generate-temporaries to-inherit)]
                      [(to-super-arg ...) (generate-temporaries to-super)]
                      [(to-inner-arg ...) (generate-temporaries to-inner)]
                      [(to-inherit-field-arg ...) (generate-temporaries to-inherit-field)]
                      [impl impl]
                      [declare keyword]
                      [this-method (if always-deep?
                                       name
                                       #'this-method)]
                      [wrap-super-indirect (if override?
                                               #'values
                                               #'omit)]
                      [wrap-inner-indirect (if augment?
                                               #'values
                                               #'omit)])
          ;; for tracking unused bindings at the end:
          (bound-identifier-mapping-put! binding-map (internal-name name) #f)
          ;; generate method:
          #`(make-method
             (member-name-key #,(external-name name))
             #,inherit? #,super? #,inner?  #,override? #,augment?
             (list (member-name-key to-inherit-ext) ...)
             (list (member-name-key to-super-ext) ...)
             (list (member-name-key to-inner-ext) ...)
             (list (member-name-key to-inherit-field-ext) ...)
             (lambda (this-method-arg to-inherit-arg ...
                                      to-super-arg ...
                                      to-inner-arg ...
                                      to-inherit-field-arg ...)
               (define-member-name this-method this-method-arg)
               (define-member-name to-inherit-ext to-inherit-arg) ...
               (define-member-name to-super-ext to-super-arg) ...
               (define-member-name to-inner-ext to-inner-arg) ...
               (define-member-name to-inherit-field-ext to-inherit-field-arg) ...
               (lambda (%)
                 (class %
                   (inherit to-inherit ...)
                   (inherit/super to-super ...)
                   (inherit/inner to-inner ...)
                   (inherit-field to-inherit-field ...)
                   (declare this-method)
                   (define this-method (let ([#,(internal-name name) impl])
                                         #,(internal-name name)))
                   (super-new))))
             ;; For `super' call indirections:
             (wrap-super-indirect
              (lambda (name-arg super-name-arg)
                (define-member-name name name-arg)
                (define-member-name super-name super-name-arg)
                (lambda (%)
                  (class %
                    (override name)
                    (inherit/super super-name)
                    (define name (similar-lambda impl (super super-name)))
                    (super-new)))))))))

    (define ((compose-field binding-map) name)
      (let ([impl (bound-identifier-mapping-get binding-map (internal-name name))])
        ;; for tracking unused bindings at the end:
        (bound-identifier-mapping-put! binding-map (internal-name name) #f)
        ;; generate method:
        #`(make-feeld
           (member-name-key #,(external-name name))
           (lambda (name-arg)
             (define-member-name #,(external-name name) name-arg)
             (lambda (%)
               (class %
                 (field [#,name #,impl])
                 (super-new)))))))

    (main stx))
  
  (define-syntax (similar-lambda stx)
    ;; Try to get arity the same:
    (syntax-case stx (lambda)
      [(_ (lambda (id ...) . __) (new-body ...))
       #'(lambda (id ...) (new-body ... id ...))]
      ;; Generic case:
      [(_ method-lambda (new-body ...))
       #'(lambda args (new-body ... . args))]))
  
  (define-syntax (omit stx) #'#f)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  trait->mixin
  
  (define (trait->mixin t)
    (let ([methods (trait-methods t)])
      
      ;; If a needed inherit, super, or inner is not immediately satisified, 
      ;;  we can just leave it to the superclass.
      ;; But we can't expect a super and have it introduced as non-overriding. 
      ;;  We need to check in trait-sum, trait-alias, etc., because we'll
      ;;  have to use dummy introductions when stacking up the mixins, and
      ;;  there might be no error otherwise.
        
      ;; Order the mixins. If M1 super-calls M2 and we have an override
      ;;  for M2, then try to mix M2 later. Similarly, if M1 inner-calls M2
      ;;  and we have an augment for M2, try to mix M2 earlier.
      ;; We'll have to break cycles by inserting indirections, but we can't
      ;;  do that for `inner'; consequently, an `inner' from M1 to M2
      ;;  might land at an implementation in the same trait!
      ;; For simplicty, we sort right now by just all augments first
      ;;  and all overrides last. In the common case where methods
      ;;  only self-call supers and inners, that will work fine.
        (let loop ([methods (sort methods
                                  (lambda (a b)
                                    (or (method-augment? a)
                                        (method-override? b))))]
                   ;; Start by adding mixins for fields. Then continue
                   ;; by mixing a dummy method for each public/pubment
                   ;; method. We'll override it, but having it here at the start
                   ;; means that the methods can refer to each other via
                   ;; `inherit'.
                   [mixin (let loop ([methods methods]
                                     [mixin (let loop ([mixin (lambda (%) %)]
                                                       [fields (trait-fields t)])
                                              (cond
                                               [(null? fields) mixin]
                                               [else (let ([mix ((feeld-make-mixin (car fields))
                                                                 (feeld-name (car fields)))])
                                                       (loop (lambda (%) (mix (mixin %)))
                                                             (cdr fields)))]))])
                            (cond
                              [(null? methods) mixin]
                              [else (let ([method (car methods)])
                                      (loop (cdr methods)
                                            (if (or (method-override? method)
                                                    (method-augment? method))
                                                mixin
                                                (introduce-into-mixin
                                                 (method-name method)
                                                 mixin))))]))]
                   [super-indirections null])
          (cond
            [(null? methods)
             ;; No more methods to add, so just insert needed
             ;; super indirections (as accumulated when adding
             ;; methods before):
             (let loop ([indirections super-indirections]
                        [mixin mixin])
               (cond
                 [(null? indirections) mixin]
                 [else (let ([method (list-ref (car indirections) 2)])
                         (loop (cdr indirections)
                               (let ([mix ((method-make-super-indirection-mixin method)
                                           (method-name method)
                                           (cadar indirections))])
                                 (lambda (%) (mix (mixin %))))))]))]
            [else
             ;; Add one method:
             (let*-values ([(method) (car methods)]
                           ;; Rename method, in case we need a super
                           ;; indirection:
                           [(name)
                            (if (and (method-override? method)
                                     (ormap (lambda (m)
                                              (ormap (lambda (n)
                                                       (same-name? n (method-name method)))
                                                     (method-need-super m)))
                                            (cdr methods)))
                                (generate-member-key)
                                (method-name method))]
                           ;; Build the base mixin:
                           [(next-mixin) (apply
                                          (method-make-mixin method)
                                          name
                                          (append
                                           (method-need-inherit method)
                                           (method-need-super method)
                                           (method-need-inner method)
                                           (method-need-field method)))])
               (loop (cdr methods)
                     (lambda (%) (next-mixin (mixin %)))
                     (if (eq? name (method-name method))
                         super-indirections
                         (cons (list (method-name method)
                                     name
                                     method)
                               super-indirections))))]))))
  
  (define (introduce-into-mixin name mixin)
    (define-member-name m name)
    (lambda (%)
      (class (mixin %)
        (define/public (m) 'inroduce-stub)
        (super-new))))
  
  (define same-name? member-name-key=?)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  sum, exclude, alias
  
  (define (validate-trait who t)
    ;; Methods:
    (let ([ht (make-hasheq)])
      ;; Build up table and check for duplicates:
      (for-each (lambda (m)
                  (let* ([name (method-name m)]
                         [key (member-name-key-hash-code name)])
                    (let ([l (hash-ref ht key null)])
                      (when (ormap (lambda (n) (member-name-key=? (car n) name))
                                   l)
                        (raise-mismatch-error
                         who
                         "result would include multiple declarations of a method: " 
                         name))
                      (hash-set! ht key (cons (cons name m) l)))))
                (trait-methods t))
      ;; Check consistency of expectations and provisions:
      (let* ([find (lambda (name)
                     (let ([l (hash-ref ht (member-name-key-hash-code name) null)])
                       (ormap (lambda (n) 
                                (and (member-name-key=? (car n) name)
                                     (cdr n)))
                              l)))]
             [check (lambda (super? inner?)
                      (lambda (name)
                        (let ([m (find name)])
                          (when m
                            (when (and super?
                                       (not (method-override? m)))
                              (raise-mismatch-error
                               who
                               (string-append
                                "result would include both a super requirement and"
                                " a non-overriding declaration for method: ")
                               name))
                            
                            (when (and inner?
                                       (not (method-inner? m)))
                              (raise-mismatch-error
                               who
                               (string-append
                                "result would include both an inner requirement and"
                                " a non-augmentable declaration for method: ")
                               name))))))])
        (for-each (lambda (m)
                    (for-each (check #t #f)
                              (method-need-super m))
                    (for-each (check #f #t)
                              (method-need-inner m)))
                  (trait-methods t))))
    ;; Fields:
    (let ([ht (make-hasheq)])
      ;; Build up table and check for duplicates:
      (for-each (lambda (f)
                  (let* ([name (feeld-name f)]
                         [key (member-name-key-hash-code name)])
                    (let ([l (hash-ref ht key null)])
                      (when (ormap (lambda (n) (member-name-key=? (car n) name))
                                   l)
                        (raise-mismatch-error
                         who
                         "result would include multiple declarations of a field: " 
                         name))
                      (hash-set! ht key (cons (cons name f) l)))))
                (trait-fields t)))
    ;; Return validated trait:
    t)

  (define (trait-sum . ts)
    (for-each (lambda (t)
                (unless (trait? t)
                  (raise-argument-error 'trait-sum "trait?" t)))
              ts)
    (validate-trait
     'trait-sum
     (make-trait (apply
                  append
                  (map trait-methods ts))
                 (apply
                  append
                  (map trait-fields ts)))))
    
  (define (:trait-exclude t name)
    (unless (trait? t)
      (raise-argument-error 'trait-exclude "trait?" t))
    (let ([new-methods
           (filter (lambda (m)
                     (not (member-name-key=? (method-name m) name)))
                   (trait-methods t))])
      (when (= (length new-methods)
               (length (trait-methods t)))
        (raise-mismatch-error
         'trait-exclude
         "method not in trait: " name))
      (make-trait new-methods (trait-fields t))))

  (define (:trait-exclude-field t name)
    (unless (trait? t)
      (raise-argument-error 'trait-exclude-field "trait?" t))
    (let ([new-fields
           (filter (lambda (m)
                     (not (member-name-key=? (feeld-name m) name)))
                   (trait-fields t))])
      (when (= (length new-fields)
               (length (trait-fields t)))
        (raise-mismatch-error
         'trait-exclude
         "field not in trait: " name))
      (make-trait (trait-methods t) new-fields)))

  (define-syntax define-trait-exclude
    (syntax-rules ()
      [(_ trait-exclude :trait-exclude)
       (define-syntax (trait-exclude stx)
         (syntax-case stx ()
           [(_ t name)
            (begin
              (unless (identifier? #'name)
                (raise-syntax-error
                 #f
                 "expected an identifier for a method name"
                 stx
                 #'name))
              #'(:trait-exclude t (member-name-key name)))]))]))
  
  (define-trait-exclude trait-exclude :trait-exclude)
  (define-trait-exclude trait-exclude-field :trait-exclude-field)

  (define (:trait-alias t name new-name)
    (unless (trait? t)
      (raise-argument-error 'trait-alias "trait?" t))
    (let ([m (ormap (lambda (m)
                      (and (member-name-key=? (method-name m) name)
                           m))
                    (trait-methods t))])
      (unless m
        (raise-mismatch-error
         'trait-alias
         "method not in trait: " name))
      (validate-trait
       'trait-alias
       (make-trait
        (cons (struct-copy method m
                           [name new-name])
              (trait-methods t))
        (trait-fields t)))))

  (define (:trait-rename t name new-name)
    (unless (trait? t)
      (raise-argument-error 'trait-rename "trait?" t))
    (let ([rename (lambda (n)
                    (if (same-name? n name)
                        new-name
                        n))])
      (validate-trait
       'trait-rename
       (make-trait
        (map (lambda (m)
               (struct-copy method m
                            [name (rename (method-name m))]
                            [need-inherit (map rename (method-need-inherit m))]
                            [need-super (map rename (method-need-super m))]
                            [need-inner (map rename (method-need-inner m))]))
             (trait-methods t))
        (trait-fields t)))))

  (define (:trait-rename-field t name new-name)
    (unless (trait? t)
      (raise-argument-error 'trait-rename-field "trait?" t))
    (let ([rename (lambda (n)
                    (if (same-name? n name)
                        new-name
                        n))])
      (validate-trait
       'trait-rename
       (make-trait
        (map (lambda (m)
               (struct-copy method m
                            [need-field (map rename (method-need-field m))]))
             (trait-methods t))
        (map (lambda (f)
               (struct-copy feeld f
                            [name (rename (feeld-name f))]))
             (trait-fields t))))))

  (define-syntax define-trait-alias
    (syntax-rules ()
      [(_ trait-alias :trait-alias)
       (define-syntax (trait-alias stx)
         (syntax-case stx ()
           [(_ t name new-name)
            (begin
              (unless (identifier? #'name)
                (raise-syntax-error
                 #f
                 "expected an identifier for a method name"
                 stx
                 #'name))
              (unless (identifier? #'new-name)
                (raise-syntax-error
                 #f
                 "expected an identifier for a method name"
                 stx
                 #'new-name))
              #'(:trait-alias t (member-name-key name) (member-name-key new-name)))]))]))

  (define-trait-alias trait-alias :trait-alias)
  (define-trait-alias trait-rename :trait-rename)
  (define-trait-alias trait-rename-field :trait-rename-field)

  ;; ----------------------------------------;
  )
