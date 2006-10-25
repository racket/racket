(module trait mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "struct.ss"))
  (require-for-syntax (lib "list.ss")
                      (lib "stx.ss" "syntax")
                      (lib "boundmap.ss" "syntax")
                      (lib "kerncase.ss" "syntax")
                      ;; This should be part of a public expand-time API
                      ;;  exported by the class system:
                      (only (lib "classidmap.ss" "mzlib" "private") 
                            generate-class-expand-context))

  (provide (rename :trait trait)
           trait->mixin
           trait-sum trait-exclude trait-alias trait-rename)
  
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
  ;;   * a mixin patameterized by all external names
  ;;   * an indrection mixin for supers
  ;;   * an indrection mixin for inners
  
  (define-struct trait (methods))
  
  (define-struct method (name inherit? super? inner?
                              override? augment?
                              need-inherit need-super need-inner
                              make-mixin
                              make-super-indirection-mixin
                              make-inner-indirection-mixin))
  
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
                                  inherits inherits/super inherits/inner)
                         (extract expanded-clauses
                                  (map syntax->list
                                       (syntax->list
                                        #'((public public-final) 
                                           (pubment)
                                           (override override-final) 
                                           (augment augment-final) 
                                           (augride)
                                           (overment)
                                           (inherit) (inherit/super) (inherit/inner)))))])
             ;; Every declaration implies direct use for other declarations:
             (let ([to-inherit
                    (append publics pubments
                            overrides augments augrides overments
                            inherits inherits/super inherits/inner)])
               ;; Check distinct delcarations:
               (check-distinct-external-names to-inherit)
               (check-distinct-internal-names to-inherit)
               
               ;; Some declarations imply use via `super' or `inner':
               (let ([to-super (append overrides inherits/super)]
                     [to-inner (append augments pubments inherits/inner)])
                 
                 (let ([to-inherit-only
                        (filter (lambda (n)
                                  (not (or (ormap (lambda (n2) (bound-identifier=? n n2))
                                                  to-super)
                                           (ormap (lambda (n2) (bound-identifier=? n n2))
                                                  to-inner))))
                                to-inherit)])
                   
                   ;; Current method-making function with respect to the
                   ;;  common part:
                   (let* ([bindings (make-bindings expanded-clauses)]
                          [make-method (make-method-with-requirements
                                        bindings
                                        to-inherit-only to-super to-inner)])

                     ;; Build a mixin and `method' record for each declaration:
                     (with-syntax ([(method ...)
                                    (append
                                     (map (make-method #'override #t #f #f  #f #f  #f) publics)
                                     (map (make-method #'overment #t #t #f  #f #f  #f) pubments)
                                     (map (make-method #'override #t #t #f  #t #f  #f) overrides)
                                     (map (make-method #'overment #t #f #f  #t #f  #t) overments)
                                     (map (make-method #'augment  #t #f #t  #f #t  #f) augments)                                   
                                     (map (make-method #'augride  #t #f #f  #f #t  #f) augrides))])

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
                       #'(make-trait (list method ...)))))))))]))

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
                   this super inner))
              (kernel-form-identifier-list #'here))]
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
                      [(id . rest)
                       (ormap (lambda (x) (module-identifier=? x #'id))
                              (syntax->list
                               #'(public public-final pubment
                                         override override-final augment augment-final augride overment
                                         inherit inherit/super inherit/inner)))
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
             (if (module-identifier=? kw #'define-values)
                 (loop (cdr l) results)
                 (loop (cdr l)
                       (let iloop ([mapping keyword-mapping]
                                   [results results])
                         (if (ormap (lambda (x) (module-identifier=? kw x))
                                    (car mapping))
                             (cons (append (stx->list (stx-cdr (car l)))
                                           (car results))
                                   (cdr results))
                             (cons (car results)
                                   (iloop (cdr mapping)
                                          (cdr results))))))))])))
    
    (define (make-bindings expanded-clauses)
      (let ([boundmap (make-bound-identifier-mapping)])
        (for-each (lambda (clause)
                    (syntax-case clause (define-values)
                      [(define-values (id) rhs)
                       (bound-identifier-mapping-put! boundmap #'id #'rhs)]
                      [_else (void)]))
                  expanded-clauses)
        boundmap))
    
    (define (check-distinct-names method-decls
                                  what which
                                  make-identifier-mapping
                                  identifier-mapping-get
                                  identifier-mapping-set!)
      (let ([idmap (make-identifier-mapping)])
        (for-each (lambda (decl)
                    (let ([ext-id 
                           (if (identifier? decl)
                               decl
                               (which decl))])
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
                            "external" (lambda (x) (stx-car (stx-cdr x)))
                            make-module-identifier-mapping
                            module-identifier-mapping-get
                            module-identifier-mapping-put!))
    
    (define (check-distinct-internal-names method-decls)
      (check-distinct-names method-decls
                            "internal" stx-car
                            make-bound-identifier-mapping
                            bound-identifier-mapping-get
                            bound-identifier-mapping-put!))
    
    (define (((make-method-with-requirements binding-map
                                             to-inherit to-super to-inner)
              keyword inherit? super? inner?  override? augment? always-deep?)
             name)
      (let ([impl (bound-identifier-mapping-get binding-map name)]
            [to-inherit (if always-deep?
                            (filter (lambda (n) (not (bound-identifier=? n name)))
                                    to-inherit)
                            to-inherit)])
        (with-syntax ([(to-inherit ...) to-inherit]
                      [(to-super ...) to-super]
                      [(to-inner ...) to-inner]
                      [(to-inherit-arg ...) (generate-temporaries to-inherit)]
                      [(to-super-arg ...) (generate-temporaries to-super)]
                      [(to-inner-arg ...) (generate-temporaries to-inner)]
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
          (bound-identifier-mapping-put! binding-map name #f)
          ;; generate method:
          #`(make-method
             (member-name-key #,name)
             #,inherit? #,super? #,inner?  #,override? #,augment?
             (list (member-name-key to-inherit) ...)
             (list (member-name-key to-super) ...)
             (list (member-name-key to-inner) ...)
             (lambda (this-method-arg to-inherit-arg ...
                                      to-super-arg ...
                                      to-inner-arg ...)
               (define-member-name this-method this-method-arg)
               (define-member-name to-inherit to-inherit-arg) ...
               (define-member-name to-super to-super-arg) ...
               (define-member-name to-inner to-inner-arg) ...
               (lambda (%)
                 (class %
                   (inherit to-inherit ...)
                   (inherit/super to-super ...)
                   (inherit/inner to-inner ...)
                   (declare this-method)
                   (define this-method impl)
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
                    (super-new)))))
             ;; For `inner' call indirections:
             (wrap-inner-indirect
              (lambda (name-arg inner-name-arg)
                (define-member-name name name-arg)
                (define-member-name inner-name inner-name-arg)
                (lambda (%)
                  (class %
                    (augment name)
                    (inherit/inner inner-name)
                    (define name (similar-lambda impl (inner 'inner-indirect-call inner-name)))
                    (super-new)))))))))
    
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
      ;; Meanwhile, for inserting needed inner indirections below,
      ;;  we need to know which augmenting methods will be added.
      (let ([augments-to-add (map method-name
                                  (filter method-augment? methods))])
        
        ;; Order the mixins. If M1 super-calls M2 and we have an override
        ;;  for M2, then try to mix M2 later. Similarly, if M1 inner-calls M2
        ;;  and we have an augment for M2, try to mix M2 earlier.
        ;; We'll have to break cycles by inserting indirections.
        ;; For simplicty, we sort right now by just all augments first
        ;;  and all overrides last. In the common case where methods
        ;;  only self-call supers and inners, that will work fine.
        (let loop ([methods (sort methods
                                  (lambda (a b)
                                    (or (method-augment? a)
                                        (method-override? b))))]
                   ;; Start by mixing a dummy method for each public/pubment
                   ;; method. We'll override it, but having it here at the start
                   ;; means that the methods can refer to each other via
                   ;; `inherit'.
                   [mixin (let loop ([methods methods]
                                     [mixin (lambda (%) %)])
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
                   [super-indirections null]
                   [inner-indirections null]
                   [done-augments null])
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
                                 (define-member-name name (cadar indirections))
                                 (lambda (%) (mix (mixin %))))))]))]
            [else
             ;; Add one method:
             (let*-values ([(method) (car methods)]
                           ;; About to complete an augment?
                           [(done-augments)
                            (if (method-augment? method)
                                (cons (method-name method)
                                      done-augments)
                                done-augments)]
                           ;; Remove inner indirection, in case we're adding
                           ;;  the augment, now:
                           [(inner-indirections insert-indirection)
                            (remove-inner-indirection (method-name method)
                                                      inner-indirections)]
                           ;; Add any newly needed indirections:
                           [(new-inner-indirections)
                            (add-inner-indirection (method-need-inner method)
                                                   augments-to-add
                                                   done-augments
                                                   inner-indirections)]
                           [(inner-indirections) (append new-inner-indirections
                                                         inner-indirections)]
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
                           ;; Prepare the mixin with any needed new indirections
                           [(mixin)
                            (let loop ([l new-inner-indirections]
                                       [mixin mixin])
                              (cond
                                [(null? l) 
                                 (if (eq? (method-name method) name)
                                     mixin
                                     (introduce-into-mixin name mixin))]
                                [else (loop
                                       (cdr l)
                                       (let ()
                                         (define-member-name m (cadar l))
                                         (lambda (%)
                                           (class (mixin %)
                                             (pubment m)
                                             (define (m) 'inner-indirection)
                                             (super-new)))))]))]
                           ;; Build the base mixin:
                           [(core-mixin) (apply
                                          (method-make-mixin method)
                                          name
                                          (append
                                           (method-need-inherit method)
                                           (method-need-super method)
                                           (apply-renames (method-need-inner method)
                                                          inner-indirections)))]
                           ;; Complete the mixin with an inner indirection,
                           ;;  if needed:
                           [(next-mixin) (if insert-indirection
                                             (let ([mix ((method-make-inner-indirection-mixin method)
                                                         name
                                                         insert-indirection)])
                                               (lambda (%)
                                                 (mix (core-mixin %))))
                                             core-mixin)])
               (loop (cdr methods)
                     (lambda (%) (next-mixin (mixin %)))
                     (if (eq? name (method-name method))
                         super-indirections
                         (cons (list (method-name method)
                                     name
                                     method)
                               super-indirections))
                     inner-indirections
                     done-augments))])))))
  
  (define (introduce-into-mixin name mixin)
    (define-member-name m name)
    (lambda (%)
      (class (mixin %)
        (define/public (m) 'inroduce-stub)
        (super-new))))

  
  (define (remove-inner-indirection name inner-indirections)
    (cond
      [(null? inner-indirections)
       (values inner-indirections #f)]
      [(same-name? name (caar inner-indirections))
       (values (cdr inner-indirections) (cadar inner-indirections))]
      [else
       (let-values ([(new-inner-indirections indirect)
                     (remove-inner-indirection name inner-indirections)])
         (if indirect
             (values (cons (car inner-indirections)
                           new-inner-indirections)
                     indirect)
             (values inner-indirections #f)))]))
  
  (define (add-inner-indirection need-inners augments-to-add done-augments inner-indirections)
    (apply append
           (map (lambda (m)
                  (if (and (ormap (lambda (n) 
                                    (same-name? m n))
                                  augments-to-add)
                           (not (ormap (lambda (n)
                                         (same-name? m n))
                                       done-augments))
                           (not (ormap (lambda (i)
                                         (same-name? m (car i)))
                                       inner-indirections)))
                      (list (list m (generate-member-key)))
                      null))
                need-inners)))
  
  (define (apply-renames names indirections)
    (map (lambda (n)
           (or (ormap (lambda (i)
                        (and (same-name? (car i) n)
                             (cadr i)))
                      indirections)
               n))
         names))
  
  (define same-name? member-name-key=?)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  sum, exclude, alias
  
  (define (validate-trait who t)
    (let ([ht (make-hash-table)])
      ;; Build up table and check for duplicates:
      (for-each (lambda (m)
                  (let* ([name (method-name m)]
                         [key (member-name-key-hash-code name)])
                    (let ([l (hash-table-get ht key null)])
                      (when (ormap (lambda (n) (member-name-key=? (car n) name))
                                   l)
                        (raise-mismatch-error
                         who
                         "result would include two declarations of a method: " 
                         name))
                      (hash-table-put! ht key (cons (cons name m) l)))))
                (trait-methods t))
      ;; Check consistency of expectations and provisions:
      (let* ([find (lambda (name)
                     (let ([l (hash-table-get ht (member-name-key-hash-code name) null)])
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
    t)

  (define (trait-sum . ts)
    (for-each (lambda (t)
                (unless (trait? t)
                  (raise-type-error 'trait-sum "trait" t)))
              ts)
    (validate-trait
     'trait-sum
     (make-trait (apply
                  append
                  (map trait-methods ts)))))
    
  (define (:trait-exclude t name)
    (unless (trait? t)
      (raise-type-error 'trait-exclude "trait" t))
    (let ([new-methods
           (filter (lambda (m)
                     (not (member-name-key=? (method-name m) name)))
                   (trait-methods t))])
      (when (= (length new-methods)
               (length (trait-methods t)))
        (raise-mismatch-error
         'trait-exclude
         "method not in trait: " name))
      (make-trait new-methods)))

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
         #'(:trait-exclude t (member-name-key name)))]))

  (define (:trait-alias t name new-name)
    (unless (trait? t)
      (raise-type-error 'trait-alias "trait" t))
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
        (cons (copy-struct method m
                           [method-name new-name])
              (trait-methods t))))))

  (define (:trait-rename t name new-name)
    (unless (trait? t)
      (raise-type-error 'trait-alias "trait" t))
    (let ([rename (lambda (n)
                    (if (same-name? n name)
                        new-name
                        n))])
      (validate-trait
       'trait-rename
       (make-trait
        (map (lambda (m)
               (copy-struct method m
                            [method-name (rename (method-name m))]
                            [method-need-inherit (map rename (method-need-inherit m))]
                            [method-need-super (map rename (method-need-super m))]
                            [method-need-inner (map rename (method-need-inner m))]))
             (trait-methods t))))))

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

  ;; ----------------------------------------;
  )
