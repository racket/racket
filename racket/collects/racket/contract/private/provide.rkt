#lang racket/base

(provide provide/contract
         provide/contract-for-contract-out
         define-module-boundary-contract
         (protect-out (for-syntax true-provide/contract
                                  ;make-provide/contract-transformer
                                  provide/contract-info?
                                  provide/contract-info-contract-id
                                  provide/contract-info-original-id
                                  contract-rename-id-property
                                  contract-lifted-property
                                  contract-neg-party-property)))

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info
                     setup/path-to-relative
                     "application-arity-checking.rkt"
                     "arr-i-parse.rkt"
                     (prefix-in a: "helpers.rkt")
                     (rename-in syntax/private/boundmap
                                ;; the private version of the library
                                ;; (the one without contracts)
                                ;; has these old, wrong names in it.
                                [make-module-identifier-mapping make-free-identifier-mapping]
                                [module-identifier-mapping-get free-identifier-mapping-get]
                                [module-identifier-mapping-put! free-identifier-mapping-put!]))
         "arrow.rkt"
         "arrow-val-first.rkt"
         "base.rkt"
         "guts.rkt"
         "misc.rkt"
         "exists.rkt"
         "opt.rkt"
         "prop.rkt"
         "blame.rkt"
         syntax/location
         syntax/srcloc)

(define-for-syntax (self-ctor-transformer orig stx)
  (with-syntax ([orig orig])
    (syntax-case stx ()
      [(_ arg ...) (datum->syntax stx
                                  (syntax-e (syntax (orig arg ...)))
                                  stx
                                  stx)]
      [_ (syntax orig)])))

(define-for-syntax make-applicable-struct-info
  (letrec-values ([(struct: make- ? ref set!)
                   (make-struct-type 'self-ctor-struct-info struct:struct-info
                                     1 0 #f
                                     (list (cons prop:procedure
                                                 (lambda (v stx)
                                                   (self-ctor-transformer ((ref v 0)) stx))))
                                     (current-inspector) #f '(0))])
    make-))

(begin-for-syntax

  ;; rename-id : identifier? : the name the lifted expression is bound to
  ;; contract-id : identifier? : the name of the contract expression
  ;; original-id : identifier? : the identifier being contracted
  (struct provide/contract-info (rename-id contract-id original-id))

  ;; keys for syntax property used below
  (define rename-id-key (gensym 'contract:rename-id))
  (define lifted-key    (gensym 'contract:lifted))
  (define neg-party-key (gensym 'contract:neg-party))

  ;; identifier? identifier? -> identifier?
  ;; add a property that tells clients what the exported id was
  (define (add-rename-id rename-id partial-id)
    (syntax-property partial-id rename-id-key rename-id))

  ;; syntax? -> syntax?
  ;; tells clients that the expression is a lifted application
  (define (add-lifted-property stx)
    (syntax-property stx lifted-key #t))

  ;; identifier? -> identifier?
  ;; tells clients that the application of this id has an extra inserted argument
  (define (add-neg-party stx)
    (syntax-property stx neg-party-key #t))

  ;; getter functions for syntax properties keyed by symbols above
  (define (contract-rename-id-property stx)
    (syntax-property stx rename-id-key))
  (define (contract-lifted-property stx)
    (syntax-property stx lifted-key))
  (define (contract-neg-party-property stx)
    (syntax-property stx neg-party-key))

  (define global-saved-id-table (make-hasheq))

  (struct provide/contract-arrow-transformer provide/contract-info
    (saved-ho-id-table
     partially-applied-id
     extra-neg-party-argument-fn
     valid-argument-lists)
    #:property
    prop:set!-transformer
    (λ (self stx)
      (let ([partially-applied-id (provide/contract-arrow-transformer-partially-applied-id self)]
            [saved-ho-id-table (provide/contract-arrow-transformer-saved-ho-id-table self)]
            [extra-neg-party-argument-fn 
             (provide/contract-arrow-transformer-extra-neg-party-argument-fn self)]
            [valid-arg-lists (provide/contract-arrow-transformer-valid-argument-lists self)]
            [rename-id (provide/contract-info-rename-id self)])
        (with-syntax ([partially-applied-id partially-applied-id]
                      [extra-neg-party-argument-fn extra-neg-party-argument-fn])
          (if (eq? 'expression (syntax-local-context))
              ;; In an expression context:
              (let* ([key (syntax-local-lift-context)]
                     ;; Already lifted in this lifting context?
                     [lifted-neg-party
                      (or (hash-ref global-saved-id-table key #f)
                          ;; No: lift the neg name creation
                          (syntax-local-introduce 
                           (syntax-local-lift-expression
                            #'(quote-module-name))))])
                (when key (hash-set! global-saved-id-table key lifted-neg-party))
                ;; Expand to a use of the lifted expression:
                (define (adjust-location new-stx)
                  (datum->syntax new-stx (syntax-e new-stx) stx new-stx))
                (define (gen-slow-path-code)
                  (define lifted-ctc-val
                    (or (hash-ref saved-ho-id-table key #f)
                        ;; No: lift the neg name creation
                        (with-syntax ([lifted-neg-party (syntax-local-introduce lifted-neg-party)])
                          (syntax-local-introduce 
                           (add-rename-id rename-id
                            (syntax-local-lift-expression
                             (add-lifted-property
                              #'(partially-applied-id lifted-neg-party))))))))
                  (when key (hash-set! saved-ho-id-table key lifted-ctc-val))
                  (adjust-location (syntax-local-introduce lifted-ctc-val)))
                (syntax-case stx (set!)
                  [name 
                   (identifier? #'name)
                   (gen-slow-path-code)]
                  [(set! id arg)
                   (raise-syntax-error
                    'contract/out
                    "cannot set! a contract/out variable"
                    stx #'id)]
                  [(name more ...)
                   (with-syntax ([app (datum->syntax stx '#%app)])
                     (if (valid-argument-list? stx valid-arg-lists)
                         (with-syntax ([lifted-neg-party (syntax-local-introduce lifted-neg-party)])
                           (adjust-location
                            #`(app #,(add-neg-party (add-rename-id
                                                     rename-id
                                                     #'extra-neg-party-argument-fn))
                                   lifted-neg-party
                                   more ...)))
                         #`(app #,(gen-slow-path-code) more ...)))]))
              ;; In case of partial expansion for module-level and internal-defn
              ;; contexts, delay expansion until it's a good time to lift
              ;; expressions:
              (quasisyntax/loc stx (#%expression #,stx)))))))
  
  (struct provide/contract-transformer provide/contract-info (saved-id-table partially-applied-id)
    #:property
    prop:set!-transformer
    (λ (self stx)
      (let ([partially-applied-id (provide/contract-transformer-partially-applied-id self)]
            [saved-id-table (provide/contract-transformer-saved-id-table self)]
            [rename-id (provide/contract-info-rename-id self)])
        (with-syntax ([partially-applied-id partially-applied-id])
          (if (eq? 'expression (syntax-local-context))
              ;; In an expression context:
              (let* ([key (syntax-local-lift-context)]
                     ;; Already lifted in this lifting context?
                     [lifted-ctcd-val
                      (or (hash-ref saved-id-table key #f)
                          ;; No: lift the neg name creation
                          (add-rename-id rename-id
                           (syntax-local-introduce
                            (syntax-local-lift-expression
                             (add-lifted-property
                              #'(partially-applied-id (quote-module-name)))))))])
                (when key (hash-set! saved-id-table key lifted-ctcd-val))
                (define (adjust-location new-stx)
                  (datum->syntax new-stx (syntax-e new-stx) stx new-stx))
                ;; Expand to a use of the lifted expression:
                (with-syntax ([lifted-ctcd-val (syntax-local-introduce lifted-ctcd-val)])
                  (syntax-case stx (set!)
                    [name 
                     (identifier? #'name)
                     (adjust-location #'lifted-ctcd-val)]
                    [(set! id arg)
                     (raise-syntax-error
                      'contract/out
                      "cannot set! a contract/out variable"
                      stx #'id)]
                    [(name more ...)
                     (with-syntax ([app (datum->syntax stx '#%app)])
                       (adjust-location 
                        #'(app lifted-ctcd-val more ...)))])))
              ;; In case of partial expansion for module-level and internal-defn
              ;; contexts, delay expansion until it's a good time to lift
              ;; expressions:
              (quasisyntax/loc stx (#%expression #,stx)))))))

  (define (make-provide/contract-transformer rename-id cid id eid pos [pid #f])
    (if pid
        (provide/contract-transformer rename-id cid id (make-hasheq) pid)
        (begin
          ;; TODO: this needs to change!
          ;; syntax/parse uses this
          ;; this will just drop contracts for now.
          (λ (stx) 
            (syntax-case stx ()
              [(_ args ...)
               (with-syntax ([app (datum->syntax stx '#%app)])
                 #`(app #,id args ...))]
              [x (identifier? #'x) id])))))
  
  (define (make-provide/contract-arrow-transformer rename-id contract-id id pai enpfn val)
    (provide/contract-arrow-transformer rename-id
                                        contract-id id
                                        (make-hasheq)
                                        pai enpfn val)))


;; tl-code-for-one-id/new-name : syntax syntax syntax (union syntax #f) -> (values syntax syntax)
;; given the syntax for an identifier and a contract,
;; builds a begin expression for the entire contract and provide
;; the first syntax object is used for source locations
(define-for-syntax (tl-code-for-one-id/new-name id-for-one-id
                                                stx id reflect-id ctrct/no-prop user-rename-id
                                                pos-module-source
                                                mangle-for-maker?
                                                provide?)
  (define ex-id (or reflect-id id))
  (define id-rename (id-for-one-id user-rename-id reflect-id id mangle-for-maker?))
  (with-syntax ([ctrct (syntax-property 
                        (syntax-property
                         ctrct/no-prop
                         'racket/contract:contract-on-boundary
                         (gensym 'provide/contract-boundary))
                        'inferred-name ex-id)]
                [external-name (or user-rename-id id)])
    (define srcloc-id 
      (if (syntax-source id)
          id
          (if (and user-rename-id
                   (syntax-source user-rename-id))
              user-rename-id
              #'ex-id)))
    (with-syntax ([code
                   (syntax-property
                    (quasisyntax/loc stx
                      (begin #,(internal-function-to-be-figured-out #'ctrct
                                                                    id
                                                                    (or reflect-id id)
                                                                    (or user-rename-id 
                                                                        id)
                                                                    id-rename
                                                                    (stx->srcloc-expr srcloc-id)
                                                                    'provide/contract
                                                                    pos-module-source)
                             #,@(if provide?
                                    (list #`(provide (rename-out [#,id-rename external-name])))
                                    null)))
                    'provide/contract-original-contract
                    (vector #'external-name #'ctrct))])
      #`(code #,id-rename))))

;; syntax -> syntax
;; returns an expression that evaluates to the source location of the argument
(define-for-syntax (stx->srcloc-expr srcloc-stx)
  #`(vector
     '#,(syntax-source srcloc-stx)
     #,(syntax-line srcloc-stx)
     #,(syntax-column srcloc-stx)
     #,(syntax-position srcloc-stx)
     #,(syntax-span srcloc-stx)))

(define-for-syntax (internal-function-to-be-figured-out ctrct
                                                        id 
                                                        ex-id
                                                        name-for-blame
                                                        id-rename
                                                        srcloc-expr
                                                        contract-error-name
                                                        pos-module-source)
  (define-values (arrow? the-valid-app-shapes)
    (syntax-case ctrct (->2 ->*2 ->i)
      [(->2 . _) 
       (->2-handled? ctrct)
       (values #t (->-valid-app-shapes ctrct))]
      [(->*2 . _) 
       (values (->*2-handled? ctrct)
               (->*-valid-app-shapes ctrct))]
      [(->i . _) (values #t (->i-valid-app-shapes ctrct))]
      [_ (values #f #f)]))
  (with-syntax ([id id]
                [(partially-applied-id extra-neg-party-argument-fn contract-id) 
                 (generate-temporaries (list 'idX 'idY 'idZ))]
                [ctrct ctrct])
    (syntax-local-lift-module-end-declaration
     #`(begin 
         (define partially-applied-id
           (do-partial-app contract-id
                           id
                           '#,name-for-blame
                           #,pos-module-source
                           #,srcloc-expr))
         #,@(if arrow?
                (list #`(define extra-neg-party-argument-fn 
                          (wrapped-extra-arg-arrow-extra-neg-party-argument
                           partially-applied-id)))
                (list))))

    #`(begin
        (define contract-id
          ;; let is here to give the right name.
          (let ([#,ex-id (coerce-contract '#,contract-error-name ctrct)
                         #;(opt/c ctrct #:error-name #,contract-error-name)])
            #,ex-id))
        
        (define-syntax #,id-rename
          #,(if arrow?
                #`(make-provide/contract-arrow-transformer 
                   (quote-syntax #,id-rename)
                   (quote-syntax contract-id) (quote-syntax id)
                   (quote-syntax partially-applied-id)
                   (quote-syntax extra-neg-party-argument-fn)
                   #,the-valid-app-shapes)
                #`(make-provide/contract-transformer
                   (quote-syntax #,id-rename)
                   (quote-syntax contract-id) (quote-syntax id)
                   #f #f
                   (quote-syntax partially-applied-id)))))))

(define-syntax (define-module-boundary-contract stx)
  (cond
    [(equal? (syntax-local-context) 'module-begin)
     #`(begin #,stx)]
    [else
     (syntax-case stx ()
       [(_ new-id orig-id ctrct kwd-args ...)
        (begin
          (unless (equal? (syntax-local-context) 'module)
            (raise-syntax-error #f "only valid in a top-level module context" stx))
          (unless (identifier? #'new-id)
            (raise-syntax-error #f "expected an identifier" stx #'new-id))
          (unless (identifier? #'orig-id)
            (raise-syntax-error #f "expected an identifier" stx #'orig-id))
          (define-values (pos-blame-party-expr srcloc-expr)
            (let loop ([kwd-args (syntax->list #'(kwd-args ...))]
                       [pos-blame-party-expr #'(quote-module-path)]
                       [srcloc-expr #f])
              (cond
                [(null? kwd-args) (values pos-blame-party-expr
                                          (or srcloc-expr (stx->srcloc-expr stx)))]
                [else
                 (define kwd (car kwd-args))
                 (cond 
                   [(equal? (syntax-e kwd) '#:pos-source)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:pos-source"
                                          stx))
                    (loop (cddr kwd-args)
                          (cadr kwd-args)
                          srcloc-expr)]
                   [(equal? (syntax-e kwd) '#:srcloc)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:srcloc"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          (cadr kwd-args))]
                   [else
                    (raise-syntax-error #f "expected either the keyword #:pos-source of #:srcloc"
                                        stx
                                        (car kwd-args))])])))
          (internal-function-to-be-figured-out #'ctrct
                                               #'orig-id
                                               #'orig-id
                                               #'new-id
                                               #'new-id
                                               srcloc-expr
                                               'define-module-boundary-contract
                                               pos-blame-party-expr))])]))

;; ... -> (or/c #f (-> blame val))
(define (do-partial-app ctc val name pos-module-source source)
  (define p (contract-struct-val-first-projection ctc))
  (define blme (make-blame (build-source-location source)
                           name
                           (λ () (contract-name ctc))
                           pos-module-source
                           #f #t))
  
  (cond
    [p
     (define neg-accepter ((p blme) val))
     
     ;; we don't have the negative blame here, but we
     ;; expect only positive failures from this; do the
     ;; check and then toss the results.
     (neg-accepter 'incomplete-blame-from-provide.rkt)
     
     neg-accepter]
    [else
     (define proj (contract-struct-projection ctc))
     
     ;; we don't have the negative blame here, but we
     ;; expect only positive failures from this; do the
     ;; check and then toss the results.
     ((proj blme) val)
     
     (procedure-rename
      (λ (neg-party)
        (define complete-blame (blame-add-missing-party blme neg-party))
        ((proj complete-blame) val))
      (string->symbol (format "provide.rkt:neg-party-fn:~s" (contract-name ctc))))]))

(define-for-syntax (true-provide/contract provide-stx just-check-errors? who)
  (syntax-case provide-stx ()
    [(_ p/c-ele ...)
     (let ()

       ;; ids : table[id -o> (listof id)]
       ;; code-for-each-clause adds identifiers to this map.
       ;; when it binds things; they are then used to signal
       ;; a syntax error for duplicates
       (define dups-table (make-hash))
       (define (add-to-dups-table id)
         (hash-update!
          dups-table
          (syntax-e id)
          (λ (ids) (cons id ids))
          '()))
       (define (signal-dup-syntax-error)
         (hash-for-each
          dups-table
          (λ (k ids)
            (let loop ([ids ids])
              (cond
                [(null? ids) (void)]
                [else
                 (cond
                   [(ormap (λ (x) (bound-identifier=? (car ids) x)) (cdr ids))
                    (let ([dups (filter (λ (x) (bound-identifier=? (car ids) x))
                                        ids)])
                      (raise-syntax-error who
                                          "duplicate identifiers"
                                          provide-stx
                                          (car dups)
                                          (cdr dups)))]
                   [else
                    (loop (cdr ids))])])))))

       ;; code-for-each-clause : (listof syntax) -> (listof syntax)
       ;; constructs code for each clause of a provide/contract
       (define (code-for-each-clause clauses)
         (let loop ([clauses clauses]
                    [exists-binders '()])
           (cond
           [(null? clauses) null]
           [else
            (let ([clause (car clauses)])
              ;; compare raw identifiers for `struct' and `rename' just like provide does
              (syntax-case* clause (struct rename) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
                [exists
                 (or (eq? '#:exists (syntax-e #'exists)) (eq? '#:∃ (syntax-e #'exists))
                     (eq? '#:forall (syntax-e #'exists)) (eq? '#:∀ (syntax-e #'exists)))
                 (cond
                   [(null? (cdr clauses))
                    (raise-syntax-error 
                     who
                     (format (string-append
                              "expected either a single variable or a sequence of variables"
                              " to follow ~a, but found nothing")
                             (syntax-e #'exists))
                     provide-stx
                     clause)]
                   [else
                    (syntax-case (cadr clauses) ()
                      [x
                       (identifier? #'x)
                       (if just-check-errors?
                           (loop (cddr clauses) exists-binders)
                           (with-syntax ([(x-gen) (generate-temporaries #'(x))])
                             (cons (code-for-one-poly-id #'x #'x-gen #'exists)
                                   (loop (cddr clauses)
                                         (add-a-binder #'x #'x-gen exists-binders)))))]
                      [(x ...)
                       (andmap identifier? (syntax->list #'(x ...)))
                       (if just-check-errors?
                           (loop (cddr clauses) exists-binders)
                           (with-syntax ([(x-gen ...) (generate-temporaries #'(x ...))])
                             (append (map (λ (x x-gen) (code-for-one-poly-id x x-gen #'exists))
                                          (syntax->list #'(x ...))
                                          (syntax->list #'(x-gen ...)))
                                     (loop (cddr clauses)
                                           (let loop ([binders exists-binders]
                                                      [xs (syntax->list #'(x ...))]
                                                      [x-gens (syntax->list #'(x-gen ...))])
                                             (cond
                                               [(null? xs) binders]
                                               [else 
                                                (loop (add-a-binder (car xs) (car x-gens) binders)
                                                      (cdr xs)
                                                      (cdr x-gens))]))))))]
                      [else
                       (raise-syntax-error 
                        who
                        (format (string-append "expected either a single variable or a sequence"
                                               " of variables to follow ~a")
                                (syntax-e #'exists))
                        provide-stx
                        (cadr clauses))])])]
                [(rename this-name new-name contract)
                 (and (identifier? (syntax this-name))
                      (identifier? (syntax new-name)))
                 (begin
                   (add-to-dups-table #'new-name)
                   (if just-check-errors?
                       (loop (cdr clauses) exists-binders)
                       (cons (code-for-one-id provide-stx
                                              (syntax this-name) #f
                                              (add-exists-binders (syntax contract) exists-binders)
                                              (syntax new-name))
                             (loop (cdr clauses) exists-binders))))]
                [(rename this-name new-name contract)
                 (identifier? (syntax this-name))
                 (raise-syntax-error who
                                     "malformed rename clause, expected an identifier"
                                     provide-stx
                                     (syntax new-name))]
                [(rename this-name new-name contract)
                 (identifier? (syntax new-name))
                 (raise-syntax-error who
                                     "malformed rename clause, expected an identifier"
                                     provide-stx
                                     (syntax this-name))]
                [(rename . _)
                 (raise-syntax-error who "malformed rename clause" provide-stx clause)]
                [(struct struct-name ((field-name contract) ...) options ...)
                 (and (well-formed-struct-name? (syntax struct-name))
                      (andmap identifier? (syntax->list (syntax (field-name ...)))))
                 (let ()
                   (for ([option (in-list (syntax->list #'(options ...)))])
                     (unless (member (syntax-e option) '(#:omit-constructor))
                       (raise-syntax-error who
                                           "malformed struct option" 
                                           provide-stx
                                           option)))
                   (add-to-dups-table #'struct-name)
                   (define omit-constructor? 
                     (member '#:omit-constructor (map syntax-e (syntax->list #'(options ...)))))
                   (if just-check-errors?
                       (loop (cdr clauses) exists-binders)
                       (let ([sc (build-struct-code provide-stx
                                                    (syntax struct-name)
                                                    (syntax->list (syntax (field-name ...)))
                                                    (map (λ (x) (add-exists-binders x exists-binders))
                                                         (syntax->list (syntax (contract ...))))
                                                    omit-constructor?)])
                         (cons sc (loop (cdr clauses) exists-binders)))))]
                [(struct name)
                 (identifier? (syntax name))
                 (raise-syntax-error who
                                     "missing fields"
                                     provide-stx
                                     clause)]
                [(struct name . rest)
                 (not (well-formed-struct-name? (syntax name)))
                 (raise-syntax-error
                  who
                  "name must be an identifier or two identifiers with parens around them"
                  provide-stx
                  (syntax name))]
                [(struct name (fields ...) options ...)
                 (let ()
                   (for ([field [in-list (syntax->list (syntax (fields ...)))]])
                     (syntax-case field ()
                       [(x y)
                        (identifier? (syntax x))
                        (void)]
                       [(x y)
                        (raise-syntax-error who
                                            "malformed struct field, expected identifier"
                                            provide-stx
                                            (syntax x))]
                       [else
                        (raise-syntax-error who
                                            "malformed struct field"
                                            provide-stx
                                            field)]))
                   ;; if we didn't find a bad field something is wrong!
                   (raise-syntax-error who "internal error.1" provide-stx clause))]
                [(struct name . fields)
                 (raise-syntax-error who
                                     "malformed struct fields"
                                     provide-stx
                                     clause)]
                [(name contract)
                 (identifier? (syntax name))
                 (begin
                   (add-to-dups-table #'name)
                   (if just-check-errors?
                       (loop (cdr clauses) exists-binders)
                       (cons (code-for-one-id provide-stx
                                              (syntax name) #f
                                              (add-exists-binders (syntax contract)
                                                                  exists-binders)
                                              #f)
                             (loop (cdr clauses) exists-binders))))]
                [(name contract)
                 (raise-syntax-error who
                                     "expected identifier"
                                     provide-stx
                                     (syntax name))]
                [unk
                 (raise-syntax-error who
                                     "malformed clause"
                                     provide-stx
                                     (syntax unk))]))])))

       ;; well-formed-struct-name? : syntax -> bool
       (define (well-formed-struct-name? stx)
         (or (identifier? stx)
             (syntax-case stx ()
               [(name super)
                (and (identifier? (syntax name))
                     (identifier? (syntax super)))
                #t]
               [else #f])))

       ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
       ;; constructs the code for a struct clause
       ;; first arg is the original syntax object, for source locations
       (define (build-struct-code stx struct-name-position field-names field-contracts 
                                  omit-constructor?)
         (let* ([struct-name (syntax-case struct-name-position ()
                               [(a b) (syntax a)]
                               [else struct-name-position])]
                [super-id (syntax-case struct-name-position ()
                            [(a b) (syntax b)]
                            [else #t])]


                [all-parent-struct-count/names 
                 (get-field-counts/struct-names struct-name provide-stx)]
                [parent-struct-count (if (null? all-parent-struct-count/names)
                                         #f
                                         (let ([pp (cdr all-parent-struct-count/names)])
                                           (if (null? pp)
                                               #f
                                               (car (car pp)))))]

                [the-struct-info (a:lookup-struct-info struct-name-position provide-stx)]
                [constructor-id (list-ref the-struct-info 1)]
                [predicate-id (list-ref the-struct-info 2)]
                [selector-ids (reverse (list-ref the-struct-info 3))]
                [type-is-only-constructor? (free-identifier=? constructor-id struct-name)]
                ; I think there's no way to detect when the struct-name binding isn't a constructor
                [type-is-constructor? #t] 
                [chaperone-constructor-id 
                 (and constructor-id (car (generate-temporaries (list constructor-id))))]
                [is-id-ok?
                 (λ (id i)
                   (if (or (not parent-struct-count)
                           (parent-struct-count . <= . i))
                       id
                       #t))]
                [mutator-ids (reverse (list-ref the-struct-info 4))] ;; (listof (union #f identifier))
                [field-contract-ids (map (λ (field-name field-contract)
                                           (a:mangle-id "provide/contract-field-contract"
                                                        field-name
                                                        struct-name))
                                         field-names
                                         field-contracts)]
                [struct:struct-name
                 (or (list-ref the-struct-info 0)
                     (datum->syntax
                      struct-name
                      (string->symbol
                       (string-append
                        "struct:"
                        (symbol->string (syntax-e struct-name))))))]

                [-struct:struct-name
                 (datum->syntax
                  struct-name
                  (string->symbol
                   (string-append
                    "-struct:"
                    (symbol->string (syntax-e struct-name)))))]

                [is-new-id?
                 (λ (index)
                   (or (not parent-struct-count)
                       (parent-struct-count . <= . index)))])

           (let ([unknown-info
                  (λ (what names)
                    (raise-syntax-error
                     who
                     (format "cannot determine ~a, found ~s" what names)
                     provide-stx
                     struct-name))])

             (unless (or (null? selector-ids)
                         (identifier? (last selector-ids)))
               (unknown-info "the selectors" (map syntax->datum selector-ids)))

             (unless constructor-id (unknown-info "constructor" constructor-id))
             (unless predicate-id (unknown-info "predicate" predicate-id))
             (unless (andmap/count is-id-ok? selector-ids)
               (unknown-info "selectors"
                             (map (λ (x) (if (syntax? x)
                                             (syntax->datum x)
                                             x))
                                  selector-ids))))

           (unless (equal? (length selector-ids)
                           (length field-contract-ids))
             (raise-syntax-error who
                                 (format "found ~a field~a in struct, but ~a contract~a"
                                         (length selector-ids)
                                         (if (= 1 (length selector-ids)) "" "s")
                                         (length field-contract-ids)
                                         (if (= 1 (length field-contract-ids)) "" "s"))
                                 provide-stx
                                 struct-name))

           ;; make sure the field names are right.
           (let* ([relative-counts (let loop ([c (map car all-parent-struct-count/names)])
                                     (cond
                                       [(null? c) null]
                                       [(null? (cdr c)) c]
                                       [else (cons (- (car c) (cadr c))
                                                   (loop (cdr c)))]))]
                  [names (map cdr all-parent-struct-count/names)]
                  [predicate-name (format "~a" (syntax-e predicate-id))])
             (let loop ([count (car relative-counts)]
                        [name (car names)]
                        [counts (cdr relative-counts)]
                        [names (cdr names)]
                        [selector-strs (reverse (map (λ (x) (format "~a" (syntax-e x))) 
                                                     selector-ids))]
                        [field-names (reverse field-names)])
               (cond
                 [(or (null? selector-strs) (null? field-names))
                  (void)]
                 [(zero? count)
                  (loop (car counts) (car names) (cdr counts) (cdr names)
                        selector-strs
                        field-names)]
                 [else
                  (let* ([selector-str (car selector-strs)]
                         [field-name (car field-names)]
                         [field-name-should-be
                          (substring selector-str
                                     (+ (string-length name) 1)
                                     (string-length selector-str))]
                         [field-name-is (format "~a" (syntax-e field-name))])
                    (unless (equal? field-name-should-be field-name-is)
                      (raise-syntax-error who
                                          (format "expected field name to be ~a, but found ~a"
                                                  field-name-should-be
                                                  field-name-is)
                                          provide-stx
                                          field-name))
                    (loop (- count 1)
                          name
                          counts
                          names
                          (cdr selector-strs)
                          (cdr field-names)))])))
           (with-syntax ([((selector-codes selector-new-names) ...)
                          (filter
                           (λ (x) x)
                           (map/count (λ (selector-id field-contract-id index)
                                        (if (is-new-id? index)
                                            (code-for-one-id/new-name
                                             stx
                                             selector-id #f
                                             (build-selector-contract struct-name
                                                                      predicate-id
                                                                      field-contract-id)
                                             #f)
                                            #f))
                                      selector-ids
                                      field-contract-ids))]
                         [(rev-selector-old-names ...)
                          (reverse
                           (filter
                            (λ (x) x)
                            (for/list ([selector-id (in-list selector-ids)]
                                       [index (in-naturals)])
                              (if (is-new-id? index)
                                  #f
                                  (let ([in-map (free-identifier-mapping-get struct-id-mapping
                                                                             selector-id
                                                                             (λ () #f))])
                                    (or in-map
                                        selector-id))))))]
                         [(mutator-codes/mutator-new-names ...)
                          (map/count (λ (mutator-id field-contract-id index)
                                       (if (and mutator-id (is-new-id? index))
                                           (code-for-one-id/new-name 
                                            stx
                                            mutator-id #f
                                            (build-mutator-contract struct-name
                                                                    predicate-id
                                                                    field-contract-id)
                                            #f)
                                           #f))
                                     mutator-ids
                                     field-contract-ids)]
                         [(predicate-code predicate-new-name)
                          (code-for-one-id/new-name stx predicate-id #f (syntax predicate/c) #f)]
                         [(constructor-code constructor-new-name)
                          (if omit-constructor?
                              #'((void) (void))
                              (code-for-one-id/new-name
                               stx
                               chaperone-constructor-id struct-name
                               (build-constructor-contract stx
                                                           field-contract-ids
                                                           predicate-id)
                               constructor-id
                               #t
                               (not type-is-only-constructor?)))]

                         [(field-contract-id-definitions ...)
                          (filter values
                                  (map (λ (field-contract-id field-contract)
                                         (with-syntax ([field-contract-id field-contract-id]
                                                       [field-contract field-contract])
                                           #'(define field-contract-id 
                                               (coerce-contract 'provide/contract field-contract)
                                               #;
                                               (opt/c field-contract #:error-name provide/contract))))
                                       field-contract-ids
                                       field-contracts))]
                         [(field-contracts ...) field-contracts]
                         [(field-contract-ids ...) field-contract-ids])

             (with-syntax ([((mutator-codes mutator-new-names) ...)
                            (filter syntax-e (syntax->list #'(mutator-codes/mutator-new-names ...)))])
               (with-syntax ([(rev-selector-new-names ...)
                              (reverse (syntax->list (syntax (selector-new-names ...))))]
                             [(rev-mutator-new-names ...)
                              (reverse (syntax->list (syntax (mutator-new-names ...))))])
                 (with-syntax ([struct-code
                                (with-syntax ([id-rename
                                               (or (free-identifier-mapping-get struct-id-mapping
                                                                                struct-name
                                                                                (λ () #f))
                                                   (error 'contract/provide.rkt
                                                          "internal error.2: ~s"
                                                          struct-name))]
                                              [struct-name struct-name]
                                              [-struct:struct-name -struct:struct-name]
                                              [super-id 
                                               (if (boolean? super-id)
                                                   super-id
                                                   (with-syntax ([the-super-id
                                                                  (or (free-identifier-mapping-get
                                                                       struct-id-mapping
                                                                       super-id
                                                                       (λ () #f))
                                                                      super-id)])
                                                     (syntax (quote-syntax the-super-id))))]
                                              [(mutator-id-info ...)
                                               (for/list ([x (in-list
                                                              (syntax->list
                                                               #'(mutator-codes/mutator-new-names
                                                                  ...)))])
                                                 (syntax-case x ()
                                                   [(a b) #'(quote-syntax b)]
                                                   [else #f]))]
                                              [(exported-selector-ids ...) (reverse selector-ids)])
                                  (define proc
                                    #`(λ ()
                                        (list (quote-syntax -struct:struct-name)
                                              #,(if type-is-only-constructor?
                                                    #'(quote-syntax id-rename)
                                                    #'(quote-syntax constructor-new-name))
                                              (quote-syntax predicate-new-name)
                                              (list (quote-syntax rev-selector-new-names) ...
                                                    (quote-syntax rev-selector-old-names) ...)
                                              (list mutator-id-info ...)
                                              super-id)))
                                  #`(begin
                                      (provide (rename-out [id-rename struct-name]))
                                      (define-syntax id-rename
                                        #,(if (and type-is-constructor? (not omit-constructor?))
                                              #`(make-applicable-struct-info 
                                                 #,proc
                                                 (lambda ()
                                                   (quote-syntax constructor-new-name)))
                                              #`(make-struct-info #,proc)))))]
                               [struct:struct-name struct:struct-name]
                               [-struct:struct-name -struct:struct-name]
                               [struct-name struct-name]
                               [(selector-ids ...) selector-ids]
                               [(constructor-args ...) (generate-temporaries selector-ids)]
                               [struct-name-srcloc `'(,(and (path-string? (syntax-source struct-name))
                                                            (path->relative-string/library
                                                             (syntax-source struct-name)))
                                                      ,(syntax-line struct-name)
                                                      ,(syntax-column struct-name)
                                                      ,(syntax-position struct-name)
                                                      ,(syntax-span struct-name))])
                   (quasisyntax/loc stx
                     (begin
                       struct-code
                       field-contract-id-definitions ...
                       selector-codes ...
                       mutator-codes ...
                       predicate-code
                       (define #,chaperone-constructor-id
                         (let ([struct-name
                                (λ (constructor-args ...)
                                  (chaperone-struct (#,constructor-id constructor-args ...)
                                                    struct:struct-name
                                                    struct-info
                                                    (λ (struct-type skipped?)
                                                      (values -struct:struct-name skipped?))))])
                           struct-name))
                       constructor-code

                       ;; expanding out the body of the `make-pc-struct-type' function
                       ;; directly here in the expansion makes this very expensive at compile time
                       ;; when there are a lot of provide/contract clause using structs
                       (define -struct:struct-name
                         (make-pc-struct-type 'struct-name
                                              struct-name-srcloc
                                              struct:struct-name
                                              field-contract-ids ...))
                       (provide (rename-out [-struct:struct-name struct:struct-name]))))))))))

       (define (map/count f . ls)
         (let loop ([ls ls]
                    [i 0])
           (cond
             [(andmap null? ls) '()]
             [(ormap null? ls) (error 'map/count "mismatched lists")]
             [else (cons (apply f (append (map car ls) (list i)))
                         (loop (map cdr ls)
                               (+ i 1)))])))

       ;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
       (define (andmap/count f l1)
         (let loop ([l1 l1]
                    [i 0])
           (cond
             [(null? l1) #t]
             [else (and (f (car l1) i)
                        (loop (cdr l1)
                              (+ i 1)))])))

       ;; get-field-counts/struct-names : syntax syntax -> (listof (cons number symbol))
       ;; returns a list of numbers corresponding to the numbers of fields for each parent struct
       (define (get-field-counts/struct-names struct-name provide-stx)
         (let loop ([parent-info-id struct-name]
                    [orig-struct? #t])
           (let ([parent-info
                  (and (identifier? parent-info-id)
                       (a:lookup-struct-info parent-info-id provide-stx))])
             (cond
               [(boolean? parent-info) null]
               [else
                (let ([fields (list-ref parent-info 3)]
                      [predicate (list-ref parent-info 2)])
                  (cond
                    [(and (not (null? fields))
                          (not (last fields)))
                     (raise-syntax-error
                      who
                      (format "cannot determine the number of fields in ~astruct"
                              (if orig-struct? "" "parent "))
                      provide-stx
                      struct-name)]
                    [else
                     (cons (cons (length fields) (predicate->struct-name provide-stx predicate))
                           (loop (list-ref parent-info 5) #f))]))]))))

       (define (predicate->struct-name orig-stx stx)
         (and stx
              (let ([m (regexp-match #rx"^(.*)[?]$" (format "~a" (syntax-e stx)))])
                (cond
                  [m (cadr m)]
                  [else (raise-syntax-error 
                         who
                         "unable to cope with a struct supertype whose predicate doesn't end with `?'"
                         orig-stx)]))))

       ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
       (define (build-constructor-contract stx field-contract-ids predicate-id)
         (with-syntax ([(field-contract-ids ...) field-contract-ids]
                       [predicate-id predicate-id])
           (syntax/loc stx
             (->2 field-contract-ids ...
                  predicate-id))))

       ;; build-selector-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-selector-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (->2 predicate-id field-contract-id))))

       ;; build-mutator-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-mutator-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (->2 predicate-id
                        field-contract-id
                        void?))))

       ;; code-for-one-poly-id : syntax -> syntax
       (define (code-for-one-poly-id x x-gen poly)
         (if (or (eq? '#:exists (syntax-e poly)) (eq? '#:∃ (syntax-e poly)))
             #`(define #,x-gen (new-∃/c '#,x))
             #`(define #,x-gen (new-∀/c '#,x))))

       (define (add-exists-binders stx exists-binders)
         (if (null? exists-binders)
             stx
             #`(let #,exists-binders #,stx)))

       (define (add-a-binder id id-gen binders)
         (cons #`[#,id #,id-gen] binders))

       ;; code-for-one-id : syntax syntax syntax (union syntax #f) -> syntax
       ;; given the syntax for an identifier and a contract,
       ;; builds a begin expression for the entire contract and provide
       ;; the first syntax object is used for source locations
       (define (code-for-one-id stx id reflect-id ctrct user-rename-id)
         (with-syntax ([(code id) (code-for-one-id/new-name stx id reflect-id ctrct user-rename-id)])
           (syntax code)))

       (define (id-for-one-id user-rename-id reflect-id id [mangle-for-maker? #f])
         ((if mangle-for-maker?
              a:mangle-id-for-maker
              a:mangle-id)
          "provide/contract-id"
          (or user-rename-id reflect-id id)))
       
       (define pos-module-source-id
         ;; Avoid context on this identifier, since it will be defined
         ;; in another module, and the definition may have to pull
         ;; along all context to support `module->namespace`:
         (datum->syntax #f 'pos-module-source))

       (define (code-for-one-id/new-name stx id reflect-id ctrct/no-prop user-rename-id
                                         [mangle-for-maker? #f]
                                         [provide? #t]) 
         (tl-code-for-one-id/new-name id-for-one-id
                                      stx id reflect-id ctrct/no-prop user-rename-id
                                      pos-module-source-id
                                      mangle-for-maker?
                                      provide?))

       (define p/c-clauses (syntax->list (syntax (p/c-ele ...))))
       (define struct-id-mapping (make-free-identifier-mapping))
       (define (add-struct-clause-to-struct-id-mapping a parent flds/stx)
         (define flds (syntax->list flds/stx))
         (when (and (identifier? a)
                    (struct-info? (syntax-local-value a (λ () #f)))
                    (or (not parent)
                        (and (identifier? parent)
                             (struct-info? (syntax-local-value parent (λ () #f)))))
                    flds
                    (andmap identifier? flds))
           (free-identifier-mapping-put!
            struct-id-mapping
            a
            (a:mangle-id "provide/contract-struct-expandsion-info-id"
                         a))
           (define parent-selectors
             (if parent
                 (let ([parent-selectors (list-ref (extract-struct-info (syntax-local-value parent))
                                                   3)])
                   (length parent-selectors))
                 0))
           ;; this test will fail when the syntax is bad; we catch syntax errors elsewhere
           (when (< parent-selectors (length flds))
             (for ([f (in-list (list-tail flds parent-selectors))])
               (define selector-id (datum->syntax 
                                    a 
                                    (string->symbol (format "~a-~a" (syntax-e a) (syntax-e f)))))
               (free-identifier-mapping-put!
                struct-id-mapping
                selector-id
                (id-for-one-id #f #f selector-id))))))

       (cond
         [just-check-errors?
          (code-for-each-clause p/c-clauses)
          (signal-dup-syntax-error)]
         [else
          (for ([clause (in-list p/c-clauses)])
            (syntax-case* clause (struct) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
              [(struct a ((fld ctc) ...) options ...)
               (identifier? #'a)
               (add-struct-clause-to-struct-id-mapping #'a #f #'(fld ...))]
              [(struct (a b) ((fld ctc) ...) options ...)
               (add-struct-clause-to-struct-id-mapping #'a #'b #'(fld ...))]
              [_ (void)]))
          (with-syntax ([(bodies ...) (code-for-each-clause p/c-clauses)]
                        [pos-module-source-id pos-module-source-id])
            (syntax
             (begin
               (define pos-module-source-id (quote-module-name))
               bodies ...)))]))]))


(define-for-syntax (provide/contract-for-whom stx who)
  (define s-l-c (syntax-local-context))
  (case s-l-c
   [(module-begin)
    #`(begin ;; force us into the 'module' local context
             #,stx)]
   [(module) ;; the good case
    (true-provide/contract stx #f who)]
   [else ;; expression or internal definition
    (raise-syntax-error who
                        (format "not allowed in a ~a context"
                                (if (pair? s-l-c)
                                    "internal definition"
                                    s-l-c))
                        stx)]))

(define-syntax (provide/contract stx) 
  (provide/contract-for-whom stx 'provide/contract))
(define-syntax (provide/contract-for-contract-out stx)
  (provide/contract-for-whom stx 'contract-out))

(define (make-pc-struct-type struct-name srcloc struct:struct-name . ctcs)
  (chaperone-struct-type
   struct:struct-name
   (λ (a b c d e f g h) (values a b c d e f g h))
   (λ (x) x)
   (λ args
     (define name #f)
     (define vals
       (let loop ([args args])
         (cond
           [(null? args) null]
           [(null? (cdr args))
            (set! name (car args))
            null]
           [else (cons (car args) (loop (cdr args)))])))
     (apply values
            (map (λ (ctc val)
                   (contract ctc
                             val
                             'not-enough-info-for-blame
                             'not-enough-info-for-blame
                             name
                             srcloc))
                 ctcs
                 vals)))))
