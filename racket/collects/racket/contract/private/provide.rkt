#lang racket/base

(provide provide/contract
         provide/contract-for-contract-out
         define-module-boundary-contract
         (protect-out (for-syntax build-definition-of-plus-one-acceptor ;; used in test suite
                                  true-provide/contract
                                  ;make-provide/contract-transformer
                                  provide/contract-info?
                                  provide/contract-info-contract-id
                                  provide/contract-info-original-id
                                  contract-rename-id-property
                                  contract-lifted-property
                                  contract-neg-party-property)))

(require (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/struct-info
                     setup/path-to-relative
                     "../../private/struct-util.rkt"
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
         "arrow-val-first.rkt"
         "base.rkt"
         "guts.rkt"
         "exists.rkt"
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

;; make-contract-out-redirect-struct-info
;; : (-> (-> (and/c struct-info? list?)) (-> identifier?) struct-info?)
;; Create a struct-info? value from two thunks:
;;  the 1st must be a valid argument for `make-struct-info`, and
;;  the 2nd must return an identifier for a structure type descriptor.
;; The 2nd thunk is used to recover the original names for a struct --- from before
;;  `contract-out` started to mangle them.
;;
;; make-applicable-contract-out-redirect-struct-info
;; : (-> (-> (and/c struct-info? list?)) (-> identifier?) (-> identifier?) struct-info?)
;; Similar to the above, but the 3rd thunk must return an identifier for a
;;  contract-protected constructor.
;; Creates a value that can be applied to construct instances of the struct type.
;;
;; undo-contract-out-redirect
;; : (-> any/c (or/c identifier? #f))
;; Return the original struct name associated with the argument, or #f if
;;  the input is not an indirect struct info.
(define-values-for-syntax [make-contract-out-redirect-struct-info
                           make-contract-out-redirect/field-struct-info
                           make-applicable-contract-out-redirect-struct-info
                           make-applicable-contract-out-redirect/field-struct-info
                           undo-contract-out-redirect]
  (let ()
    (define-values (struct:r make-r r? r-ref r-set!)
      (make-struct-type
       'contract-out-redirect-struct-info struct:struct-info
       1 0 #f
       '()
       (current-inspector) #f '(0)))

    (define-values (struct:r/field make-r/field r/field? r/field-ref r/field-set!)
      (make-struct-type
       'contract-out-redirect/field-struct-info struct:r
       1 0 #f
       (list (cons prop:struct-field-info
                   (lambda (rec)
                     (r/field-ref rec 0))))))

    (define-values (struct:app-r make-app-r app-r? app-r-ref app-r-set!)
      (make-struct-type
       'applicable-contract-out-redirect-struct-info struct:r
       1 0 #f
       (list (cons prop:procedure
                   (lambda (v stx)
                     (self-ctor-transformer ((app-r-ref v 0)) stx))))
       (current-inspector) #f '(0)))

    (define-values (struct:app-r/field
                    make-app-r/field
                    app-r/field?
                    app-r/field-ref
                    app-r/field-set!)
      (make-struct-type
       'applicable-contract-out-redirect/field-struct-info struct:app-r
       1 0 #f
       (list (cons prop:struct-field-info
                   (lambda (rec)
                     (app-r/field-ref rec 0))))))

    (define (undo-contract-out-redirect v)
      (and (r? v) ((r-ref v 0))))

    (values make-r make-r/field make-app-r make-app-r/field undo-contract-out-redirect)))

(begin-for-syntax

  ;; rename-id : identifier? : the name the lifted expression is bound to
  ;; contract-id : identifier? : the name of the contract expression
  ;; original-id : identifier? : the identifier being contracted
  (struct provide/contract-info (rename-id contract-id original-id))

  ;; keys for syntax property used below
  (define rename-id-key (gensym 'contract:rename-id))
  (define neg-party-key (gensym 'contract:neg-party))

  ;; identifier? identifier? -> identifier?
  ;; add a property that tells clients what the exported id was
  (define (add-rename-id rename-id partial-id)
    (syntax-property partial-id rename-id-key rename-id))

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
                            (add-lifted-property
                             #'(quote-module-name)))))])
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
                         (adjust-location
                          #`(app #,(gen-slow-path-code) more ...))))]))
              ;; In case of partial expansion for module-level and internal-defn
              ;; contexts, delay expansion until it's a good time to lift
              ;; expressions:
              (quasisyntax/loc stx (#%expression #,stx)))))))
  
  (struct provide/contract-transformer provide/contract-info
    (saved-id-table partially-applied-id blame)
    #:property
    prop:set!-transformer
    (λ (self stx)
      (let ([partially-applied-id (provide/contract-transformer-partially-applied-id self)]
            [saved-id-table (provide/contract-transformer-saved-id-table self)]
            [rename-id (provide/contract-info-rename-id self)]
            [blame (provide/contract-transformer-blame self)])
        (with-syntax ([partially-applied-id partially-applied-id]
                      [blame blame])
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
                              #'(with-contract-continuation-mark
                                 (cons blame 'no-negative-party)
                                 (partially-applied-id (quote-module-name))))))))])
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

  (define (make-provide/contract-transformer rename-id cid id eid pos [pid #f] [blame #f])
    (if pid
        (provide/contract-transformer rename-id cid id (make-hasheq) pid blame)
        (begin
          ;; TODO: this needs to change!
          ;; syntax/parse uses this
          ;; this will just drop contracts for now.
          ;; VS: is this still the case? this function is not exported anymore
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


(define-for-syntax current-unprotected-submodule-name (make-parameter #f))

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
              ex-id)))
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
                                                                    pos-module-source
                                                                    #f)
                             #,@(let ([upe (current-unprotected-submodule-name)])
                                  (if upe
                                      (list #`(module+ #,upe
                                                (provide (rename-out [#,id external-name]))))
                                      (list)))
                             #,@(if provide?
                                    (list #`(provide (rename-out [#,id-rename external-name])))
                                    null)))
                    'provide/contract-original-contract
                    (vector #'external-name #'ctrct))])
      #`(code #,id-rename))))

;; syntax -> syntax
;; returns an expression that evaluates to the source location of the argument
(define-for-syntax (stx->srcloc-expr srcloc-stx)
  #`(quote-srcloc #,srcloc-stx))

(define-for-syntax (internal-function-to-be-figured-out ctrct
                                                        id 
                                                        ex-id
                                                        name-for-blame
                                                        id-rename
                                                        srcloc-expr
                                                        contract-error-name
                                                        pos-module-source
                                                        context-limit)
  (with-syntax ([id id]
                [(partially-applied-id extra-neg-party-argument-fn contract-id blame-id) 
                 (generate-temporaries (list 'idX 'idY 'idZ 'idB))]
                [ctrct ctrct])
    (define-values (arrow? definition-of-plus-one-acceptor the-valid-app-shapes)
      (build-definition-of-plus-one-acceptor #'ctrct
                                             #'id
                                             #'extra-neg-party-argument-fn
                                             #'contract-id
                                             #'blame-id))
    (syntax-local-lift-module-end-declaration
     #`(begin 
         (define-values (partially-applied-id blame-id)
           (do-partial-app contract-id
                           id
                           '#,name-for-blame
                           #,pos-module-source
                           #,srcloc-expr
                           #,context-limit))
         #,@(if arrow?
                (list definition-of-plus-one-acceptor)
                (list))))

    #`(begin
        (define contract-id
          ;; let is here to give the right name.
          (let ([#,ex-id #,(if arrow?
                               #'ctrct
                               #`(coerce-contract '#,contract-error-name ctrct))
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
                   (quote-syntax partially-applied-id)
                   (quote-syntax blame-id)))))))

(define-for-syntax (build-definition-of-plus-one-acceptor ctrct
                                                          id
                                                          extra-neg-party-argument-fn
                                                          contract-id
                                                          blame-id)
  (define-values (arrow? the-valid-app-shapes
                         build-plus-one-acceptor
                         plus-one-arity-function-code)
    (syntax-case ctrct (-> ->* ->i)
      [(-> . _) 
       (not (->-arity-check-only->? ctrct))
       (let ()
         (define-values (valid-app-shapes plus-one-arity-function-code)
           (->-valid-app-shapes ctrct))
         (values #t
                 valid-app-shapes
                 #'build->*-plus-one-acceptor
                 plus-one-arity-function-code))]
      [(->* . _)
       (cond
         [(->*-arity-check-only->? ctrct) (values #f #f #f #f)]
         [else
          (define-values (shapes plus-one-arity-function-code)
            (->*-valid-app-shapes ctrct))
          (if shapes
              (values #t shapes #'build->*-plus-one-acceptor plus-one-arity-function-code)
              (values #f #f #f #f))
          ])]
      [_ (values #f #f #f #f)]))
  (values arrow?
          #`(define #,extra-neg-party-argument-fn
              (#,build-plus-one-acceptor (#,plus-one-arity-function-code #,id)
                                         #,blame-id
                                         #,contract-id))
          the-valid-app-shapes))

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
          (define-values (pos-blame-party-expr srcloc-expr name-for-blame context-limit)
            (let loop ([kwd-args (syntax->list #'(kwd-args ...))]
                       [pos-blame-party-expr #'(quote-module-path)]
                       [srcloc-expr #f]
                       [name-for-blame #f]
                       [context-limit #f])
              (cond
                [(null? kwd-args) (values pos-blame-party-expr
                                          (or srcloc-expr (stx->srcloc-expr stx))
                                          (or name-for-blame #'new-id)
                                          context-limit)]
                [else
                 (define kwd (car kwd-args))
                 (cond 
                   [(equal? (syntax-e kwd) '#:pos-source)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:pos-source"
                                          stx))
                    (loop (cddr kwd-args)
                          (cadr kwd-args)
                          srcloc-expr
                          name-for-blame
                          context-limit)]
                   [(equal? (syntax-e kwd) '#:srcloc)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:srcloc"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          (cadr kwd-args)
                          name-for-blame
                          context-limit)]
                   [(equal? (syntax-e kwd) '#:name-for-blame)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:name-for-blame"
                                          stx))
                    (define name-for-blame (cadr kwd-args))
                    (unless (identifier? name-for-blame)
                      (raise-syntax-error #f "expected an identifier to follow #:name-for-blame"
                                          stx
                                          name-for-blame))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          srcloc-expr
                          name-for-blame
                          context-limit)]
                   [(equal? (syntax-e kwd) '#:context-limit)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected an expression to follow #:context-limit"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          srcloc-expr
                          name-for-blame
                          (cadr kwd-args))]
                   [else
                    (raise-syntax-error
                     #f
                     (string-append
                      "expected one of the keywords"
                      " #:pos-source, #:srcloc, #:name-for-blame, or #:context-limit")
                     stx
                     (car kwd-args))])])))
          (internal-function-to-be-figured-out #'ctrct
                                               #'orig-id
                                               #'orig-id
                                               name-for-blame
                                               #'new-id
                                               srcloc-expr
                                               'define-module-boundary-contract
                                               pos-blame-party-expr
                                               context-limit))])]))

;; ... -> (values (or/c #f (-> neg-party val)) blame)
(define (do-partial-app ctc val name pos-module-source source context-limit)
  (define p (parameterize ([warn-about-val-first? #f])
              ;; when we're building the val-first projection
              ;; here we might be needing the plus1 arity
              ;; function (which will be on the val first's result)
              ;; so this is a legtimate use. don't warn.
              (get/build-val-first-projection ctc)))
  (define blme (make-blame (build-source-location source)
                           name
                           (λ () (contract-name ctc))
                           pos-module-source
                           #f #t
                           #:context-limit context-limit))
  (with-contract-continuation-mark
   (cons blme 'no-negative-party) ; we don't know the negative party yet
   ;; computing neg-accepter may involve some front-loaded checking. instrument
   (define neg-accepter ((p blme) val))

   ;; check as much as we can while knowing only the
   ;; contracted value (e.g., function arity)
   ;; we don't have the negative blame here, but we
   ;; expect only positive failures from this; do the
   ;; check and then toss the results.
   (neg-accepter 'incomplete-blame-from-provide.rkt)

   (values neg-accepter blme)))

(define-for-syntax (true-provide/contract provide-stx just-check-errors? who)
  (syntax-case provide-stx ()
    [(_ p/c-ele ...)
     (let ()

       (define mangled-id-scope (make-syntax-introducer))

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
                   (unless (<= (length (syntax->list #'(options ...))) 1)
                     (raise-syntax-error who
                                         "malformed struct option"
                                         provide-stx))
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
               [_ #f])))

       ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
       ;; constructs the code for a struct clause
       ;; first arg is the original syntax object, for source locations
       (define (build-struct-code stx struct-name-position field-names field-contracts 
                                  omit-constructor?)
         (let* ([struct-name (syntax-case struct-name-position ()
                               [(a b) (syntax a)]
                               [else struct-name-position])]
                [the-struct-info (a:lookup-struct-info struct-name-position provide-stx)]
                [true-field-names (and (struct-field-info? the-struct-info)
                                       (struct-field-info-list the-struct-info))]
                [orig-struct-name
                  (or (undo-contract-out-redirect the-struct-info)
                      struct-name)]
                [the-struct-info-list (extract-struct-info the-struct-info)]
                [orig-struct-info-list (extract-struct-info (syntax-local-value orig-struct-name))]
                [constructor-id (list-ref the-struct-info-list 1)]
                [predicate-id (list-ref the-struct-info-list 2)]
                [orig-predicate-id (list-ref orig-struct-info-list 2)]
                [selector-ids (reverse (list-ref the-struct-info-list 3))]
                [_ (when (and (not (null? selector-ids))
                              (not (last selector-ids)))
                     (raise-syntax-error
                      who
                      (format "cannot determine the number of fields in struct")
                      provide-stx
                      struct-name))]
                [orig-selector-ids (reverse (list-ref orig-struct-info-list 3))]
                [super-id (list-ref the-struct-info-list 5)]
                [parent-struct-count (cond
                                       [(boolean? super-id) #f]
                                       [else (length
                                              (list-ref
                                               (extract-struct-info
                                                (a:lookup-struct-info
                                                 super-id
                                                 provide-stx))
                                               3))])]
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
                [mutator-ids (reverse (list-ref the-struct-info-list 4))] ;; (listof (union #f identifier))
                [orig-mutator-ids (reverse (list-ref orig-struct-info-list 4))]

                [struct:struct-name
                 (or (list-ref the-struct-info-list 0)
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
                           (length field-names))
             (raise-syntax-error who
                                 (format "found ~a field~a in struct, but ~a contract~a"
                                         (length selector-ids)
                                         (if (= 1 (length selector-ids)) "" "s")
                                         (length field-names)
                                         (if (= 1 (length field-names)) "" "s"))
                                 provide-stx
                                 struct-name))

           ;; make sure the field names are right.
           (define all-field+struct-names
             (extract-field+struct-names the-struct-info struct-name provide-stx))
           (for ([field+struct-name (in-list all-field+struct-names)]
                 [field-name (in-list (reverse field-names))])
             (define field-name-should-be (car field+struct-name))
             (define field-name-is (syntax-e field-name))
             (unless (equal? field-name-should-be field-name-is)
               (raise-syntax-error who
                                   (format "expected field name to be ~a, but found ~a"
                                           field-name-should-be
                                           field-name-is)
                                   provide-stx
                                   field-name)))

           (define (make-identifier sym)
             (datum->syntax #f sym))

           (define field-contract-ids
             (for/list ([field+struct-name (in-list all-field+struct-names)])
               (mangled-id-scope
                (a:mangle-id "provide/contract-field-contract"
                             (make-identifier (car field+struct-name))
                             (make-identifier (cdr field+struct-name))
                             (make-identifier 'for)
                             struct-name))))

           (with-syntax ([((selector-codes selector-new-names) ...)
                          (for/list ([selector-id (in-list selector-ids)]
                                     [orig-selector-id (in-list orig-selector-ids)]
                                     [field-contract-id (in-list field-contract-ids)]
                                     [index (in-naturals)]
                                     #:when (is-new-id? index))
                            (code-for-one-id/new-name
                             stx
                             selector-id #f
                             (build-selector-contract struct-name
                                                      predicate-id
                                                      field-contract-id)
                             (datum->syntax stx orig-selector-id)))]
                         [(rev-selector-old-names ...)
                          (reverse
                            (for/list ([selector-id (in-list selector-ids)]
                                       [index (in-naturals)]
                                       #:unless (is-new-id? index))
                              (let ([in-map (free-identifier-mapping-get struct-id-mapping
                                                                         selector-id
                                                                         (λ () #f))])
                                (or in-map
                                    selector-id))))]
                         [(mutator-codes/mutator-new-names ...)
                          (for/list ([mutator-id (in-list mutator-ids)]
                                     [orig-mutator-id (in-list orig-mutator-ids)]
                                     [field-contract-id (in-list field-contract-ids)]
                                     [index (in-naturals)])
                            (if (and mutator-id (is-new-id? index))
                                (code-for-one-id/new-name
                                 stx
                                 mutator-id #f
                                 (build-mutator-contract struct-name
                                                         predicate-id
                                                         field-contract-id)
                                 (datum->syntax stx orig-mutator-id))
                                #f))]
                         [(predicate-code predicate-new-name)
                          (code-for-one-id/new-name stx predicate-id #f (syntax predicate/c)
                                                    (datum->syntax stx orig-predicate-id))]
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

             (with-syntax ([((mutator-codes _) ...)
                            (filter syntax-e (syntax->list #'(mutator-codes/mutator-new-names ...)))])
               (with-syntax ([(rev-selector-new-names ...)
                              (reverse (syntax->list (syntax (selector-new-names ...))))])
                 (with-syntax ([struct-code
                                (with-syntax ([id-rename
                                               (or (free-identifier-mapping-get struct-id-mapping
                                                                                struct-name
                                                                                (λ () #f))
                                                   (error 'contract/provide.rkt
                                                          "internal error.2: ~s"
                                                          struct-name))]
                                              [struct-name struct-name]
                                              [orig-struct-name orig-struct-name]
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
                                              [(rev-mutator-id-info ...)
                                               (reverse
                                                 (for/list ([x (in-list
                                                                (syntax->list
                                                                 #'(mutator-codes/mutator-new-names ...)))])
                                                   (syntax-case x ()
                                                     [(a b) #'(quote-syntax b)]
                                                     [else #f])))]
                                              [(exported-selector-ids ...) (reverse selector-ids)])
                                  (define mk
                                    (if (and type-is-constructor? (not omit-constructor?))
                                        (if true-field-names
                                            #'make-applicable-contract-out-redirect/field-struct-info
                                            #'make-applicable-contract-out-redirect-struct-info)
                                        (if true-field-names
                                            #'make-contract-out-redirect/field-struct-info
                                            #'make-contract-out-redirect-struct-info)))
                                  (define proc
                                    #`(λ ()
                                        (list (quote-syntax -struct:struct-name)
                                              #,(if type-is-only-constructor?
                                                    #'(quote-syntax id-rename)
                                                    #'(quote-syntax constructor-new-name))
                                              (quote-syntax predicate-new-name)
                                              (list (quote-syntax rev-selector-new-names) ...
                                                    (quote-syntax rev-selector-old-names) ...)
                                              (list rev-mutator-id-info ...)
                                              super-id)))
                                  (define the-constructor
                                    (if (and type-is-constructor? (not omit-constructor?))
                                        #'((lambda () (quote-syntax constructor-new-name)))
                                        #'()))
                                  (define the-field-names
                                    (if true-field-names
                                        #`('#,true-field-names)
                                        #'()))
                                  #`(begin
                                      (provide (rename-out [id-rename struct-name]))
                                      (define-syntax id-rename
                                        (#,mk
                                         #,proc
                                         (lambda () (quote-syntax orig-struct-name))
                                         #,@the-constructor
                                         #,@the-field-names))))]
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
                         (make-pc-struct-type #,pos-module-source-id
                                              'struct-name
                                              struct-name-srcloc
                                              struct:struct-name
                                              '(#,@field-names)
                                              field-contract-ids ...))
                       (provide (rename-out [-struct:struct-name struct:struct-name]))))))))))

       ;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
       (define (andmap/count f l1)
         (let loop ([l1 l1]
                    [i 0])
           (cond
             [(null? l1) #t]
             [else (and (f (car l1) i)
                        (loop (cdr l1)
                              (+ i 1)))])))

       ;; get-field-names/no-field-info :: string?
       ;;                                  (listof identifier?)
       ;;                                  (or/c identifier? boolean?)
       ;;                                  syntax?
       ;;                                  syntax?
       ;;                                  ->
       ;;                                  (listof symbol?)
       ;; attempts to extract field names from accessors
       (define (get-field-names/no-field-info struct-name
                                              accessors
                                              super-info
                                              orig-struct-name-stx
                                              provide-stx)
         (define own-accessors
           (cond
             [(boolean? super-info) accessors]
             [else
              (define parent-accessors
                (list-ref (extract-struct-info (a:lookup-struct-info super-info provide-stx)) 3))
              (drop-right accessors (length parent-accessors))]))
         (for/list ([accessor (in-list own-accessors)])
           (define accessor-str (symbol->string (syntax-e accessor)))
           (unless (string-prefix? accessor-str (string-append struct-name "-"))
             (raise-syntax-error
              who
              (format "unexpected accessor name ~a should start with ~a-"
                      accessor-str struct-name)
              provide-stx
              orig-struct-name-stx))
           (string->symbol (substring accessor-str (add1 (string-length struct-name))))))

       ;; extract-field+struct-names : struct-info? syntax? syntax? -> (listof (cons/c symbol? symbol?))
       ;; returns a list of pair of field name and the struct name the field belongs to
       (define (extract-field+struct-names the-struct-info orig-struct-name-stx provide-stx)
         (define struct-info-list (extract-struct-info the-struct-info))
         (define predicate (list-ref struct-info-list 2))
         (define accessors (list-ref struct-info-list 3))
         (define super-info (list-ref struct-info-list 5))
         (define struct-name (predicate->struct-name who provide-stx predicate))
         (define immediate-field-names
           (if (struct-field-info? the-struct-info)
               (struct-field-info-list the-struct-info)
               (get-field-names/no-field-info struct-name
                                              accessors
                                              super-info
                                              orig-struct-name-stx
                                              provide-stx)))
         (define immediate-field+struct-names
           (for/list ([fld (in-list immediate-field-names)])
             (cons fld (string->symbol struct-name))))
         (cond
           [(boolean? super-info) immediate-field+struct-names]
           [else (append immediate-field+struct-names
                         (extract-field+struct-names
                          (a:lookup-struct-info super-info provide-stx)
                          orig-struct-name-stx
                          provide-stx))]))

       ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
       (define (build-constructor-contract stx field-contract-ids predicate-id)
         (with-syntax ([(field-contract-ids ...) field-contract-ids]
                       [predicate-id predicate-id])
           (syntax/loc stx
             (-> field-contract-ids ...
                 predicate-id))))

       ;; build-selector-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-selector-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (-> predicate-id field-contract-id))))

       ;; build-mutator-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-mutator-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (-> predicate-id
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
         (mangled-id-scope
          ((if mangle-for-maker?
               a:mangle-id-for-maker
               a:mangle-id)
           "provide/contract-id"
           (or user-rename-id reflect-id id))))
       
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

       (define-values (p/c-clauses unprotected-submodule-name)
         (syntax-case (syntax (p/c-ele ...)) ()
           [(#:unprotected-submodule modname . more)
            (identifier? #'modname)
            (values (syntax->list #'more) (syntax-e #'modname))]
           [(#:unprotected-submodule x . more)
            (raise-syntax-error who
                                "expected a module name to follow #:unprotected-submodule"
                                provide-stx
                                (if (pair? (syntax-e #'more))
                                    (car (syntax-e #'more))
                                    #f))]
           [_ (values (syntax->list (syntax (p/c-ele ...))) #f)]))
       (define struct-id-mapping (make-free-identifier-mapping))
       (define (add-struct-clause-to-struct-id-mapping a flds/stx)
         (define flds (syntax->list flds/stx))
         (define compile-time-info (syntax-local-value a (λ () #f)))
         (when (and (identifier? a)
                    (struct-info? compile-time-info))
           (define parent
             (let ([parent (list-ref (extract-struct-info compile-time-info) 5)])
               (if (boolean? parent) #f parent)))
           (when (and (or (not parent)
                          (and (identifier? parent)
                               (struct-info? (syntax-local-value parent (λ () #f)))))
                      flds
                      (andmap identifier? flds))
             (free-identifier-mapping-put!
              struct-id-mapping
              a
              (mangled-id-scope
               (a:mangle-id "provide/contract-struct-expansion-info-id"
                            a)))
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
                  (id-for-one-id #f #f selector-id)))))))
       (parameterize ([current-unprotected-submodule-name unprotected-submodule-name])
         (cond
           [just-check-errors?
            (code-for-each-clause p/c-clauses)
            (signal-dup-syntax-error)]
           [else
            (for ([clause (in-list p/c-clauses)])
              (syntax-case* clause (struct) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
                [(struct a ((fld ctc) ...) options ...)
                 (identifier? #'a)
                 (add-struct-clause-to-struct-id-mapping #'a #'(fld ...))]
                [(struct (a b) ((fld ctc) ...) options ...)
                 (add-struct-clause-to-struct-id-mapping #'a #'(fld ...))]
                [_ (void)]))
            (with-syntax ([(bodies ...) (code-for-each-clause p/c-clauses)]
                          [pos-module-source-id pos-module-source-id])
              (syntax
               (begin
                 (define pos-module-source-id (quote-module-name))
                 bodies ...)))])))]))


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

(define (make-pc-struct-type pos-module-source struct-name srcloc struct-type field-names . ctcs)
  (define blame
    (make-blame (build-source-location srcloc) struct-type (λ () `(substruct-of ,struct-name))
                pos-module-source #f #t))
  (define late-neg-acceptors
    (for/list ([ctc (in-list ctcs)]
               [field-name (in-list field-names)])
      ((get/build-late-neg-projection ctc)
       (blame-add-context blame
                          (format "the ~a field of" field-name)
                          #:swap? #t))))
  (chaperone-struct-type
   struct-type
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
            (map (λ (late-neg-acceptors val)
                   (late-neg-acceptors val 'not-enough-info-for-blame))
                 late-neg-acceptors
                 vals)))))
