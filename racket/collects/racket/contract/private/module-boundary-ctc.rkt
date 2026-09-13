#lang racket/base

(provide define-module-boundary-contract
         (protect-out (for-syntax build-definition-of-plus-one-acceptor ;; used in test suite
                                  ;make-provide/contract-transformer
                                  define-module-boundary-contract/proc
                                  provide/contract-info?
                                  provide/contract-info-contract-id
                                  provide/contract-info-original-id
                                  contract-rename-id-property
                                  contract-lifted-property
                                  contract-neg-party-property)))

(require (for-syntax racket/base
                     "application-arity-checking.rkt")
         "arrow-val-first.rkt"
         "base.rkt"
         "guts.rkt"
         "blame.rkt"
         syntax/location
         syntax/srcloc)

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

;; make-unprotected-submodule-code
;; : (-> (-> syntax?) (listof syntax?))
;; Calls `thunk` if `current-unprotected-submodule-name` is non-#f,
;; incorporating the resulting syntax object into a singleton list of
;; `module+` form; otherwise returns empty list.
(define-for-syntax (make-unprotected-submodule-code thunk)
  (define upe-submod (current-unprotected-submodule-name))
  (if upe-submod (list #`(module+ #,upe-submod #,(thunk))) null))

(define-for-syntax (define-module-boundary-contract/proc
                     id-rename
                     id
                     ctrct
                     name-for-contract
                     name-for-blame
                     srcloc-expr
                     contract-error-name
                     pos-module-source
                     context-limit
                     lift-to-end?
                     start-swapped?)
  (with-syntax ([(partially-applied-id extra-neg-party-argument-fn contract-id blame-id)
                 (generate-temporaries (list 'id-partially-applied 'id-extra-neg-party-argument-fn 'id-contract 'id-blame))]
                [ctrct ctrct])
    (define-values (arrow? definition-of-plus-one-acceptor the-valid-app-shapes)
      (build-definition-of-plus-one-acceptor #'ctrct
                                             id
                                             #'extra-neg-party-argument-fn
                                             #'contract-id
                                             #'blame-id))
    (define maybe-at-end
      (cons
       #`(define-values (partially-applied-id blame-id)
           (do-partial-app contract-id
                           #,id
                           '#,name-for-blame
                           #,pos-module-source
                           #,srcloc-expr
                           #,context-limit
                           #,start-swapped?))
       (if arrow?
           (list definition-of-plus-one-acceptor)
           (list))))
    (when lift-to-end?
      (syntax-local-lift-module-end-declaration #`(begin #,@maybe-at-end)))


    #`(begin
        (define contract-id
          ;; let is here to give the right name.
          (let ([#,name-for-contract
                 #,(if arrow?
                       #'ctrct
                       #`(coerce-contract '#,contract-error-name ctrct))
                 #;(opt/c ctrct #:error-name #,contract-error-name)])
            #,name-for-contract))

        (define-syntax #,id-rename
          #,(if arrow?
                #`(make-provide/contract-arrow-transformer
                   (quote-syntax #,id-rename)
                   (quote-syntax contract-id) (quote-syntax #,id)
                   (quote-syntax partially-applied-id)
                   (quote-syntax extra-neg-party-argument-fn)
                   #,the-valid-app-shapes)
                #`(make-provide/contract-transformer
                   (quote-syntax #,id-rename)
                   (quote-syntax contract-id) (quote-syntax #,id)
                   #f #f
                   (quote-syntax partially-applied-id)
                   (quote-syntax blame-id))))
        #,@(if lift-to-end?
               '()
               maybe-at-end))))

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
          (define-values (pos-blame-party-expr
                          srcloc-expr
                          name-for-blame name-for-contract
                          context-limit lift-to-end?
                          start-swapped?)
            (let loop ([kwd-args (syntax->list #'(kwd-args ...))]
                       [pos-blame-party-expr #'(quote-module-path)]
                       [srcloc-expr #f]
                       [name-for-blame #f]
                       [name-for-contract #f]
                       [context-limit #f]
                       [lift-to-end? #t]
                       [start-swapped? #f])
              (cond
                [(null? kwd-args) (values pos-blame-party-expr
                                          (or srcloc-expr #`(quote-srcloc #,stx))
                                          (or name-for-blame #'new-id)
                                          (or name-for-contract #'orig-id)
                                          context-limit
                                          lift-to-end?
                                          start-swapped?)]
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
                          name-for-contract
                          context-limit
                          lift-to-end?
                          start-swapped?)]
                   [(equal? (syntax-e kwd) '#:srcloc)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:srcloc"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          (cadr kwd-args)
                          name-for-blame
                          name-for-contract
                          context-limit
                          lift-to-end?
                          start-swapped?)]
                   [(equal? (syntax-e kwd) '#:name-for-blame)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected an argument to follow #:name-for-blame"
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
                          name-for-contract
                          context-limit
                          lift-to-end?
                          start-swapped?)]
                   [(equal? (syntax-e kwd) '#:name-for-contract)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected an argument to follow #:name-for-contract"
                                          stx))
                    (define name-for-contract (cadr kwd-args))
                    (unless (identifier? name-for-blame)
                      (raise-syntax-error #f "expected an identifier to follow #:name-for-contract"
                                          stx
                                          name-for-blame))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          srcloc-expr
                          name-for-blame
                          name-for-contract
                          context-limit
                          lift-to-end?
                          start-swapped?)]
                   [(equal? (syntax-e kwd) '#:context-limit)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected an expression to follow #:context-limit"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          srcloc-expr
                          name-for-blame
                          name-for-contract
                          (cadr kwd-args)
                          lift-to-end?
                          start-swapped?)]
                   [(equal? (syntax-e kwd) '#:lift-to-end?)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:lift-to-end?"
                                          stx))
                    (define new-lift-to-end? (syntax-e (cadr kwd-args)))
                    (unless (boolean? new-lift-to-end?)
                      (raise-syntax-error #f "expected a (syntactic) boolean to follow #:lift-to-end?"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          srcloc-expr
                          name-for-blame
                          name-for-contract
                          context-limit
                          new-lift-to-end?
                          start-swapped?)]
                   [(equal? (syntax-e kwd) '#:start-swapped?)
                    (when (null? (cdr kwd-args))
                      (raise-syntax-error #f "expected a keyword argument to follow #:start-swapped?"
                                          stx))
                    (define new-start-swapped? (syntax-e (cadr kwd-args)))
                    (unless (boolean? start-swapped?)
                      (raise-syntax-error #f "expected a (syntactic) boolean to follow #:start-swapped?"
                                          stx))
                    (loop (cddr kwd-args)
                          pos-blame-party-expr
                          srcloc-expr
                          name-for-blame
                          name-for-contract
                          context-limit
                          lift-to-end?
                          new-start-swapped?)]
                   [else
                    (raise-syntax-error
                     #f
                     (string-append
                      "expected one of the keywords"
                      " #:pos-source, #:srcloc, #:name-for-blame, #:name-for-contract,"
                      " #:context-limit, #:lift-to-end?, or #:start-swapped?")
                     stx
                     (car kwd-args))])])))
          (define-module-boundary-contract/proc #'new-id
            #'orig-id
            #'ctrct
            name-for-contract
            name-for-blame
            srcloc-expr
            'define-module-boundary-contract
            pos-blame-party-expr
            context-limit
            lift-to-end?
            start-swapped?))])]))

;; ... -> (values (or/c #f (-> neg-party val)) blame)
(define (do-partial-app ctc val name pos-module-source source context-limit start-swapped?)
  (define p (parameterize ([warn-about-val-first? #f])
              ;; when we're building the val-first projection
              ;; here we might be needing the plus1 arity
              ;; function (which will be on the val first's result)
              ;; so this is a legtimate use. don't warn.
              (get/build-val-first-projection ctc)))
  (define blme (make-blame (build-source-location source)
                           name
                           (λ () (contract-name ctc))
                           (if start-swapped? #f pos-module-source)
                           (if start-swapped? pos-module-source #f)
                           (not start-swapped?) ;; original?
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
