(module unit mzscheme
  (require-for-syntax (lib "list.ss")
                      (lib "boundmap.ss" "syntax")
                      (lib "context.ss" "syntax")
                      (lib "kerncase.ss" "syntax")
                      (lib "name.ss" "syntax")
                      (lib "struct.ss" "syntax")
                      (lib "stx.ss" "syntax")
                      "private/unit-compiletime.ss"
                      "private/unit-syntax.ss")
  
  (require (lib "etc.ss")
           "private/unit-keywords.ss"
           "private/unit-runtime.ss"
           (only "private/unit-compiletime.ss" apply-mac))
  
  (provide define-signature-form struct open
           define-signature provide-signature-elements
           only except rename import export prefix link tag init-depend extends
           unit?
           (rename :unit unit) define-unit 
           compound-unit define-compound-unit compound-unit/infer define-compound-unit/infer
           invoke-unit define-values/invoke-unit
           invoke-unit/infer define-values/invoke-unit/infer
           unit-from-context define-unit-from-context
           define-unit-binding
           unit/new-import-export define-unit/new-import-export)
 
  (define-syntax/err-param (define-signature-form stx)
    (syntax-case stx ()
      ((_ (name arg) . val)
       (begin
         (check-id #'name)
         (check-id #'arg)
         #'(define-syntax name
             (make-set!-transformer
              (make-signature-form (λ (arg) . val))))))
      ((_ . l)
       (let ((l (checked-syntax->list stx)))
         (unless (>= 3 (length l))
           (raise-stx-err 
            (format "expected syntax matching (~a (id id) expr ...)"
                    (syntax-e (stx-car stx)))))
         (unless (= 2 (length (checked-syntax->list (car l))))
           (raise-stx-err
            "expected syntax matching (identifier identifier)"
            (car l)))))))
  
  (define-signature-form (struct stx)
    (parameterize ((error-syntax stx))
      (syntax-case stx ()
        ((_ name (field ...) . omissions)
         (let ([omit-selectors #f]
               [omit-setters #f]
               [omit-constructor #f]
               [omit-type #f])
           (define (remove-ctor&type-name l)
             (cond
               ((and omit-constructor omit-type)
                (cddr l))
               (omit-type
                (cdr l))
               (omit-constructor
                (cons (car l) (cddr l)))
               (else
                l)))
           (define (remove-ctor&type-info l)
             (define new-type
               (if omit-type
                   #f
                   (cadr l)))
             (define new-ctor
               (if omit-constructor
                   #f
                   (caddr l)))
             (cons-immutable (car l)
                             (cons-immutable new-type
                                             (cons-immutable new-ctor
                                                             (cdddr l)))))
           (check-id #'name)
           (for-each check-id (syntax->list #'(field ...)))
           (for-each
            (lambda (omission)
              (cond
                ((and (identifier? omission)
                      (module-identifier=? omission #'-selectors))
                 (set! omit-selectors #t))
                ((and (identifier? omission)
                      (module-identifier=? omission #'-setters))
                 (set! omit-setters #t))
                ((and (identifier? omission)
                      (module-identifier=? omission #'-constructor))
                 (set! omit-constructor #t))
                ((and (identifier? omission)
                      (module-identifier=? omission #'-type))
                 (set! omit-type #t))
                (else
                 (raise-stx-err
                  "expected \"-selectors\" or \"-setters\" or \"-constructor\" or \"-type\""
                  omission))))
            (checked-syntax->list #'omissions))
           (cons
            #`(define-syntaxes (name)
                #,(remove-ctor&type-info
                   (build-struct-expand-info
                    #'name (syntax->list #'(field ...))
                    omit-selectors omit-setters
                    #f '(#f) '(#f))))
            (remove-ctor&type-name
             (build-struct-names #'name (syntax->list #'(field ...))
                                 omit-selectors omit-setters #f)))))
        ((_ name (x . y) . omissions)
         ;; Will fail
         (checked-syntax->list (stx-car (stx-cdr (stx-cdr stx)))))
        ((_ name fields . omissions)
         (raise-stx-err "expected syntax matching (identifier ...)" #'fields))
        ((_ name)
         (raise-stx-err "missing fields"))
        ((_)
         (raise-stx-err "missing name and fields")))))

  
  ;; build-val+macro-defs : sig -> (list syntax-object^3)
  (define-for-syntax (build-val+macro-defs sig)
    (with-syntax ([(((int-ivar . ext-ivar) ...)
                    ((((int-vid . ext-vid) ...) . vbody) ...)
                    ((((int-sid . ext-sid) ...) . sbody) ...))
                   (map-sig (lambda (x) x)
                            (make-syntax-introducer)
                            sig)
                   #;(add-context-to-sig sig)])
      (list
       #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
          (values
           (make-rename-transformer 
            (quote-syntax int-ivar)) ...
           (make-rename-transformer
            (quote-syntax int-vid)) ... ...
           (make-rename-transformer
            (quote-syntax int-sid)) ... ...))
       #'(((int-sid ...) sbody) ...)
       #'(((int-vid ...) vbody) ...))))
  
  
  (define-signature-form (open stx)
    (parameterize ([error-syntax stx])
      (syntax-case stx ()
        ((_ export-spec)
         (let ([sig (process-spec #'export-spec)])
           (with-syntax ((((int . ext) ...) (car sig))
                         ((renames
                           (((mac-name ...) mac-body) ...) 
                           (((val-name ...) val-body) ...))
                          (build-val+macro-defs sig)))
             (syntax->list
              #'(int ...
                 (define-syntaxes . renames)
                 (define-syntaxes (mac-name ...) mac-body) ...
                 (define-values (val-name ...) val-body) ...)))))
        (_
         (raise-stx-err (format "must match (~a export-spec)"
                                (syntax-e (stx-car stx))))))))
  
  
  (define-for-syntax (introduce-def d)
    (cons (map syntax-local-introduce (car d))
          (syntax-local-introduce (cdr d))))
  
  ;; build-define-syntax : identifier (or/c identifier #f) syntax-object -> syntax-object
  (define-for-syntax (build-define-signature sigid super-sigid sig-exprs)
    (unless (or (stx-null? sig-exprs) (stx-pair? sig-exprs))
      (raise-stx-err "expected syntax matching (sig-expr ...)" sig-exprs))
    (let ([ses (checked-syntax->list sig-exprs)])
      (define-values (super-names super-ctimes super-rtimes super-bindings
                                  super-val-defs super-stx-defs)
        (if super-sigid
            (let* ([super-sig (lookup-signature super-sigid)]
                   [super-siginfo (signature-siginfo super-sig)])
              (values (siginfo-names super-siginfo)
                      (siginfo-ctime-ids super-siginfo)
                      (map syntax-local-introduce
                           (siginfo-rtime-ids super-siginfo))
                      (map syntax-local-introduce (signature-vars super-sig))
                      (map introduce-def (signature-val-defs super-sig))
                      (map introduce-def (signature-stx-defs super-sig))))
            (values '() '() '() '() '() '())))
      (let loop ((sig-exprs ses)
                 (bindings null)
                 (val-defs null)
                 (stx-defs null))
        (cond
          ((null? sig-exprs)
           (let* ([all-bindings (append super-bindings (reverse bindings))]
                  [all-val-defs (append super-val-defs (reverse val-defs))]
                  [all-stx-defs (append super-stx-defs (reverse stx-defs))]
                  [dup
                   (check-duplicate-identifier
                    (append all-bindings
                            (apply append (map car all-val-defs))
                            (apply append (map car all-stx-defs))))])
             (when dup
               (raise-stx-err "duplicate identifier" dup))
             (with-syntax (((super-rtime ...) super-rtimes)
                           ((super-name ...) super-names)
                           ((var ...) all-bindings)
                           ((((vid ...) . vbody) ...) all-val-defs)
                           ((((sid ...) . sbody) ...) all-stx-defs))
               #`(begin
                   (define signature-tag (gensym))
                   (define-syntax #,sigid
                     (make-set!-transformer
                      (make-signature
                       (make-siginfo (list #'#,sigid #'super-name ...)
                                     (list ((syntax-local-certifier) (quote-syntax signature-tag))
                                           #'super-rtime
                                           ...))
                       (list (quote-syntax var) ...)
                       (list (cons (list (quote-syntax vid) ...)
                                   ((syntax-local-certifier)
                                    (quote-syntax vbody)))
                             ...)
                       (list (cons (list (quote-syntax sid) ...)
                                   ((syntax-local-certifier)
                                    (quote-syntax sbody)))
                             ...))))))))
          (else
           (syntax-case (car sig-exprs) (define-values define-syntaxes)
             (x
              (identifier? #'x)
              (loop (cdr sig-exprs) (cons #'x bindings) val-defs stx-defs))
             ((x . y)
              (and (identifier? #'x)
                   (or (module-identifier=? #'x #'define-values)
                       (module-identifier=? #'x #'define-syntaxes)))
              (begin
                (check-def-syntax (car sig-exprs))
                (syntax-case #'y ()
                  (((name ...) body)
                   (begin
                     (for-each (lambda (id) (check-id id))
                               (syntax->list #'(name ...)))
                     (let ((b #'body))
                       (loop (cdr sig-exprs)
                             bindings
                             (if (module-identifier=? #'x #'define-values)
                                 (cons (cons (syntax->list #'(name ...)) b)
                                       val-defs)
                                 val-defs)
                             (if (module-identifier=? #'x #'define-syntaxes)
                                 (cons (cons (syntax->list #'(name ...)) b)
                                       stx-defs)
                                 stx-defs))))))))
             ((x . y)
              (let ((trans 
                     (set!-trans-extract
                      (syntax-local-value
                       (syntax-local-introduce #'x)
                       (lambda ()
                         (raise-stx-err "unknown signature form" #'x))))))
                (unless (signature-form? trans)
                  (raise-stx-err "not a signature form" #'x))
                (let ((results ((signature-form-f trans) (car sig-exprs))))
                  (unless (list? results)
                    (raise-stx-err
                     (format "expected list of results from signature form, got ~e" results)
                     (car sig-exprs)))
                  (loop (append results (cdr sig-exprs))
                        bindings
                        val-defs
                        stx-defs))))
             (x (raise-stx-err 
                 "expected either an identifier or signature form"
                 #'x))))))))

  
  (define-syntax/err-param (define-signature stx)
    (syntax-case stx (extends)
      ((_ sig-name sig-exprs)
       (begin
         (check-id #'sig-name)
         (build-define-signature #'sig-name #f #'sig-exprs)))
      ((_ sig-name extends super-name sig-exprs)
       (begin
         (check-id #'sig-name)
         (check-id #'super-name)
         (build-define-signature #'sig-name #'super-name #'sig-exprs)))
      (_
       (begin
         (checked-syntax->list stx)
         (raise-stx-err
          (format "expected syntax matching (~a identifier (sig-expr ...)) or (~a identifier extends identifier (sig-expr ...))"
                  (syntax-e (stx-car stx)) (syntax-e (stx-car stx))))))))
  
  (define-for-syntax (signature->identifiers sigids)
    (define provide-tagged-sigs (map process-tagged-import sigids))
    (define provide-sigs (map caddr provide-tagged-sigs))
    (map sig-int-names provide-sigs))
  
  (define-syntax/err-param (provide-signature-elements stx)
    (syntax-case stx ()
      ((_ . p)
       (let* ((sigs (checked-syntax->list #'p))
              (nameses (signature->identifiers sigs))
              ;; Export only the names that would be visible to uses
              ;;  with the same lexical context as p. Otherwise, we
              ;;  can end up with collisions with renamings that are
              ;;  symbolically the same, such as those introduced by
              ;;  `open'.
              (nameses (map (lambda (sig names)
                              (filter (lambda (name)
                                        (bound-identifier=?
                                         name
                                         (datum->syntax-object sig (syntax-e name))))
                                      names))
                            sigs nameses))
              (names (apply append nameses))
              (dup (check-duplicate-identifier names)))
         (when dup
           (raise-stx-err (format "duplicate binding for ~e" (syntax-e dup))))
         (quasisyntax/loc stx
           (provide #,@names))))))
              
  ;; A unit is 
  ;; - (unit (import import-spec ...) (export export-spec ...) unit-body-expr ...)
  
  (define-for-syntax (localify exp def-ctx)
    (cadr (syntax->list
           (local-expand #`(stop #,exp)
                         'expression
                         (list #'stop)
                         def-ctx))))

  (define-for-syntax (add-context-to-sig sig)
    (let ((def-ctx (syntax-local-make-definition-context)))
      (syntax-local-bind-syntaxes (sig-ext-names sig) #f def-ctx)
      (map-sig (lambda (x) x)
               (lambda (x) (localify x def-ctx))
               sig)))
    
  (define-for-syntax (iota n)
    (let loop ((n n)
               (acc null))
      (cond
        ((= n 0) acc)
        (else (loop (sub1 n) (cons (sub1 n) acc))))))
  

  (define-syntax (unit-export stx)
    (syntax-case stx ()
      ((_ ((esig ...) elocs) ...)
       (with-syntax ((((kv ...) ...)
                      (map 
                        (lambda (esigs eloc)
                          (map
                           (lambda (esig) #`(#,esig #,eloc))
                           (syntax->list esigs)))
                        (syntax->list #'((esig ...) ...))
                        (syntax->list #'(elocs ...)))))
         #'(hash-table 'equal kv ... ...)))))
  
  ;; build-key : (or symbol #f) identifier -> syntax-object
  (define-for-syntax (build-key tag i)
    (if tag
        #`(cons '#,tag #,i)
        i))
 
  ;; tagged-info->keys : (cons (or symbol #f) siginfo) -> (listof syntax-object)
  (define-for-syntax (tagged-info->keys tagged-info)
    (define tag (car tagged-info))
    (map (lambda (rid) 
           (build-key tag (syntax-local-introduce rid)))
         (siginfo-rtime-ids (cdr tagged-info))))

  ;; check-duplicate-sigs : (listof (cons symbol siginfo)) (listof syntax-object)
  ;;                        (listof (cons symbol siginfo)) (listof syntax-object) ->
  (define-for-syntax (check-duplicate-sigs tagged-siginfos sources tagged-deps dsources)
    (define import-idx (make-hash-table 'equal))
    (for-each
     (lambda (tinfo s)
       (define key (cons (car tinfo)
                         (car (siginfo-ctime-ids (cdr tinfo)))))
       (when (hash-table-get import-idx key (lambda () #f))
         (raise-stx-err "duplicate import signature" s))
       (hash-table-put! import-idx key #t))
     tagged-siginfos
     sources)
    (for-each
     (lambda (dep s)
       (unless (hash-table-get import-idx
                               (cons (car dep)
                                     (car (siginfo-ctime-ids (cdr dep))))
                               (lambda () #f))
         (raise-stx-err "initialization dependency on unknown import" s)))
     tagged-deps
     dsources))
         
  (define-for-syntax (tagged-sigid->tagged-siginfo x)
    (cons (car x)
          (signature-siginfo (lookup-signature (cdr x)))))
                 
  (define-for-syntax (check-unit-ie-sigs import-sigs export-sigs)
    (let ([dup (check-duplicate-identifier
                (apply append (map sig-int-names import-sigs)))])
      (when dup
        (raise-stx-err 
         (format "~a is imported by multiple signatures" (syntax-e dup)))))
    
    (let ([dup (check-duplicate-identifier
                (apply append (map sig-int-names export-sigs)))])
      (when dup
        (raise-stx-err (format "~a is exported by multiple signatures"
                               (syntax-e dup)))))
    
    (let ([dup (check-duplicate-identifier 
                (append
                 (apply append (map sig-int-names import-sigs))
                 (apply append (map sig-int-names export-sigs))))])
      (when dup
        (raise-stx-err (format "import ~a is exported" (syntax-e dup))))))
  

  (define-for-syntax (process-unit-import/export process)
    (lambda (s)
      (define x1 (syntax->list s))
      (define x2 (map process x1))
      (values x1 x2 (map car x2) (map cadr x2) (map caddr x2))))
  
  (define-for-syntax process-unit-import
    (process-unit-import/export process-tagged-import))

  (define-for-syntax process-unit-export
    (process-unit-import/export process-tagged-export))
  
  ;; build-unit : syntax-object -> 
  ;;             (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit expression.  stx must be
  ;; such that it passes check-unit-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define-for-syntax (build-unit stx)
    (syntax-case stx (import export init-depend)
      (((import i ...)
        (export e ...)
        (init-depend id ...)
        . body)
       
       (let* ([d (syntax->list #'(id ...))]
              [dep-tagged-sigids (map check-tagged-id d)]
              [dep-tagged-siginfos 
               (map tagged-sigid->tagged-siginfo dep-tagged-sigids)])
         
         (define-values (isig tagged-import-sigs import-tagged-infos 
                              import-tagged-sigids import-sigs)
           (process-unit-import #'(i ...)))
         
         (define-values (esig tagged-export-sigs export-tagged-infos 
                              export-tagged-sigids export-sigs)
           (process-unit-export #'(e ...)))

         (check-duplicate-sigs import-tagged-infos isig dep-tagged-siginfos d)
         
         (check-duplicate-subs export-tagged-infos esig)
         
         (check-unit-ie-sigs import-sigs export-sigs)
         
         (with-syntax ((((dept . depr) ...)
                        (map
                         (lambda (tinfo)
                           (cons (car tinfo)
                                 (syntax-local-introduce (car (siginfo-rtime-ids (cdr tinfo))))))
                         dep-tagged-siginfos))
                       [((renames (mac ...) (val ...)) ...)
                        (map build-val+macro-defs import-sigs)]
                       [(((int-ivar . ext-ivar) ...) ...) (map car import-sigs)]
                       [(((int-evar . ext-evar) ...) ...) (map car export-sigs)]
                       [((iloc ...) ...)
                        (map (lambda (x) (generate-temporaries (car x))) import-sigs)]
                       [((eloc ...) ...)
                        (map (lambda (x) (generate-temporaries (car x))) export-sigs)]
                       [((import-key import-super-keys ...) ...)
                        (map tagged-info->keys import-tagged-infos)]
                       [((export-key ...) ...)
                        (map tagged-info->keys export-tagged-infos)]
                       [(import-name ...)
                        (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                             import-tagged-infos)]
                       [(export-name ...)
                        (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                             export-tagged-infos)]
                       [name (syntax-local-infer-name (error-syntax))]
                       [(icount ...) (map
                                      (lambda (import) (length (car import)))
                                      import-sigs)])
           (values 
            (quasisyntax/loc (error-syntax)
              (make-unit
               'name
               (vector-immutable (cons-immutable 'import-name
                                                 (vector-immutable import-key import-super-keys ...)) ...)
               (vector-immutable (cons-immutable 'export-name 
                                                 (vector-immutable export-key ...)) ...)
               (list-immutable (cons-immutable 'dept depr) ...)
               (lambda ()
                 (let ([eloc (box undefined)] ... ...)
                   (values 
                    (lambda (import-table)
                      (let-values ([(iloc ...)
                                    (vector->values (hash-table-get import-table import-key) 0 icount)]
                                   ...)
                        (letrec-syntaxes ([(int-ivar ...)
                                           (make-id-mappers
                                            (quote-syntax (unbox iloc))
                                            ...)]
                                          ...
                                          [(int-evar ...)
                                           (make-id-mappers
                                            (quote-syntax (unbox eloc))
                                            ...)]
                                          ...)
                          (letrec-syntaxes+values (renames ...
                                                   mac ... ...)
                            (val ... ...)
                            (unit-body #,(error-syntax)
                                       (int-ivar ... ...)
                                       (int-evar ... ...)
                                       (eloc ... ...)
                                       . body)))))
                    (unit-export ((export-key ...) (vector-immutable eloc ...)) ...))))))
            import-tagged-sigids
            export-tagged-sigids
            dep-tagged-sigids))))))

  (define-syntax/err-param (:unit stx)
    (syntax-case stx ()
      ((_ . x)
       (begin
         (let-values (((u x y z) (build-unit (check-unit-syntax #'x))))
           u)))))
  
  (define-syntax (unit-body stx)
    (syntax-case stx ()
      ((_ err-stx ivars evars elocs body ...)
       (parameterize ((error-syntax #'err-stx))
         (let* ([expand-context (generate-expand-context)]
                [def-ctx (syntax-local-make-definition-context)]
                [local-ivars (syntax->list (localify #'ivars def-ctx))]
                [local-evars (syntax->list (localify #'evars def-ctx))]
                [stop-list
                 (append
                  (kernel-form-identifier-list (quote-syntax here))
                  (syntax->list #'ivars)
                  (syntax->list #'evars))]
                [definition?
                  (lambda (id)
                    (and (identifier? id)
                         (or (module-identifier=? id (quote-syntax define-values))
                             (module-identifier=? id (quote-syntax define-syntaxes)))))]
                [expanded-body
                 (let expand-all ((defns&exprs (syntax->list #'(body ...))))
                   ;; Also lifted from Matthew, to expand the body enough
                   (apply
                    append
                    (map
                     (lambda (defn-or-expr)
                       (let ([defn-or-expr
                               (local-expand
                                defn-or-expr
                                expand-context
                                stop-list
                                def-ctx)])
                         (syntax-case defn-or-expr (begin define-values define-syntaxes)
                           [(begin . l)
                            (let ([l (parameterize ((error-syntax defn-or-expr))
                                       (checked-syntax->list #'l))])
                              (expand-all (map (lambda (s)
                                                 (syntax-track-origin s defn-or-expr #'begin))
                                               l)))]
                           [(define-syntaxes (id ...) rhs)
                            (andmap identifier? (syntax->list #'(id ...)))
                            (with-syntax ([rhs (local-transformer-expand
                                                #'rhs
                                                'expression
                                                null)])
                              (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
                              (list #'(define-syntaxes (id ...) rhs)))]
                           [(define-values (id ...) rhs)
                            (andmap identifier? (syntax->list #'(id ...)))
                            (begin
                              (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f def-ctx)
                              (list defn-or-expr))]
                           [else (list defn-or-expr)])))
                     defns&exprs)))]
                ;; Get all the defined names, sorting out variable definitions
                ;; from syntax definitions.
                [defined-names-table
                  (let ((table (make-bound-identifier-mapping)))
                    (for-each
                     (lambda (defn-or-expr)
                       (syntax-case defn-or-expr ()
                         ((dv . rest)
                          (definition? #'dv)
                          (begin
                            (check-def-syntax defn-or-expr)
                            (syntax-case #'rest ()
                              [((id ...) expr)
                               (for-each 
                                (lambda (id)
                                  (when (bound-identifier-mapping-get table id (lambda () #f))
                                    (raise-stx-err "variable defined twice" id))
                                  (bound-identifier-mapping-put!
                                   table id 
                                   (make-var-info (module-identifier=? #'dv (quote-syntax define-syntaxes))
                                                  #f
                                                  id)))
                                (syntax->list #'(id ...)))]
                              [_ (void)])))
                         [_ (void)]))
                     expanded-body)
                    table)])
           
           ;; Mark exported names and
           ;; check that all exported names are defined (as var):
           (for-each
            (lambda (name loc)
              (let ([v (bound-identifier-mapping-get defined-names-table
                                                     name
                                                     (lambda () #f))])
                (unless v
                  (raise-stx-err (format "undefined export ~a" (syntax-e name))))
                (when (var-info-syntax? v)
                  (raise-stx-err "cannot export syntax from a unit" name))
                (set-var-info-exported?! v loc)))
            local-evars
            (syntax->list #'elocs))
           
           ;; Check that none of the imports are defined
           (for-each
            (lambda (i)
              (let ((defid (bound-identifier-mapping-get defined-names-table
                                                         i
                                                         (lambda () #f))))
                (when defid
                  (raise-stx-err
                   "definition for imported identifier"
                   (var-info-id defid)))))
            local-ivars)
           
           (with-syntax ([(intname ...)
                          (foldr
                           (lambda (var res)
                             (cond
                               ((not (or (var-info-syntax? (cdr var))
                                         (var-info-exported? (cdr var))))
                                (cons (car var) res))
                               (else res)))
                           null
                           (bound-identifier-mapping-map defined-names-table cons))]
                         [(evar ...) #'evars]
                         [(l-evar ...) local-evars]
                         [(defn&expr ...) 
                          (filter
                           values
                           (map (lambda (defn-or-expr)
                                  (syntax-case defn-or-expr (define-values define-syntaxes)
                                    [(define-values () expr)
                                     (syntax/loc defn-or-expr (set!-values () expr))]
                                    [(define-values ids expr)
                                     (let ([ids (syntax->list #'ids)]
                                           [do-one
                                            (lambda (id tmp name)
                                              (let ([export-loc
                                                     (var-info-exported?
                                                      (bound-identifier-mapping-get
                                                       defined-names-table
                                                       id))])
                                                (cond
                                                  (export-loc
                                                   ;; set! exported id:
                                                   (quasisyntax/loc defn-or-expr
                                                     (set-box! #,export-loc 
                                                               #,(if name
                                                                     #`(let ([#,name #,tmp])
                                                                         #,name)
                                                                     tmp))))
                                                  (else
                                                   ;; not an exported id
                                                   (quasisyntax/loc defn-or-expr
                                                     (set! #,id #,tmp))))))])
                                       (if (null? (cdr ids))
                                           (do-one (car ids) (syntax expr) (car ids))
                                           (let ([tmps (generate-temporaries ids)])
                                             (with-syntax ([(tmp ...) tmps]
                                                           [(set ...)
                                                            (map (lambda (id tmp)
                                                                   (do-one id tmp #f))
                                                                 ids tmps)])
                                               (syntax/loc defn-or-expr
                                                 (let-values ([(tmp ...) expr])
                                                   set ...))))))]
                                    [(define-syntaxes . l) #f]
                                    [else defn-or-expr]))
                                expanded-body))]
                         [(stx-defn ...) 
                          (filter
                           values
                           (map (lambda (defn-or-expr)
                                  (syntax-case defn-or-expr (define-syntaxes)
                                    [(define-syntaxes . l) #'l]
                                    [else #f]))
                                expanded-body))])
             #'(letrec-syntaxes+values (stx-defn
                                        ...
                                        ((l-evar) (make-rename-transformer (quote-syntax evar)))
                                        ...)
                 ([(intname) undefined] ...)
                 (void) ; in case the body would be empty
                 defn&expr ...)))))))

  (define-for-syntax (redirect-imports/exports import?)
    (lambda (table-stx
        import-tagged-infos
        import-sigs
        target-import-tagged-infos
        target-import-sigs)
      (define def-table (make-bound-identifier-mapping))
      (for-each
       (lambda (tagged-info sig)
         (define v
           #`(hash-table-get #,table-stx #,(car (tagged-info->keys tagged-info))))
         (for-each
          (lambda (int/ext-name index)
            (bound-identifier-mapping-put! def-table
                                           (car int/ext-name)
                                           #`(vector-ref #,v #,index)))
          (car sig)
          (iota (length (car sig)))))
       import-tagged-infos
       import-sigs)
      (with-syntax ((((eloc ...) ...) 
                     (map
                      (lambda (target-sig)
                        (map
                         (lambda (target-int/ext-name)
                           (bound-identifier-mapping-get
                            def-table
                            (car target-int/ext-name)
                            (lambda ()
                              (raise-stx-err
                               (format (if import?
                                           "identifier ~a is not present in new imports"
                                           "identifier ~a is not present in old export")
                                       (syntax-e (car target-int/ext-name)))))))
                         (car target-sig)))
                      target-import-sigs))
                    (((export-keys ...) ...) 
                     (map tagged-info->keys target-import-tagged-infos)))
        #`(unit-export ((export-keys ...)
                        (vector-immutable eloc ...)) ...))))
  
  (define-for-syntax redirect-imports (redirect-imports/exports #t))
  (define-for-syntax redirect-exports (redirect-imports/exports #f))
    
    
  ;; build-unit/new-import-export : syntax-object -> 
  ;;             (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit expression that changes the import and export signatures
  ;; of another.  stx must be such that it passes check-unit-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define-for-syntax (build-unit/new-import-export stx)
    (syntax-case stx (import export init-depend)
      (((import i ...)
        (export e ...)
        (init-depend id ...)
        . body)
       
       (let* ([d (syntax->list #'(id ...))]
              [dep-tagged-sigids (map check-tagged-id d)]
              [dep-tagged-siginfos 
               (map tagged-sigid->tagged-siginfo dep-tagged-sigids)])
         (define-values (isig tagged-import-sigs import-tagged-infos 
                              import-tagged-sigids import-sigs)
           (process-unit-import #'(i ...)))
         
         (define-values (esig tagged-export-sigs export-tagged-infos 
                              export-tagged-sigids export-sigs)
           (process-unit-export #'(e ...)))

         (check-duplicate-sigs import-tagged-infos isig dep-tagged-siginfos d)
         
         (check-duplicate-subs export-tagged-infos esig)
         
         (check-unit-ie-sigs import-sigs export-sigs)
                  
         (syntax-case #'body ()
           ((b) (check-link-line-syntax #'b))
           (() (raise-stx-err "missing unit specification"))
           (_ (raise-stx-err "expects a single unit specification")))
         
         (with-syntax (((((orig-e ...) unit-exp orig-i ...)) #'body))
           (define-values (orig-isig orig-tagged-import-sigs orig-import-tagged-infos 
                                orig-import-tagged-sigids orig-import-sigs)
             (process-unit-export #'(orig-i ...)))
           
           (define-values (orig-esig orig-tagged-export-sigs orig-export-tagged-infos 
                                     orig-export-tagged-sigids orig-export-sigs)
             (process-unit-import #'(orig-e ...)))
           (with-syntax ((((dept . depr) ...)
                          (map
                           (lambda (tinfo)
                             (cons (car tinfo)
                                   (syntax-local-introduce (car (siginfo-rtime-ids (cdr tinfo))))))
                           dep-tagged-siginfos))
                         [((import-key ...) ...)
                          (map tagged-info->keys import-tagged-infos)]
                         [((export-key ...) ...)
                          (map tagged-info->keys export-tagged-infos)]
                         [((orig-import-key ...) ...)
                          (map tagged-info->keys orig-import-tagged-infos)]
                         [((orig-export-key ...) ...)
                          (map tagged-info->keys orig-export-tagged-infos)]
                         [(import-name ...)
                          (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                               import-tagged-infos)]
                         [(export-name ...)
                          (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                               export-tagged-infos)]
                         [(orig-import-name ...)
                          (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                               orig-import-tagged-infos)]
                         [(orig-export-name ...)
                          (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                               orig-export-tagged-infos)]
                         [name (syntax-local-infer-name (error-syntax))]
                         [form (syntax-e (stx-car (error-syntax)))])
             (values 
              (quasisyntax/loc (error-syntax)
                (let ([unit-tmp unit-exp])
                  (check-unit unit-tmp 'form)
                  (check-sigs unit-tmp
                              (vector-immutable
                               (cons-immutable 'orig-import-name
                                               (vector-immutable orig-import-key ...)) ...)
                              (vector-immutable 
                               (cons-immutable 'orig-export-name 
                                               (vector-immutable orig-export-key ...)) ...)
                              'form)
                  (make-unit
                   'name
                   (vector-immutable (cons-immutable 'import-name
                                                     (vector-immutable import-key ...)) ...)
                   (vector-immutable (cons-immutable 'export-name 
                                                     (vector-immutable export-key ...)) ...)
                   (list-immutable (cons-immutable 'dept depr) ...)
                   (lambda ()
                     (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                       (values (lambda (import-table)
                                 (unit-fn #,(redirect-imports #'import-table
                                                              import-tagged-infos
                                                              import-sigs
                                                              orig-import-tagged-infos
                                                              orig-import-sigs)))
                               #,(redirect-exports #'export-table
                                                   orig-export-tagged-infos
                                                   orig-export-sigs
                                                   export-tagged-infos
                                                   export-sigs)))))))
              import-tagged-sigids
              export-tagged-sigids
              dep-tagged-sigids)))))))
    
  
  (define-syntax/err-param (unit/new-import-export stx)
    (syntax-case stx ()
      ((_ . x)
       (begin
         (let-values (((u x y z) (build-unit/new-import-export (check-unit-syntax #'x))))
           u)))))

  ;; build-compound-unit : syntax-object  -> 
  ;;                      (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a compound-unit expression.  stx match the return of 
  ;; check-compound-syntax
  ;; The two additional values are the identifiers of the compound-unit's import and export
  ;; signatures
  (define-for-syntax (build-compound-unit stx)
    (define-struct lnkid-record (access-code names ctime-ids rtime-ids source-idx sigid siginfo))
    (define (lnkid-rec->keys t rec)
      (map (lambda (rid) (build-key t rid))
           (lnkid-record-rtime-ids rec)))
    (syntax-case stx ()
      (((import ...)
        (export-lnktag ...)
        (((sub-out ...) sub-exp sub-in-lnktag ...) ...))
       (with-syntax ((((import-tag import-lnkid . import-sigid) ...)
                      (map check-tagged-:-clause (syntax->list #'(import ...))))
                     (((export-tag . export-lnkid) ...)
                      (map check-tagged-id
                           (syntax->list #'(export-lnktag ...))))
                     ((((sub-out-tag sub-out-lnkid . sub-out-sigid) ...) ...)
                      (map (lambda (e) (map check-tagged-:-clause (syntax->list e)))
                           (syntax->list #'((sub-out ...) ...))))
                     ((((sub-in-tag . sub-in-lnkid) ...) ...)
                      (map (lambda (t) (map check-tagged-id (syntax->list t)))
                           (syntax->list #'((sub-in-lnktag ...) ...)))))

         (let ([dup (check-duplicate-identifier 
                     (syntax->list #'(import-lnkid ... sub-out-lnkid ... ...)))])
           (when dup
             (raise-stx-err "duplicate linking identifier definition" dup)))
         
         
         (let ([bt (make-bound-identifier-mapping)])
           (for-each
            (lambda (lnkid)
              (bound-identifier-mapping-put! bt lnkid #t))
            (syntax->list #'(import-lnkid ...)))
           (for-each
            (lambda (lnkid)
              (when (bound-identifier-mapping-get bt lnkid (lambda () #f))
                (raise-stx-err "cannot directly export an import" lnkid)))
            (syntax->list #'(export-lnkid ...))))
         
         
         (let* ([idxs (iota (add1 (length (syntax->list #'(sub-exp ...)))))]
                [sub-export-table-tmps (generate-temporaries #'(sub-exp ...))]
                [link-map
                 (let ((bt (make-bound-identifier-mapping)))
                   (for-each 
                    (lambda (tags lnkids sigids tableid i)
                      (for-each
                       (lambda (tag lnkid sigid)
                         (define siginfo (signature-siginfo (lookup-signature sigid)))
                         (define rtime-ids (map syntax-local-introduce
                                                (siginfo-rtime-ids siginfo)))
                         (bound-identifier-mapping-put!
                          bt
                          lnkid
                          (make-lnkid-record 
                           #`(hash-table-get
                              #,tableid
                              #,(build-key (syntax-e tag) (car rtime-ids)))
                           (siginfo-names siginfo)
                           (siginfo-ctime-ids siginfo)
                           rtime-ids
                           i
                           sigid
                           siginfo)))
                      (syntax->list tags)
                      (syntax->list lnkids)
                      (syntax->list sigids)))
                    (syntax->list #'((import-tag ...) (sub-out-tag ...) ...))
                    (syntax->list #'((import-lnkid ...) (sub-out-lnkid ...) ...))
                    (syntax->list #'((import-sigid ...) (sub-out-sigid ...) ...))
                    (cons #'import-table-id sub-export-table-tmps)
                    idxs)
                   (lambda (id)
                     (bound-identifier-mapping-get
                      bt 
                      id
                      (lambda ()
                        (raise-stx-err "unknown linking identifier" id)))))]
                [link-deps
                 (map
                  (lambda (tags lnkids i)
                    (define ht (make-hash-table 'equal))
                    (for-each
                     (lambda (t l)
                       (define et (syntax-e t))
                       (define el (syntax-e l))
                       (define rec (link-map l))
                       (define forward-dep (>= (lnkid-record-source-idx rec) i))
                       (define import-dep (= 0 (lnkid-record-source-idx rec)))
                       (for-each
                        (lambda (ctime-id rtime-id name)
                          (hash-table-put! ht
                                           (build-key et ctime-id)
                                           (list forward-dep import-dep et rtime-id name el)))
                        (lnkid-record-ctime-ids rec)
                        (lnkid-record-rtime-ids rec)
                        (lnkid-record-names rec)))
                     (syntax->list tags)
                     (syntax->list lnkids))
                    (hash-table-map ht (lambda (x y) y)))
                  (syntax->list #'((sub-in-tag ...) ...))
                  (syntax->list #'((sub-in-lnkid ...) ...))
                  (cdr idxs))])
           
           (check-duplicate-subs 
            (map (lambda (t lid) (cons (syntax-e t)
                                  (lnkid-record-siginfo (link-map lid))))
                 (syntax->list #'(export-tag ...))                 
                 (syntax->list #'(export-lnkid ...)))
            (syntax->list #'(export-lnktag ...)))
           
           (with-syntax (((sub-tmp ...) (generate-temporaries #'(sub-exp ...)))
                         ((sub-export-table-tmp ...) sub-export-table-tmps)
                         (name (syntax-local-infer-name (error-syntax)))
                         (((import-key ...) ...)
                          (map
                           (lambda (t l) 
                             (lnkid-rec->keys (syntax-e t) (link-map l)))
                           (syntax->list #'(import-tag ...))
                           (syntax->list #'(import-lnkid ...))))
                         (((export-key ...) ...)
                          (map
                           (lambda (t l) 
                             (lnkid-rec->keys (syntax-e t) (link-map l)))
                           (syntax->list #'(export-tag ...))
                           (syntax->list #'(export-lnkid ...))))
                         ((import-name ...)
                          (map (lambda (l) (car (lnkid-record-names (link-map l))))
                               (syntax->list #'(import-lnkid ...))))
                         ((export-name ...)
                          (map (lambda (l) (car (lnkid-record-names (link-map l))))
                               (syntax->list #'(export-lnkid ...))))
                         (((((sub-in-key sub-in-code) ...) ...) ...)
                          (map
                           (lambda (stxed-tags lnkids)
                             (define lnkid-recs (map link-map (syntax->list lnkids)))
                             (define tags (map syntax-e (syntax->list stxed-tags)))
                             (define tagged-siginfos 
                               (map
                                (lambda (t l) (cons t (lnkid-record-siginfo l)))
                                tags
                                lnkid-recs))
                             (check-duplicate-subs tagged-siginfos (syntax->list lnkids))
                             (map
                              (lambda (t lr)
                                (with-syntax (((key ...)
                                               (lnkid-rec->keys t lr)))
                                  #`((key #,(lnkid-record-access-code lr)) ...)))
                              tags
                              lnkid-recs))
                           (syntax->list #'((sub-in-tag ...) ...))
                           (syntax->list #'((sub-in-lnkid ...) ...))))
                         ((((sub-out-key ...) ...) ...)
                          (map
                           (lambda (lnkids tags)
                             (map
                              (lambda (l t)
                                (lnkid-rec->keys (syntax-e t) (link-map l)))
                              (syntax->list lnkids)
                              (syntax->list tags)))
                           (syntax->list #'((sub-out-lnkid ...) ...))
                           (syntax->list #'((sub-out-tag ...) ...))))
                         (((export-sigid . export-code) ...)
                          (map (lambda (lnkid)
                                 (define s (link-map lnkid))
                                 (cons (lnkid-record-sigid s)
                                       (lnkid-record-access-code s)))
                               (syntax->list #'(export-lnkid ...))))
                         (form (syntax-e (stx-car (error-syntax))))
                         )

             (with-syntax (((check-sub-exp ...)
                            (map
                             (lambda (stx link-deps)
                               (with-syntax (((sub-exp
                                               sub-tmp
                                               ((sub-in-key ...) ...)
                                               ((sub-out-key ...) ...)
                                               sub-in-lnkid
                                               sub-out-lnkid)
                                              stx))
                                 (with-syntax (((sub-in-signame ...)
                                                (map (lambda (l) (car (lnkid-record-names (link-map l))))
                                                     (syntax->list #'sub-in-lnkid)))
                                               ((sub-out-signame ...)
                                                (map (lambda (l) (car (lnkid-record-names (link-map l))))
                                                     (syntax->list #'sub-out-lnkid)))
                                               (((fdep-tag fdep-rtime fsig-name flnk-name) ...)
                                                (map cddr (filter car link-deps)))
                                               (((rdep-tag rdep-rtime . _) ...)
                                                (map cddr (filter cadr link-deps))))
                                   #`(begin
                                       #,(syntax/loc #'sub-exp
                                           (check-unit sub-tmp 'form))
                                       #,(syntax/loc #'sub-exp
                                           (check-sigs sub-tmp
                                                       (vector-immutable
                                                        (cons-immutable 'sub-in-signame
                                                                        (vector-immutable sub-in-key ...))
                                                        ...)
                                                       (vector-immutable
                                                        (cons-immutable 'sub-out-signame
                                                                        (vector-immutable sub-out-key ...))
                                                        ...)
                                                       'form))
                                       (let ([fht (hash-table 'equal
                                                              ((cons-immutable 'fdep-tag fdep-rtime)
                                                               (cons-immutable 'fsig-name 'flnk-name))
                                                              ...)]
                                             [rht (hash-table 'equal
                                                              ((cons-immutable 'rdep-tag rdep-rtime)
                                                               #t)
                                                              ...)])
                                         #,(syntax/loc #'sub-exp (check-deps fht sub-tmp 'form))
                                         (for-each
                                          (lambda (dep)
                                            (when (hash-table-get rht dep (lambda () #f))
                                              (set! deps (cons dep deps))))
                                          (unit-deps sub-tmp)))))))
                             (syntax->list #'((sub-exp
                                               sub-tmp
                                               ((sub-in-key ...) ...)
                                               ((sub-out-key ...) ...)
                                               (sub-in-lnkid ...)
                                               (sub-out-lnkid ...))
                                              ...))
                             link-deps))
                           (((sub-in-key-code-workaround ...) ...)
                            (map
                             (lambda (x)
                               (with-syntax ((((a ...) ...) x))
                                 #'(a ... ...)))
                             (syntax->list #'((((sub-in-key sub-in-code) ...) ...) ...))))
                           )
               (values
                (quasisyntax/loc (error-syntax)
                  (let ([deps '()]
                        [sub-tmp sub-exp] ...)
                    check-sub-exp ...
                    (make-unit
                     'name
                     (vector-immutable
                      (cons-immutable 'import-name
                                      (vector-immutable import-key ...))
                      ...)
                     (vector-immutable
                      (cons-immutable 'export-name
                                      (vector-immutable export-key ...))
                      ...)
                     deps
                     (lambda ()
                       (let-values ([(sub-tmp sub-export-table-tmp) ((unit-go sub-tmp))]
                                    ...)
                         (values (lambda (import-table-id)
                                   (void)
                                   (sub-tmp (hash-table 'equal sub-in-key-code-workaround ...))
                                   ...)
                                 (unit-export ((export-key ...) export-code) ...)))))))
                (map syntax-e (syntax->list #'((import-tag . import-sigid) ...)))
                (map syntax-e (syntax->list #'((export-tag . export-sigid) ...)))
                '()))))))
      (((i ...) (e ...) (l ...))
       (for-each check-link-line-syntax (syntax->list #'(l ...))))))

  
  (define-syntax/err-param (compound-unit stx)
    (let-values (((u x y z)
                  (build-compound-unit
                   (check-compound-syntax (syntax-case stx () ((_ . x) #'x))))))
      u))

  
  (define (invoke-unit/core unit)
    (check-unit unit 'invoke-unit)
    (check-no-imports unit 'invoke-unit)
    (let-values ([(f exports) ((unit-go unit))])
      (f #f)))
      
  (define-syntax/err-param (define-values/invoke-unit/core stx)
    (syntax-case stx ()
      ((_ unit-expr . unit-out)
       (let* ((unit-out (checked-syntax->list #'unit-out))
              (tagged-out (map process-tagged-import unit-out))
              (out-tags (map car tagged-out))
              (out-sigs (map caddr tagged-out))
              (dup (check-duplicate-identifier (apply append (map sig-int-names out-sigs))))
              (out-vec (generate-temporaries out-sigs)))
         (when dup
           (raise-stx-err (format "duplicate binding for ~e" (syntax-e dup))))
         (with-syntax ((((key1 key ...) ...) (map tagged-info->keys out-tags))
                       ((((int-binding . ext-binding) ...) ...) (map car out-sigs))
                       ((out-vec ...) out-vec)
                       (((renames
                          (((mac-name ...) mac-body) ...) 
                          (((val-name ...) val-body) ...))
                         ...)
                        (map build-val+macro-defs out-sigs))
                       ((out-names ...)
                        (map (lambda (info) (car (siginfo-names (cdr info))))
                             out-tags))
                       (((out-code ...) ...)
                        (map 
                         (lambda (os ov)
                           (map 
                            (lambda (i)
                              #`(vector-ref #,ov #,i))
                            (iota (length (car os)))))
                         out-sigs
                         out-vec)))
           (quasisyntax/loc stx
             (begin
               (define-values (int-binding ... ...)
                 #,(syntax/loc #'unit-expr
                     (let ((unit-tmp unit-expr))
                       (check-unit unit-tmp 'define-values/invoke-unit)
                       (check-sigs unit-tmp
                                   (vector-immutable)
                                   (vector-immutable (cons 'out-names
                                                           (vector-immutable key1 key ...)) ...)
                                   'define-values/invoke-unit)
                       (let-values (((unit-fn export-table)
                                     ((unit-go unit-tmp))))
                         (let ([out-vec (hash-table-get export-table key1)] ...)
                           (unit-fn #f)
                           (values (unbox out-code) ... ...))))))
               (define-syntaxes . renames) ...
               (define-syntaxes (mac-name ...) mac-body) ... ...
               (define-values (val-name ...) val-body) ... ...)))))
      ((_)
       (raise-stx-err "missing unit expression"))))

  ;; build-unit-from-context : syntax-object -> 
  ;;                           (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit-from-context expression.  stx must be
  ;; such that it passes check-ufc-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define-for-syntax (build-unit-from-context stx)
    (syntax-case stx ()
      ((export-spec)
       (let* ((tagged-export-sig (process-tagged-export #'export-spec))
              (export-sig (caddr tagged-export-sig)))
         (with-syntax ((((int-id . ext-id) ...) (car export-sig))
                       ((def-name ...) (generate-temporaries (map car (car export-sig)))))
           (values
            #'(:unit (import) (export (rename export-spec (def-name int-id) ...))
                     (define def-name int-id)
                     ...)
            null
            (list (cadr tagged-export-sig))
            '()))))))
  
  (define-for-syntax (check-ufc-syntax stx)
    (syntax-case stx ()
      ((export-spec) (void))
      (()
       (raise-stx-err "missing export-spec"))
      (_
       (raise-stx-err "nothing is permitted after export-spec"))))
    
  (define-syntax/err-param (unit-from-context stx)
    (syntax-case stx ()
      ((_ . x)
       (begin
         (check-ufc-syntax #'x)
         (let-values (((u x y z) (build-unit-from-context #'x)))
           u)))))

  
  
  ;; build-define-unit : syntax-object
  ;;                     (syntax-object -> (values syntax-object (listof identifier) (listof identifier))
  ;;                     string ->
  ;;                     syntax-object
  (define-for-syntax (build-define-unit stx build err-msg)
    (syntax-case stx ()
      ((_ name . rest)
       (begin
         (check-id #'name)
         (let-values (((exp i e d) (build #'rest)))
           (with-syntax ((((itag . isig) ...) i)
                         (((etag . esig) ...) e)
                         (((deptag . depsig) ...) d))
             (quasisyntax/loc (error-syntax)
               (begin
                 (define u #,exp)
                 (define-syntax name
                   (make-set!-transformer
                    (make-unit-info ((syntax-local-certifier) (quote-syntax u))
                                    (list (cons 'itag (quote-syntax isig)) ...)
                                    (list (cons 'etag (quote-syntax esig)) ...)
                                    (list (cons 'deptag (quote-syntax deptag)) ...))))))))))
      ((_)
       (raise-stx-err err-msg))))

  (define-for-syntax (build-define-unit-binding stx)
    
    (define (check-helper tagged-info)
      (cons (car (siginfo-names (cdr tagged-info)))
            (tagged-info->keys tagged-info)))
    
    (syntax-case stx (import export init-depend)
      ((unit-exp (import i ...) (export e ...) (init-depend idep ...))
       (let* ([ti (syntax->list #'(i ...))]
              [te (syntax->list #'(e ...))]
              [tidep (syntax->list #'(idep ...))]
              [tagged-import-sigids (map check-tagged-id ti)]
              [tagged-export-sigids (map check-tagged-id te)]
              [tagged-dep-sigids (map check-tagged-id tidep)]
              [tagged-import-infos (map tagged-sigid->tagged-siginfo tagged-import-sigids)]
              [tagged-export-infos (map tagged-sigid->tagged-siginfo tagged-export-sigids)]
              [tagged-dep-siginfos (map tagged-sigid->tagged-siginfo tagged-dep-sigids)])
         (check-duplicate-sigs tagged-import-infos ti tagged-dep-siginfos tidep)         
         (check-duplicate-subs tagged-export-infos te)
         (with-syntax ((((import-name . (import-keys ...)) ...)
                        (map check-helper tagged-import-infos))
                       (((export-name . (export-keys ...)) ...)
                        (map check-helper tagged-export-infos))
                       (form (stx-car (error-syntax))))
           (values
            #`(let ([unit-tmp unit-exp])
                #,(syntax/loc #'unit-exp
                    (check-unit unit-tmp 'form))
                #,(syntax/loc #'unit-exp
                    (check-sigs unit-tmp 
                                (vector-immutable
                                 (cons-immutable 'import-name
                                                 (vector-immutable import-keys ...))
                                 ...)
                                (vector-immutable
                                 (cons-immutable 'export-name
                                                 (vector-immutable export-keys ...))
                                 ...)
                                'form))
                unit-tmp)
            tagged-import-sigids
            tagged-export-sigids
            tagged-dep-sigids))))))
 
  (define-syntax/err-param (define-unit-binding stx)
    (build-define-unit stx  (lambda (unit)
                              (build-define-unit-binding (check-unit-body-syntax unit)))
      "missing unit name, unit expression, import clause, and export clause"))
  
  (define-syntax/err-param (define-unit stx)
    (build-define-unit stx (lambda (unit)
                             (build-unit (check-unit-syntax unit)))
      "missing unit name, import clause, and export clause"))
  
  (define-syntax/err-param (define-unit/new-import-export stx)
    (build-define-unit stx (lambda (unit)
                             (build-unit/new-import-export (check-unit-syntax unit)))
      "missing unit name, import clause, and export clause"))
  
  (define-syntax/err-param (define-compound-unit stx)
    (build-define-unit stx (lambda (clauses)
                             (build-compound-unit (check-compound-syntax clauses)))
      "missing unit name"))
  
  (define-syntax/err-param (define-unit-from-context stx)
    (build-define-unit stx (lambda (sig)
                             (check-ufc-syntax sig)
                             (build-unit-from-context sig))
      "missing unit name and signature"))

  (define-for-syntax (unprocess-tagged-id ti)
    (if (car ti)
        #`(tag #,(car ti) #,(cdr ti))
        (cdr ti)))

  (define-syntax/err-param (define-values/invoke-unit/infer stx)
    (syntax-case stx ()
      ((_ u) 
       (let ((ui (lookup-def-unit #'u)))
         (with-syntax (((sig ...) (map unprocess-tagged-id (unit-info-export-sig-ids ui)))
                       ((isig ...) (map unprocess-tagged-id (unit-info-import-sig-ids ui))))
           (quasisyntax/loc stx
             (define-values/invoke-unit u (import isig ...) (export sig ...))))))
      ((_)
       (raise-stx-err "missing unit" stx))
      ((_ . b)
       (raise-stx-err
        (format "expected syntax matching (~a <define-unit-identifier>)"
                (syntax-e (stx-car stx)))))))
  
  (define-for-syntax (temp-id-with-tags id i)
    (syntax-case i (tag)
      [(tag t sig)
       (list id #`(tag t #,id) #'sig)]
      [_else
       (list id id i)]))

  (define-syntax/err-param (define-values/invoke-unit stx)
    (syntax-case stx (import export)
      ((_ u (import) (export e ...))
       (quasisyntax/loc stx
         (define-values/invoke-unit/core u e ...)))
      ((_ u (import i ...) (export e ...))
       (with-syntax (((EU ...) (generate-temporaries #'(e ...)))
                     (((IU IUl i) ...) (map temp-id-with-tags
                                            (generate-temporaries #'(i ...))
                                            (syntax->list #'(i ...))))
                     ((iu ...) (generate-temporaries #'(i ...)))
                     ((i-id ...) (map cdadr
                                      (map process-tagged-import
                                           (syntax->list #'(i ...)))))
                     ((e-id ...) (map cdadr 
                                      (map process-tagged-export
                                           (syntax->list #'(e ...))))))
         (quasisyntax/loc stx
           (begin
             (define-unit-from-context iu i)
             ...
             (define-compound-unit u2 (import)
               (export EU ...)
               (link [((IU : i-id)) iu] ... [((EU : e-id) ...) u IUl ...]))
             (define-values/invoke-unit/core u2 e ...)))))
      ((_)
       (raise-stx-err "missing unit" stx))
      ((_ . b)
       (raise-stx-err
        (format "expected syntax matching (~a <unit-expression> (import <sig-expr> ...) (export <sig-expr> ...))"
                (syntax-e (stx-car stx)))))))
  
  ;; build-compound-unit/infer : syntax-object  -> 
  ;;                      (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a compound-unit/infer expression.  stx match the return of 
  ;; check-compound-syntax
  ;; The two additional values are the identifiers of the compound-unit's import and export
  ;; signatures
  (define-for-syntax (build-compound-unit/infer stx)
    
    (define (lookup-tagged tid)
      (cons (car tid) (lookup-signature (cdr tid))))
    
    (define (process-signature s)
      (define l
        ((check-tagged 
          (lambda (b)
            (syntax-case* b (:) (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
              ((x : y)
               (and (identifier? #'x) (identifier? #'y))
               (list #'x #'y (signature-siginfo (lookup-signature #'y))))
              (x
               (identifier? #'x)
               (list (car (generate-temporaries (list #'x)))
                     #'x
                     (signature-siginfo (lookup-signature #'x))))
              (_
               (raise-stx-err "expected syntax matching <identifier> or (<identifier> : <identifier>)"
                              b)))))
         s))
      (apply make-link-record l))
    
    (define (process-tagged-sigid sid)
      (make-link-record (car sid) #f (cdr sid) (signature-siginfo (lookup-signature (cdr sid)))))
      
    (syntax-case stx ()
      (((import ...) 
        (export ...)
        (((out ...) u l ...) ...))
       (let* ([units (map lookup-def-unit (syntax->list #'(u ...)))]
              [import-sigs (map process-signature 
                                (syntax->list #'(import ...)))]
              [sub-outs
               (map
                (lambda (outs unit)
                  (define o
                    (map
                     (lambda (clause)
                       (define c (check-tagged-:-clause clause))
                       (make-link-record (car c) (cadr c) (cddr c)
                                         (signature-siginfo (lookup-signature (cddr c)))))
                     (syntax->list outs)))
                  (complete-exports (map process-tagged-sigid (unit-info-export-sig-ids unit))
                                    o))
                (syntax->list #'((out ...) ...))
                units)]
              [link-defs (append import-sigs (apply append sub-outs))])

         (define lnk-table (make-bound-identifier-mapping))
         (define sig-table (make-hash-table))

         (let ([dup (check-duplicate-identifier (map link-record-linkid link-defs))])
           (when dup
             (raise-stx-err "duplicate identifier" dup)))

         (for-each
          (lambda (b)
            (bound-identifier-mapping-put! lnk-table (link-record-linkid b) b))
          link-defs)

         (for-each
          (lambda (b)
            (for-each
             (lambda (cid)
               (define there? (hash-table-get sig-table cid (lambda () #f)))
               (hash-table-put! sig-table cid (if there? 'duplicate (link-record-linkid b))))
             (siginfo-ctime-ids (link-record-siginfo b))))
          link-defs)
             
         (let ([sub-ins
                (map
                 (lambda (ins unit unit-stx)
                   (define is (syntax->list ins))
                   (define lrs
                     (map
                      (lambda (i)
                        (define tagged-lnkid (check-tagged-id i))
                        (define sig
                          (bound-identifier-mapping-get lnk-table
                                                        (cdr tagged-lnkid)
                                                        (lambda () #f)))
                        (unless sig
                          (raise-stx-err "unknown linking identifier" i))
                        (make-link-record (car tagged-lnkid)
                                          (cdr tagged-lnkid)
                                          (link-record-sigid sig)
                                          (link-record-siginfo sig)))
                      is))
                   (check-duplicate-subs
                    (map 
                     (lambda (lr) (cons (link-record-tag lr) (link-record-siginfo lr)))
                     lrs)
                    is)
                   (complete-imports sig-table 
                                     lrs
                                     (map process-tagged-sigid
                                          (unit-info-import-sig-ids unit))
                                     unit-stx))
                 (syntax->list #'((l ...) ...))
                 units
                 (syntax->list #'(u ...)))]
               [exports
                (map 
                 (lambda (e)
                   (define tid (check-tagged-id e))
                   (define lookup (bound-identifier-mapping-get 
                                   lnk-table
                                   (cdr tid)
                                   (lambda () #f)))
                   (cond
                     [lookup (unprocess-tagged-id tid)]
                     [else
                      (let ([lnkid (hash-table-get
                                    sig-table
                                    (car (siginfo-ctime-ids (signature-siginfo (lookup-signature (cdr tid)))))
                                    (lambda () #f))])
                        (cond
                          [(not lnkid)
                           (raise-stx-err "no sub unit exports this signature" (cdr tid))]
                          [(eq? lnkid 'duplicate)
                           (raise-stx-err "multiple sub units export this signature" (cdr tid))]
                          [else 
                           (unprocess-tagged-id
                            (cons (car tid) lnkid))]))]))
                 (syntax->list #'(export ...)))])
           (with-syntax (((import ...)
                          (map unprocess-link-record-bind import-sigs))
                         (((out ...) ...)
                          (map
                           (lambda (out) 
                             (map unprocess-link-record-bind out))
                           sub-outs))
                         (((in ...) ...)
                          (map
                           (lambda (ins)
                             (map unprocess-link-record-use ins))
                           sub-ins))
                         ((unit-id ...) (map 
                                         (lambda (u stx)
                                           (quasisyntax/loc stx #,(unit-info-unit-id u)))
                                         units (syntax->list #'(u ...)))))
             (build-compound-unit #`((import ...)
                                     #,exports
                                     (((out ...) unit-id in ...) ...)))))))
      (((i ...) (e ...) (l ...))
       (for-each check-link-line-syntax (syntax->list #'(l ...))))))


  (define-for-syntax (check-compound/infer-syntax stx)
    (syntax-case (check-compound-syntax stx) ()
      ((i e (b ...))
       (with-syntax (((b ...)
                      (map
                       (lambda (b)
                         (if (identifier? b)
                             #`(() #,b)
                             b))
                       (syntax->list #'(b ...)))))
         #'(i e (b ...))))))
  
  (define-syntax/err-param (compound-unit/infer stx)
    (let-values (((u i e d)
                  (build-compound-unit/infer
                   (check-compound/infer-syntax 
                    (syntax-case stx () ((_ . x) #'x))))))
      u))
                  
  (define-for-syntax (do-define-compound-unit/infer stx)
    (build-define-unit stx 
                       (lambda (clause)
                         (build-compound-unit/infer (check-compound/infer-syntax clause)))
                       "missing unit name"))

  (define-syntax/err-param (define-compound-unit/infer stx)
    (do-define-compound-unit/infer stx))

  (define-syntax/err-param (invoke-unit stx)
    (syntax-case stx (import)
      ((_ unit)
       (syntax/loc stx
         (invoke-unit/core unit)))
      ((_ unit (import isig ...))
       (with-syntax (((u ...) (generate-temporaries (syntax->list #'(isig ...))))
                     (((U Ul isig) ...) (map temp-id-with-tags
                                             (generate-temporaries #'(isig ...))
                                             (syntax->list #'(isig ...))))
                     ((isig-id ...) (map cdadr
                                         (map process-tagged-import
                                              (syntax->list #'(isig ...))))))
         (syntax/loc stx
           (let ()
             (define-unit-from-context u isig)
             ...
             (define-compound-unit u2 (import) (export)
               (link [((U : isig-id)) u] ... [() unit Ul ...]))
             (invoke-unit/core u2)))))
      (_ (raise-stx-err (format
                         "expected (~a <expr>) or (~a <expr> (import <sig-expr> ...))"
                         (syntax-e (stx-car stx))
                         (syntax-e (stx-car stx)))))))
  
  (define-syntax/err-param (invoke-unit/infer stx)
    (syntax-case stx ()
      ((_ u) 
       (let ((ui (lookup-def-unit #'u)))
         (with-syntax (((isig ...) (map unprocess-tagged-id
                                        (unit-info-import-sig-ids ui))))
           (quasisyntax/loc stx
             (invoke-unit u (import isig ...))))))
      ((_)
       (raise-stx-err "missing unit" stx))
      ((_ . b)
       (raise-stx-err
        (format "expected syntax matching (~a <define-unit-identifier>)"
                (syntax-e (stx-car stx)))))))
  
  )
;(load "test-unit.ss")
