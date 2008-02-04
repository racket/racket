(module reqprov '#%kernel
  (#%require "more-scheme.ss" "small-scheme.ss" "define.ss" "../stxparam.ss"
             (for-syntax '#%kernel "define.ss"
                         "stx.ss" "stxcase-scheme.ss" "small-scheme.ss" 
                         "stxloc.ss" "qqstx.ss"
                         "../require-transform.ss"
                         "../provide-transform.ss"
                         "struct-info.ss"))
  
  (#%provide lib file planet
             for-syntax for-template for-label
             require
             only-in rename-in prefix-in except-in
             provide
             all-defined-out all-from-out
             rename-out except-out prefix-out struct-out
             protect-out)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; helpers

  (define-for-syntax (filter pred l)
    (cond
     [(null? l) null]
     [(pred (car l)) (cons (car l) (filter pred (cdr l)))]
     [else (filter pred (cdr l))]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lib
  
  (define-for-syntax (xlate-path stx)
    (if (pair? (syntax-e stx))
        (let ([kw
               ;; free-identifier=? identifers are not necessarily module=?
               (syntax-case stx (lib planet file quote)
                 [(quote . _) 'quote]
                 [(lib . _) 'lib]
                 [(planet . _) 'planet]
                 [(file . _) 'file])]
              [d (syntax->datum stx)])
          (if (eq? (car d) kw)
              stx
              (datum->syntax
               stx
               (cons kw (cdr d))
               stx
               stx)))
        stx))
  
  (define-for-syntax (check-lib-form stx)
    (unless (module-path? (syntax->datum (xlate-path stx)))
      (raise-syntax-error
       #f
       "ill-formed module path"
       stx)))

  (define-syntaxes (lib file planet)
    (let ([t (lambda (stx)
               (check-lib-form stx)
               (let*-values ([(mod-path) (syntax->datum stx)]
                             [(names et-names lt-names) (syntax-local-module-exports stx)])
                 (values
                  (apply
                   append
                   (map (lambda (names mode)
                          (map (lambda (name)
                                 (make-import (datum->syntax
                                               stx
                                               name
                                               stx)
                                              name
                                              mod-path
                                              mode
                                              'run
                                              stx))
                               names))
                        (list names et-names lt-names)
                        (list 'run 'syntax 'label)))
                  (list (make-import-source stx 'run)))))])
      (let ([t2
             (let-values ([(s: mk s? s-ref s-set!)
                           (make-struct-type 'req+prov
                                             #f
                                             0 0 #f
                                             (list 
                                              (cons prop:require-transformer (lambda (a) t)))
                                             (current-inspector)
                                             (lambda (p stx)
                                               (raise-syntax-error
                                                #f
                                                "misuse of module-path constructor (not within, e.g., `require' or `provide')"
                                                stx)))])
               (mk))])
        (values t2 t2 t2))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; for-syntax, for-template, for-label

  (define-for-syntax (shift-subs stx mode)
    (syntax-case stx ()
      [(_ in ...)
       (let* ([imports+sourcess
               (map (lambda (in)
                      (let-values ([(imports sources) (expand-import in)])
                        (cons imports sources)))
                    (syntax->list #'(in ...)))]
              [imports (apply append (map car imports+sourcess))]
              [sources (apply append (map cdr imports+sourcess))])
         (values (map (lambda (import)
                        (make-import (import-local-id import)
                                     (import-src-sym import)
                                     (import-src-mod-path import)
                                     mode
                                     mode
                                     (import-orig-stx import)))
                      (filter (lambda (import)
                                (eq? (import-mode import) 'run))
                              imports))
                 (map (lambda (source)
                        (make-import-source (import-source-mod-path-stx source)
                                            mode))
                      (filter (lambda (source)
                                (eq? (import-source-mode source) 'run))
                              sources))))]))

  (define-for-syntax (make-require+provide-transformer r p)
    (let-values ([(s: mk s? s-ref s-set!)
                  (make-struct-type 'req+prov
                                    #f
                                    0 0 #f
                                    (list 
                                     (cons prop:require-transformer (lambda (a) r))
                                     (cons prop:provide-transformer (lambda (a) p))))])
      (mk)))

  (define-for-syntax (exports-at-phase stx modes mode)
    (if (not (null? modes))
        (raise-syntax-error
         #f
         "nested phases specification not allowed"
         stx)
        (syntax-case stx ()
          [(_ ex ...)
           (apply append
                  (map (lambda (ex)
                         (expand-export ex (list mode)))
                       (syntax->list #'(ex ...))))])))

  (define-syntax for-syntax
    (make-require+provide-transformer
     (lambda (stx)
       (shift-subs stx 'syntax))
     (lambda (stx modes)
       (exports-at-phase stx modes 'syntax))))
  
  (define-syntax for-template
    (make-require-transformer
     (lambda (stx)
       (shift-subs stx 'template))))
  
  (define-syntax for-label
    (make-require+provide-transformer
     (lambda (stx)
       (shift-subs stx 'label))
     (lambda (stx modes)
       (exports-at-phase stx modes 'label))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; require
  
  (define-syntax (require stx)
    (unless (memq (syntax-local-context) '(module module-begin top-level))
      (raise-syntax-error #f
                          "not at module level or top level"
                          stx))
    (letrec ([mode-wrap
              (lambda (mode base)
                (cond
                  [(eq? mode 'run) base]
                  [(eq? mode 'syntax) #`(for-syntax #,base)]
                  [(eq? mode 'template) #`(for-template #,base)]
                  [(eq? mode 'label) #`(for-label #,base)]
                  [else (error "huh?" mode)]))]
             [simple-path? (lambda (p)
                             (syntax-case p (lib quote)
                               [(lib . _)
                                (check-lib-form p)]
                               [(quote . _)
                                (check-lib-form p)]
                               [_
                                (or (identifier? p)
                                    (and (string? (syntax-e p))
                                         (module-path? (syntax-e p))))]))]
             [transform-simple
              (lambda (in base-mode)
                (syntax-case in (lib file planet prefix-in except-in quote)
                  ;; Detect simple cases first:
                  [_ 
                   (string? (syntax-e in))
                   (begin
                     (unless (module-path? (syntax-e in))
                       (raise-syntax-error
                        #f
                        "bad module-path string"
                        stx
                        in))
                     (list (mode-wrap base-mode in)))]
                  [_
                   (and (identifier? in)
                        (module-path? (syntax-e #'in)))
                   (list (mode-wrap base-mode in))]
                  [(quote . s)
                   (check-lib-form in)
                   (list (mode-wrap base-mode (xlate-path in)))]
                  [(lib . s)
                   (check-lib-form in)
                   (list (mode-wrap base-mode (xlate-path in)))]
                  [(file . s)
                   (check-lib-form in)
                   (list (mode-wrap base-mode (xlate-path in)))]
                  [(planet . s)
                   (check-lib-form in)
                   (list (mode-wrap base-mode (xlate-path in)))]
                  [(prefix-in pfx path)
                   (simple-path? #'path)
                   (list (mode-wrap
                          base-mode
                          (datum->syntax
                           #'path
                           (syntax-e
                            (quasisyntax
                             (prefix pfx #,(xlate-path #'path))))
                           in
                           in)))]
                  [(except-in path id ...)
                   (and (simple-path? #'path)
                        ;; check that it's well-formed...
                        (call-with-values (lambda () (expand-import in))
                          (lambda (a b) #t)))
                   (list (mode-wrap
                          base-mode
                          (datum->syntax
                           #'path
                           (syntax-e
                            (quasisyntax/loc in
                              (all-except #,(xlate-path #'path) id ...))))))]
                  ;; General case:
                  [_ (let-values ([(imports sources) (expand-import in)])
                       ;; TODO: collapse back to simple cases when possible
                       (append
                        (map (lambda (import)
                               (mode-wrap (if (eq? base-mode 'run)
                                              (import-req-mode import)
                                              base-mode)
                                          #`(rename #,(import-src-mod-path import)
                                                    #,(import-local-id import)
                                                    #,(import-src-sym import))))
                             (if (eq? base-mode 'run)
                                 imports
                                 (filter (lambda (import)
                                           (eq? (import-mode import) 'run))
                                         imports)))
                        (map (lambda (src)
                               (mode-wrap (if (eq? base-mode 'run)
                                              (import-source-mode src)
                                              base-mode)
                                          #`(only #,(import-source-mod-path-stx src))))
                             (if (eq? base-mode 'run)
                                 sources
                                 (filter (lambda (source)
                                           (eq? (import-source-mode source) 'run))
                                         sources)))))]))]
             [transform-one
              (lambda (in)
                ;; Recognize `for-syntax', etc. for simple cases:
                (syntax-case in ()
                  [(for-something elem ...)
                   (and (identifier? #'for-something)
                        (ormap (lambda (i) (free-identifier=? i #'for-something))
                               (list #'for-syntax #'for-template #'for-label)))
                   (apply append
                          (map (lambda (in)
                                 (transform-simple in
                                                   (cond
                                                    [(free-identifier=? #'for-something #'for-syntax)
                                                     'syntax]
                                                    [(free-identifier=? #'for-something #'for-template)
                                                     'template]
                                                    [(free-identifier=? #'for-something #'for-label)
                                                     'label])))
                               (syntax->list #'(elem ...))))]
                  [_ (transform-simple in 'run)]))])
      (syntax-case stx ()
        [(_ in ...)
         (with-syntax ([(new-in ...)
                        (apply append
                               (map transform-one (syntax->list #'(in ...))))])
           (syntax/loc stx
             (#%require new-in ...)))])))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; require transformers

  (define-syntax only-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ in id ...)
          (let-values ([(imports sources) (expand-import #'in)]
                       [(ids) (syntax->list #'(id ...))])
            (for-each (lambda (id)
                        (unless (or (identifier? id)
                                    (let ([l (syntax->list id)])
                                      (and l
                                           (= 2 (length l))
                                           (identifier? (car l))
                                           (identifier? (cadr l)))))
                          (raise-syntax-error
                           #f
                           "expected <id> or [<id> <id>], but found something else"
                           stx
                           id)))
                      ids)
            (let ([orig-ids (map (lambda (id)
                                   (if (identifier? id)
                                       id
                                       (car (syntax-e id))))
                                 ids)]
                  [new-ids (map (lambda (id)
                                  (if (identifier? id)
                                      id
                                      (cadr (syntax->list id))))
                                ids)])
              (let ([dup-id (check-duplicate-identifier new-ids)])
                (when dup-id
                  (raise-syntax-error
                   #f
                   "duplicate identifier"
                   stx
                   dup-id)))
              (values
               (map (lambda (new-id orig-id)
                      (or (ormap (lambda (import)
                                   (and (free-identifier=? orig-id
                                                             (import-local-id import))
                                        (if (eq? new-id orig-id)
                                            import
                                            (make-import new-id
                                                         (import-src-sym import)
                                                         (import-src-mod-path import)
                                                         (import-mode import)
                                                         (import-req-mode import)
                                                         new-id))))
                                 imports)
                          (raise-syntax-error
                           #f
                           (format "identifier `~a' not included in nested require spec"
                                   (syntax-e orig-id))
                           stx
                           #'in)))
                    new-ids orig-ids)
               sources)))]))))

  (define-syntax except-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ in id ...)
          (let-values ([(imports sources) (expand-import #'in)]
                       [(ids) (syntax->list #'(id ...))])
            (for-each (lambda (id)
                        (unless (identifier? id)
                          (raise-syntax-error
                           #f
                           "expected <id>, but found something else"
                           stx
                           id)))
                      ids)
            (let ([dup-id (check-duplicate-identifier ids)])
              (when dup-id
                (raise-syntax-error
                 #f
                 "duplicate identifier"
                 stx
                 dup-id)))
            (for-each (lambda (id)
                        (or (ormap (lambda (import)
                                     (free-identifier=? id (import-local-id import)))
                                   imports)
                            (raise-syntax-error
                             #f
                             (format "identifier `~a' not included in nested require spec"
                                     (syntax-e id))
                             stx
                             #'in)))
                      ids)
            (values
             (filter (lambda (import)
                       (not (ormap (lambda (id)
                                     (free-identifier=? id (import-local-id import)))
                                   ids)))
                     imports)
             sources))]))))

  (define-syntax rename-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ in [orig-id bind-id] ...)
          (let-values ([(imports sources) (expand-import #'in)]
                       [(orig-ids) (syntax->list #'(orig-id ...))]
                       [(bind-ids) (syntax->list #'(bind-id ...))])
            (for-each (lambda (id)
                        (unless (identifier? id)
                          (raise-syntax-error
                           #f
                           "expected an identifier, but found something else"
                           stx
                           id)))
                      (append orig-ids bind-ids))
            (let ([dup-id (check-duplicate-identifier bind-ids)])
              (when dup-id
                (raise-syntax-error
                 #f
                 "duplicate identifier"
                 stx
                 dup-id)))
            (let ([new+olds
                   (map (lambda (orig-id bind-id)
                          (let ([import (ormap (lambda (import)
                                                 (and (free-identifier=? orig-id
                                                                           (import-local-id import))
                                                      import))
                                               imports)])
                            (unless import
                              (raise-syntax-error
                               #f
                               (format "identifier `~a' not included in nested require spec"
                                       (syntax-e orig-id))
                               stx
                               #'in))
                            (cons (make-import bind-id
                                               (import-src-sym import)
                                               (import-src-mod-path import)
                                               (import-mode import)
                                               (import-req-mode import)
                                               bind-id)
                                  import)))
                        orig-ids bind-ids)])
              (let ([leftover-imports 
                     (let ([ht (make-immutable-hash-table
                                (map (lambda (v) (cons (cdr v) #f))
                                     new+olds))])
                       (filter (lambda (i) (hash-table-get ht i #t)) imports))])
                ;; Make sure no new name is in the leftover set:
                (for-each (lambda (bind-id)
                            (when (ormap (lambda (import)
                                           (and (free-identifier=? bind-id
                                                                     (import-local-id import))
                                                import))
                                         leftover-imports)
                              (raise-syntax-error
                               #f
                               (format "identifier `~a' already in nested require spec"
                                       (syntax-e bind-id))
                               stx
                               #'in)))
                          bind-ids)
                (values
                 (append
                  (map car new+olds)
                  leftover-imports)
                 sources))))]))))
  
  (define-syntax prefix-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ pfx in)
          (let-values ([(imports sources) (expand-import #'in)]
                       [(pfx) #'pfx])
            (unless (identifier? #'pfx)
              (raise-syntax-error
               #f
               "expected an <id> for prefix, found something else"
               stx
               #'pfx))
            (values
             (map (lambda (import)
                    (let ([id (import-local-id import)])
                      (make-import (datum->syntax
                                    id
                                    (string->symbol
                                     (format "~a~a" 
                                             (syntax-e pfx)
                                             (syntax-e id)))
                                    id
                                    id)
                                   (import-src-sym import)
                                   (import-src-mod-path import)
                                   (import-mode import)
                                   (import-req-mode import)
                                   (import-orig-stx import))))
                  imports)
             sources))]))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; provide
  
  (define-syntax (provide stx)
    (unless (memq (syntax-local-context) '(module module-begin))
      (raise-syntax-error #f
                          "not at module level"
                          stx))
    
    (syntax-case stx ()
      [(_ out ...)
       (syntax-property
        (quasisyntax/loc stx 
          (#%provide #,(syntax-property
                        #`(expand (provide-trampoline out ...))
                        'certify-mode 'transparent)))
        'certify-mode 'transparent)]))
  
  (define-syntax (provide-trampoline stx)
    (syntax-case stx ()
      [(_ out ...)
       (letrec ([transform-simple
                 (lambda (out)
                   (let ([exports (expand-export out null)])
                     (map (lambda (export)
                            (let ([base
                                   (if (eq? (syntax-e (export-local-id export))
                                            (export-out-sym export))
                                       (export-local-id export)
                                       #`(rename #,(export-local-id export)
                                                 #,(export-out-sym export)))]
                                  [mode (export-mode export)])
                              (let ([phased
                                     (cond
                                      [(eq? mode 'run) base]
                                      [(eq? mode 'syntax) #`(for-syntax #,base)]
                                      [(eq? mode 'label) #`(for-label #,base)])])
                                (if (export-protect? export)
                                    #`(protect #,phased)
                                    phased))))
                          exports)))])
         (syntax-case stx ()
           [(_ out ...)
            (with-syntax ([(new-out ...)
                           (apply append
                                  (map transform-simple (syntax->list #'(out ...))))])
              (syntax/loc stx
                (begin new-out ...)))]))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; provide transformers

  (define-syntax all-defined-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_) 
          (let-values ([(ids stx-ids) (syntax-local-module-defined-identifiers)]
                       [(same-ctx?) (lambda (free-identifier=?)
                                      (lambda (id)
                                        (free-identifier=? id
                                                           (datum->syntax
                                                            stx
                                                            (syntax-e id)))))])
            (append
             (if (memq 'syntax modes)
                 (map (lambda (id)
                        (make-export id (syntax-e id) 'syntax #f stx))
                      (filter (same-ctx? free-transformer-identifier=?)
                              stx-ids))
                 null)
             (if (or (null? modes)
                     (memq 'run modes))
                 (map (lambda (id)
                        (make-export id (syntax-e id) 'run #f stx))
                      (filter (same-ctx? free-identifier=?)
                              ids))
                 null)))]))))

  (define-syntax all-from-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ mp ...)
          (apply
           append
           (map
            (lambda (mp)
              (unless (module-path? (syntax->datum mp))
                (raise-syntax-error
                 #f
                 "bad module path"
                 stx
                 mp))
              (let-values ([(ids stx-ids label-ids) 
                            (syntax-local-module-required-identifiers (syntax->datum mp) 
                                                                      (or (null? modes)
                                                                          (memq 'run modes))
                                                                      (memq 'syntax modes)
                                                                      (memq 'label modes))]
                           [(ok-context?) (lambda (id id=?)
                                            (id=? id
                                                  (datum->syntax mp (syntax-e id))))]) 
                (when (or (null? modes) 
                          (memq 'run modes))
                  (unless ids
                    (raise-syntax-error
                     #f
                     "no corresponding require"
                     stx
                     mp)))
                (when (memq 'syntax modes)
                  (unless stx-ids
                    (raise-syntax-error
                     #f
                     "no corresponding for-syntax require"
                     stx
                     mp)))
                (when (memq 'label modes)
                  (unless label-ids
                    (raise-syntax-error
                     #f
                     "no corresponding for-label require"
                     stx
                     mp)))
                (filter
                 values
                 (append
                  (map (lambda (id)
                         (and (ok-context? id free-transformer-identifier=?)
                              (make-export id (syntax-e id) 'syntax #f stx)))
                       (if (or (null? modes)
                               (memq 'syntax modes))
                           (or stx-ids null)
                           null))
                  (map (lambda (id)
                         (and (ok-context? id free-label-identifier=?)
                              (make-export id (syntax-e id) 'label #f stx)))
                       (if (or (null? modes)
                               (memq 'label modes))
                           (or label-ids null)
                           null))
                  (map (lambda (id)
                         (and (ok-context? id free-identifier=?)
                              (make-export id (syntax-e id) 'run #f stx)))
                       (if (or (null? modes)
                               (memq 'run modes))
                           ids
                           null))))))
            (syntax->list #'(mp ...))))]))))
  
  (define-syntax rename-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ [orig-id bind-id] ...)
          (let ([orig-ids (syntax->list #'(orig-id ...))]
                [bind-ids (syntax->list #'(bind-id ...))])
            (for-each (lambda (id)
                        (unless (identifier? id)
                          (raise-syntax-error
                           #f
                           "expected an identifier, but found something else"
                           stx
                           id)))
                      (append orig-ids bind-ids))
            (apply
             append
             (map (lambda (mode identifier-binding env-desc)
                    (map (lambda (orig-id bind-id)
                           (unless (list? (identifier-binding orig-id))
                             (raise-syntax-error
                              #f
                              (format "no binding~a for identifier" env-desc)
                              stx
                              orig-id))
                           (make-export orig-id
                                        (syntax-e bind-id)
                                        mode
                                        #f
                                        bind-id))
                         orig-ids bind-ids))
                  (if (null? modes)
                      '(run)
                      modes)
                  (if (null? modes)
                      (list identifier-binding)
                      (map (lambda (mode)
                             (cond
                              [(eq? mode 'run) identifier-binding]
                              [(eq? mode 'syntax) identifier-transformer-binding]
                              [(eq? mode 'label) identifier-label-binding]))
                           modes))
                  (if (null? modes)
                      (list "")
                      (map (lambda (mode)
                             (cond
                              [(eq? mode 'run) ""]
                              [(eq? mode 'syntax) " for-syntax"]
                              [(eq? mode 'label) " for-label"]))
                           modes)))))]))))

  (define-syntax except-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ out id ...)
          (let ([exports (expand-export #'out modes)]
                [ids (syntax->list #'(id ...))])
            (for-each (lambda (id)
                        (unless (identifier? id)
                          (raise-syntax-error
                           #f
                           "expected <id>, but found something else"
                           stx
                           id)))
                      ids)
            (let ([dup-id (check-duplicate-identifier ids)])
              (when dup-id
                (raise-syntax-error
                 #f
                 "duplicate identifier"
                 stx
                 dup-id)))
            (map (lambda (id)
                   (or (ormap (lambda (export)
                                (free-identifier=? id (export-local-id export)))
                              exports)
                       (raise-syntax-error
                        #f
                        (format "identifier `~a' not included in nested provide spec"
                                (syntax-e id))
                        stx
                        #'out)))
                 ids)
            (filter (lambda (export)
                      (not (ormap (lambda (id)
                                    ((let ([mode (export-mode export)])
                                       (cond
                                        [(eq? mode 'run) free-identifier=?]
                                        [(eq? mode 'syntax) free-transformer-identifier=?]
                                        [(eq? mode 'label) free-label-identifier=?]))
                                     id 
                                     (export-local-id export)))
                                  ids)))
                    exports))]))))

  (define-for-syntax (build-name id . parts)
    (datum->syntax
     id
     (string->symbol
      (apply string-append
             (map (lambda (p)
                    (if (syntax? p)
                        (symbol->string (syntax-e p))
                        p))
                  parts)))
     id))

  (define-syntax struct-out
    (make-provide-transformer
     (lambda (stx modes)
       (unless (or (null? modes)
                   (memq 'run modes))
         (raise-syntax-error
          #f
          "allowed only for run-time bindings"
          stx))
       (syntax-case stx ()
         [(_ id)
          (let ([id #'id])
            (unless (identifier? id)
              (raise-syntax-error
               #f
               "expected an identifier for a struct-type name, but found something else"
               stx
               id))
            (let ([v (syntax-local-value id (lambda () #f))])
              (if (struct-info? v)
                  (let* ([v (extract-struct-info v)]
                         [super-v (let ([super-id (list-ref v 5)])
                                    (and (identifier? super-id)
                                         (let ([super-v (syntax-local-value super-id (lambda () #f))])
                                           (and (struct-info? super-v)
                                                (extract-struct-info super-v)))))]
                         [list-ize (lambda (ids super-ids)
                                     (let loop ([ids ids])
                                       (cond
                                        [(not (pair? ids)) null]
                                        [(and (pair? super-ids)
                                              (car ids)
                                              (free-identifier=? (car ids)
                                                                 (car super-ids)))
                                         ;; stop because we got to ids that belong to the supertype
                                         null]
                                        [else (cons (car ids) (loop (cdr ids)))])))]
                         ;; FIXME: we're building a list of all imports on every expansion
                         ;; of `syntax-out'. That could become expensive if `syntax-out' is
                         ;; used a lot.
                         [avail-ids (append (let-values ([(ids _) (syntax-local-module-defined-identifiers)])
                                              ids)
                                            (let-values ([(ids _ __)
                                                          (syntax-local-module-required-identifiers #f #t #f #f)])
                                              ids))]
                         [find-imported/defined (lambda (id)
                                                  (let ([ids (filter (lambda (id2)
                                                                       (and (free-identifier=? id2 id)
                                                                            id2))
                                                                     avail-ids)])
                                                    (cond
                                                     [(or (null? ids)
                                                          (pair? (cdr ids)))
                                                      (raise-syntax-error
                                                       #f
                                                       (if (null? ids)
                                                           "no import for structure-type identifier"
                                                           (format "multiple imports (~a~a~a~a) for structure-type identifier"
                                                                   (syntax-e (car ids))
                                                                   (if (null? (cddr ids))
                                                                       " and "
                                                                       ", ")
                                                                   (syntax-e (cadr ids))
                                                                   (if (null? (cddr ids))
                                                                       ""
                                                                       ", ...")))
                                                       stx
                                                       id)]
                                                     [else (car ids)])))])
                    (filter
                     values
                     (map (lambda (id)
                            (and id
                                 (let ([id (find-imported/defined id)])
                                   (make-export id
                                                (syntax-e id)
                                                'run
                                                #f
                                                id))))
                          (append
                           (list id
                                 (list-ref v 0)
                                 (list-ref v 1)
                                 (list-ref v 2))
                           (list-ize (list-ref v 3)
                                     (and super-v
                                          (list-ref super-v 3)))
                           (list-ize (list-ref v 4)
                                     (and super-v
                                          (list-ref super-v 3)))))))
                  (raise-syntax-error
                   #f
                   "identifier is not bound to struct type information"
                   stx
                   id))))]))))

  (define-syntax protect-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ out ...)
          (let ([exports (apply
                          append
                          (map (lambda (out)
                                 (expand-export out modes))
                               (syntax->list #'(out ...))))])
            (map (lambda (e)
                   (make-export
                    (export-local-id e)
                    (export-out-sym e)
                    (export-mode e)
                    #t
                    (export-orig-stx e)))
                 exports))]))))

  (define-syntax prefix-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ pfx out)
          (let ([exports (expand-export #'out modes)])
            (unless (identifier? #'pfx)
              (raise-syntax-error
               #f
               "expected an <id> for prefix, found something else"
               stx
               #'pfx))
            (map (lambda (e)
                   (make-export
                    (export-local-id e)
                    (string->symbol (format "~s~s"
                                            (syntax-e #'pfx)
                                            (export-out-sym e)))
                    (export-mode e)
                    (export-protect? e)
                    (export-orig-stx e)))
                 exports))]))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
