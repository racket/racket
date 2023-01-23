(module reqprov '#%kernel
  (#%require "define.rkt"
             (for-syntax '#%kernel
                         "stx.rkt" "stxcase-scheme.rkt" "define-et-al.rkt"
                         "qq-and-or.rkt" "cond.rkt"
                         "stxloc.rkt" "qqstx.rkt" "more-scheme.rkt"
                         "../require-transform.rkt"
                         "../provide-transform.rkt"
                         "struct-info.rkt"
                         "../phase+space.rkt"))
  
  (#%provide lib file planet submod
             for-syntax for-template for-label for-meta for-space
             require
             only-in rename-in prefix-in except-in combine-in only-meta-in only-space-in
             relative-in
             provide
             all-defined-out all-from-out
             rename-out except-out prefix-out struct-out combine-out
             protect-out
             local-require)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; helpers

  (define-for-syntax (filter pred l)
    (cond
     [(null? l) null]
     [(pred (car l)) (cons (car l) (filter pred (cdr l)))]
     [else (filter pred (cdr l))]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lib, file, planet, submod
  
  (define-for-syntax (xlate-path stx)
    ;; Converts a path based on `require' submform bindings into
    ;; one based on `#%require' symbolic combinations. For example,
    ;; is `quote' is imported as `alt:quote', changes the `alt:quote'
    ;; to plain `quote'.
    (convert-relative-module-path
     (if (pair? (syntax-e stx))
         (let ([kw
                ;; symbolic-identifier=? identifiers are not necessarily free-identifier=?
                (syntax-case stx (lib planet file submod quote)
                  [(quote . _) 'quote]
                  [(lib . _) 'lib]
                  [(planet . _) 'planet]
                  [(file . _) 'file]
                  [(submod . _) 'submod])]
               [d (syntax->datum stx)])
           (cond
            [(eq? kw 'submod)
             (syntax-case stx ()
               [(_ mp . rest)
                (let ([new-mp (case (syntax-e #'mp)
                                [("." "..") #'mp]
                                [else (xlate-path #'mp)])])
                  (if (and (eq? new-mp #'mp)
                           (eq? (car d) 'submod))
                      stx
                      (syntax-track/form (datum->syntax
                                          stx
                                          (list* kw new-mp #'rest)
                                          stx)
                                         stx)))])]
            [(eq? (car d) kw) stx]
            [else (syntax-track/form (datum->syntax
                                      stx
                                      (cons kw (cdr d))
                                      stx)
                                     stx)]))
         stx)))
  
  (define-for-syntax (check-lib-form stx)
    (unless (module-path? (syntax->datum (xlate-path stx)))
      (raise-syntax-error
       #f
       "ill-formed module path"
       stx)))

  (define-syntaxes (lib file planet submod)
    (let ([t (lambda (stx)
               (check-lib-form stx)
               (let* ([stx (xlate-path stx)]
                      [mod-path (syntax->datum stx)]
                      [namess (syntax-local-module-exports stx)])
                 (values
                  (apply
                   append
                   (map (lambda (names)
                          (let ([mode (car names)])
                            (map (lambda (name)
                                   (make-import (datum->syntax
                                                 stx
                                                 name
                                                 stx)
                                                name
                                                mod-path
                                                mode
                                                0
                                                mode
                                                stx))
                                 (cdr names))))
                        namess))
                  (list (make-import-source stx 0)))))])
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
        (values t2 t2 t2 t2))))

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
                                     (phase+space+ (import-mode import) mode)
                                     (phase+space-shift+ (import-req-mode import) mode)
                                     (import-orig-mode import)
                                     (import-orig-stx import)))
                      imports)
                 (map (lambda (source)
                        (make-import-source 
                         (import-source-mod-path-stx source)
                         (phase+space-shift+ (import-source-mode source)
                                             mode)))
                      sources)))]))

  (define-for-syntax (make-require+provide-transformer r p pp)
    (let-values ([(s: mk s? s-ref s-set!)
                  (make-struct-type 'req+prov
                                    #f
                                    0 0 #f
                                    (list 
                                     (cons prop:require-transformer (lambda (a) r))
                                     (cons prop:provide-transformer (lambda (a) p))
                                     (cons prop:provide-pre-transformer (lambda (a) pp))))])
      (mk)))

  (define-for-syntax (exports-at-mode stx modes mode)
    (let ([modes (if (null? modes)
                     (list mode)
                     (map (lambda (m) (phase+space+ m mode))
                          modes))])
      (syntax-case stx ()
        [(_ ex ...)
         (apply append
                (map (lambda (ex) (expand-export ex modes))
                     (syntax->list #'(ex ...))))])))

  (define-syntax for-syntax
    (make-require+provide-transformer
     (lambda (stx)
       (shift-subs stx 1))
     (lambda (stx modes)
       (exports-at-mode stx modes 1))
     (lambda (stx modes)
       (recur-pre stx (if (null? modes) '(1) (map (lambda (mode) (phase+space+ mode 1))
                                                  modes))))))
  
  (define-syntax for-template
    (make-require+provide-transformer
     (lambda (stx)
       (shift-subs stx -1))
     (lambda (stx modes)
       (exports-at-mode stx modes -1))
     (lambda (stx modes)
       (recur-pre stx (if (null? modes) '(-1) (map (lambda (mode) (phase+space+ mode -1))
                                                   modes))))))
  
  (define-syntax for-label
    (make-require+provide-transformer
     (lambda (stx)
       (shift-subs stx #f))
     (lambda (stx modes)
       (exports-at-mode stx modes #f))
     (lambda (stx modes)
       (recur-pre stx '(#f)))))

  (define-for-syntax (make-for-mode extract-mode)
    (make-require+provide-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ mode in ...)
          (let ([base-mode (extract-mode stx #'mode)])
            (shift-subs #'(for-mode in ...) base-mode))]))
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ mode out ...)
          (let ([base-mode (extract-mode stx #'mode)])
            (exports-at-mode #'(for-mode out ...) modes base-mode))]))
     (lambda (stx modes)
       (syntax-case stx ()
         [(for-mode mode out ...)
          (let* ([base-mode (extract-mode stx #'mode)]
                 [modes (if (null? modes)
                            (list base-mode)
                            (map (lambda (v) (phase+space+ v base-mode)) modes))])
            (with-syntax ([(out ...) (map (lambda (o)
                                            (pre-expand-export o modes))
                                          (syntax->list #'(out ...)))])
              (syntax/loc stx
                (for-mode mode out ...))))]))))

  (define-syntax for-meta
    (let ([extract-phase
           (lambda (stx mode)
             (let ([base-mode (syntax-e mode)])
               (unless (or (not base-mode)
                           (exact-integer? base-mode))
                 (raise-syntax-error
                  #f
                  "phase level must be #f or an exact integer"
                  stx
                  mode))
               base-mode))])
      (make-for-mode extract-phase)))

  (define-syntax for-space
    (let ([extract-space
           (lambda (stx mode)
             (let ([base-mode (syntax-e mode)])
               (unless (or (not base-mode)
                           (symbol? base-mode))
                 (raise-syntax-error
                  #f
                  "space must be #f or an identifier"
                  stx
                  mode))
               (phase+space 0 base-mode)))])
      (make-for-mode extract-space)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; require
  
  (define-syntax (require stx)
    (case (syntax-local-context)
      [(module-begin)
       (quasisyntax/loc stx (begin #,stx))]
      [(module top-level)
       (parameterize ([current-require-module-path #f])
         (letrec ([mode-wrap
                   (lambda (mode base)
                     (cond
                       [(eq? mode 0) base]
                       [(pair? mode)
                        (let ([phase-shift (car mode)])
                          (if (eq? phase-shift 0)
                              #`(for-space #,(cdr mode) #,base)
                              #`(for-meta #,phase-shift
                                          (for-space #,(cdr mode) #,base))))]
                       [else #`(for-meta #,mode #,base)]))]
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
                     (syntax-case in (lib file planet submod prefix-in except-in rename-in quote)
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
                       [(submod . s)
                        (check-lib-form in)
                        (list (mode-wrap base-mode (xlate-path in)))]
                       [(prefix-in pfx path)
                        (simple-path? #'path)
                        (list (mode-wrap
                               base-mode
                               (syntax-track/form
                                (datum->syntax
                                 #'path
                                 (syntax-e
                                  (quasisyntax
                                   (prefix pfx #,(xlate-path #'path))))
                                 in)
                                in)))]
                       [(except-in path id ...)
                        (and (simple-path? #'path)
                             ;; check that it's well-formed...
                             (call-with-values (lambda () (expand-import in))
                               (lambda (a b) #t)))
                        (list (mode-wrap
                               base-mode
                               (syntax-track/form
                                (datum->syntax
                                 #'path
                                 (syntax-e
                                  (quasisyntax/loc in
                                    (all-except #,(xlate-path #'path) id ...))))
                                in)))]
                       [(rename-in path [orig-id bind-id] ...)
                        (and (simple-path? #'path)
                             (call-with-values (lambda () (expand-import in))
                               (lambda (a b) #t)))
                        (let ([xlated-path (xlate-path #'path)])
                          (cons (mode-wrap
                                 base-mode
                                 (syntax-track/form
                                  (datum->syntax
                                   #'path
                                   (syntax-e
                                    (quasisyntax/loc in
                                      (all-except #,xlated-path orig-id ...))))
                                  in))
                                (map (λ (bind-id orig-id)
                                       (mode-wrap
                                        base-mode
                                        (datum->syntax
                                         #'path
                                         (syntax-e
                                          (quasisyntax/loc in
                                            (rename #,xlated-path #,bind-id #,orig-id))))))
                                     (syntax->list #'(bind-id ...))
                                     (syntax->list #'(orig-id ...)))))]
                       ;; General case:
                       [_ (let-values ([(imports sources) (expand-import in)])
                            ;; Note: in case `in` could be expressed as a simple import form,
                            ;; the core `#%require` form will collapse back to simple form
                            ;; in many cases.
                            (cons/syntax-track/form
                             #'(just-meta 0)
                             in
                             (append
                              (map (lambda (import)
                                     #`(just-meta
                                        #,(phase+space-phase (import-orig-mode import))
                                        #,(mode-wrap (phase+space-shift+ base-mode (import-req-mode import))
                                                     #`(just-space
                                                        #,(phase+space-space (import-orig-mode import))
                                                        #,(quasisyntax/loc in
                                                            (rename #,(import-src-mod-path import)
                                                                    #,(import-local-id import)
                                                                    #,(if (eq? (syntax-e (import-orig-stx import))
                                                                               (import-src-sym import))
                                                                          (import-orig-stx import)
                                                                          (datum->syntax
                                                                           #f
                                                                           (import-src-sym import)
                                                                           (import-orig-stx import)))))))))
                                   imports)
                              (map (lambda (src)
                                     (mode-wrap (phase+space-shift+ base-mode (import-source-mode src))
                                                #`(only #,(import-source-mod-path-stx src))))
                                   sources))))]))]
                  [transform-one
                   (lambda (in)
                     ;; Recognize `for-syntax', etc. for simple cases:
                     (syntax-case in (for-meta)
                       [(for-meta n elem ...)
                        (or (exact-integer? (syntax-e #'n))
                            (not (syntax-e #'n)))
                        (cons/syntax-track/form
                         #'(for-meta n)
                         in
                         (apply append
                                (map (lambda (in)
                                       (transform-simple in (syntax-e #'n)))
                                     (syntax->list #'(elem ...)))))]
                       [(for-something elem ...)
                        (and (identifier? #'for-something)
                             (ormap (lambda (i) (free-identifier=? i #'for-something))
                                    (list #'for-syntax #'for-template #'for-label)))
                        (cons/syntax-track/form
                         #'(for-something)
                         in
                         (apply append
                                (map (lambda (in)
                                       (transform-simple in
                                                         (cond
                                                          [(free-identifier=? #'for-something #'for-syntax)
                                                           1]
                                                          [(free-identifier=? #'for-something #'for-template)
                                                           -1]
                                                          [(free-identifier=? #'for-something #'for-label)
                                                           #f])))
                                     (syntax->list #'(elem ...)))))]
                       [_ (transform-simple in 0 #| run phase |#)]))])
           (syntax-case stx ()
             [(_ in)
              (with-syntax ([(new-in ...) (transform-one #'in)])
                (syntax/loc stx
                  (#%require new-in ...)))]
             [(_ in ...)
              ;; Prefetch on simple module paths:
              (let ([prefetches
                     (let loop ([in (syntax->list #'(in ...))])
                       (cond
                        [(null? in) null]
                        [(let ([a (syntax->datum (car in))])
                           (and (module-path? a) a))
                         => (lambda (a)
                              (cons a (loop (cdr in))))]
                        [else
                         (let ([a (syntax->list (car in))])
                           (if (and a
                                    (let ([i (car a)])
                                      (and (identifier? i)
                                           (or (free-identifier=? #'for-something #'for-syntax)
                                               (free-identifier=? #'for-something #'for-template)
                                               (free-identifier=? #'for-something #'for-label)))))
                               (loop (append (cdr a) (cdr in)))
                               (loop (cdr in))))]))])
                (unless (null? prefetches)
                  (log-message (current-logger)
                               'info 
                               'module-prefetch
                               (format "module-prefetch: ~s in: ~s"
                                       prefetches
                                       (current-load-relative-directory))
                               (list prefetches (current-load-relative-directory))
                               #f))
                (with-syntax ([(req-in ...)
                               (map (lambda (in)
                                      (with-syntax ([in in])
                                        (syntax/loc stx (require in))))
                                    (syntax->list #'(in ...)))])
                  (syntax/loc stx
                    (begin req-in ...))))])))]
      [else
       (raise-syntax-error #f
                           "not at module level or top level"
                           stx)]))

  (define-for-syntax (syntax-track/form stx orig)
    (syntax-track-origin stx orig (syntax-local-introduce (car (syntax-e orig)))))

  (define-for-syntax (cons/syntax-track/form stx orig l)
    ;; Add `stx` as a dummy require if needed to track `orig`
    (if (pair? l)
        (cons (syntax-track/form (car l) orig)
              (cdr l))
        (cons (syntax-track/form stx orig) l)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; require transformers

  (define-for-syntax (import-identifier=? a b)
    ;; Prior to v8.2.0.3, this was `free-identifier=?` (without a phase);
    ;; the primitive `all-except` uses symbol equality, not binding,
    ;; and that seems more appropriate
    (eq? (syntax-e a) (syntax-e b)))

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
               (apply
                append
                (map (lambda (new-id orig-id)
                       (let ([l (filter
                                 values
                                 (map (lambda (import)
                                        (and (import-identifier=? orig-id (import-local-id import))
                                             (if (eq? new-id orig-id)
                                                 import
                                                 (make-import new-id
                                                              (import-src-sym import)
                                                              (import-src-mod-path import)
                                                              (import-mode import)
                                                              (import-req-mode import)
                                                              (import-orig-mode import)
                                                              orig-id))))
                                      imports))])
                         (if (null? l)
                             (raise-syntax-error
                              #f
                              (format "identifier `~a' not included in nested require spec"
                                      (syntax-e orig-id))
                              stx
                              #'in)
                             l)))
                     new-ids orig-ids))
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
                                     (import-identifier=? id (import-local-id import)))
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
                                     (import-identifier=? id (import-local-id import)))
                                   ids)))
                     imports)
             sources))]))))

  (define-syntax combine-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ in ...)
          (let ([subs
                 (map (lambda (in)
                        (let-values ([(imports sources) (expand-import in)])
                          (cons imports sources)))
                      (syntax->list #'(in ...)))])
              (values (apply append (map car subs))
                      (apply append (map cdr subs))))]))))

  (define-syntax only-meta-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ mode in ...)
          (let ([base-mode (syntax-e #'mode)])
            (unless (or (not base-mode)
                        (exact-integer? base-mode))
              (raise-syntax-error
               #f
               "phase level must be #f or an exact integer"
               stx
               #'mode))
            (filter-by-mode base-mode phase+space-phase #'(in ...)))]))))

  (define-syntax only-space-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ mode in ...)
          (let ([base-mode (syntax-e #'mode)])
            (unless (or (not base-mode)
                        (symbol? base-mode))
              (raise-syntax-error
               #f
               "space must be #f or an identifier"
               stx
               #'mode))
            (filter-by-mode base-mode phase+space-space #'(in ...)))]))))

  (define-for-syntax (filter-by-mode base-mode sel ins)
    (let ([subs
           (map (lambda (in)
                  (let-values ([(imports sources) (expand-import in)])
                    (cons
                     (filter (lambda (import)
                               (equal? (sel (import-mode import)) base-mode))
                             imports)
                     sources)))
                (syntax->list ins))])
      (values (apply append (map car subs))
              (apply append (map cdr subs)))))

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
                   (apply
                    append
                    (map (lambda (orig-id bind-id)
                           (let ([rename-imports (filter (lambda (import)
                                                           (import-identifier=? orig-id
                                                                                (import-local-id import)))
                                                         imports)])
                             (unless (pair? rename-imports)
                               (raise-syntax-error
                                #f
                                (format "identifier `~a' not included in nested require spec"
                                        (syntax-e orig-id))
                                stx
                                #'in))
                             (map (lambda (import)
                                    (cons (make-import bind-id
                                                       (import-src-sym import)
                                                       (import-src-mod-path import)
                                                       (import-mode import)
                                                       (import-req-mode import)
                                                       (import-orig-mode import)
                                                       orig-id)
                                          import))
                                  rename-imports)))
                         orig-ids bind-ids))])
              (let ([leftover-imports 
                     (let ([ht (make-immutable-hash
                                (map (lambda (v) (cons (cdr v) #f))
                                     new+olds))])
                       (filter (lambda (i) (hash-ref ht i #t)) imports))])
                ;; Make sure no new name is in the leftover set:
                (for-each (lambda (bind-id)
                            (when (ormap (lambda (import)
                                           (and (import-identifier=? bind-id
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
                                   (import-orig-mode import)
                                   (import-orig-stx import))))
                  imports)
             sources))]))))

  (define-syntax relative-in
    (make-require-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ mod-path in ...)
          (let ([mp (syntax->datum #'mod-path)])
            (unless (module-path? mp)
              (raise-syntax-error #f
                                  "bad module path"
                                  stx
                                  #'mod-path))
            (parameterize ([current-require-module-path (module-path-index-join
                                                         mp
                                                         (current-require-module-path))])
              (expand-import #`(combine-in in ...))))]))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; provide
  
  (define-syntax (provide stx)
    (case (syntax-local-context)
      [(module-begin)
       (quasisyntax/loc stx (begin #,stx))]
      [(module)
       (syntax-case stx ()
         [(_ out ...)
          (with-syntax ([(out ...)
                         (map (lambda (o) (pre-expand-export o null))
                              (syntax->list #'(out ...)))])
            (syntax-property
             (quasisyntax/loc stx 
               (#%provide #,(syntax-property
                             #`(expand (provide-trampoline out ...) #,stx)
                             'certify-mode 'transparent)))
             'certify-mode 'transparent))])]
      [else
       (raise-syntax-error #f
                           "not at module level"
                           stx)]))

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
                                       (quasisyntax/loc out
                                         (rename #,(export-local-id export)
                                                 #,(if (eq? (syntax-e (export-orig-stx export))
                                                            (export-out-sym export))
                                                       (export-orig-stx export)
                                                       (datum->syntax
                                                        #f
                                                        (export-out-sym export)
                                                        (export-orig-stx export))))))]
                                  [mode (export-mode export)])
                              (let ([moded
                                     (let ([spaced (let ([space (phase+space-space mode)])
                                                     (if space
                                                         #`(for-space #,space #,base)
                                                         base))])
                                       (let ([phase (phase+space-phase mode)])
                                         (cond
                                           [(eq? phase 0) spaced]
                                           [else #`(for-meta #,phase #,spaced)])))])
                                (if (export-protect? export)
                                    #`(protect #,moded)
                                    moded))))
                          exports)))])
         (syntax-case stx ()
           [(_ out ...)
            (let ([outs (syntax->list #'(out ...))])
              (with-syntax ([(new-out ...) (apply append (map transform-simple outs))])
                (copy-disappeared-uses
                 outs
                 (syntax/loc stx
                   (begin new-out ...)))))]))]))

  (define-for-syntax (combine-prop b a)
    (if a (if b (cons a b) a) b))

  (define-for-syntax (copy-disappeared-uses outs r)
    (cond
      [(null? outs) r]
      [else
       (syntax-property
        r
        'disappeared-use
        (let loop ([outs outs]
                   [disappeared-uses (syntax-property r 'disappeared-use)])
          (cond
            [(null? outs) disappeared-uses]
            [else
             (let ([p (syntax-property (car outs) 'disappeared-use)]
                   [name (if (identifier? (car outs))
                             #f
                             (syntax-local-introduce (car (syntax-e (car outs)))))])
               (loop
                (cdr outs)
                (combine-prop p (combine-prop name disappeared-uses))))])))]))



  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; provide transformers

  (define-for-syntax (free-identifier=?/mode a b mode)
    (let ([phase (phase+space-phase mode)])
      (free-identifier=? (add-mode-scope a mode) (add-mode-scope b mode) phase)))

  (define-for-syntax (add-mode-scope stx mode)
    (let ([space (phase+space-space mode)])
      (if space
          ((make-interned-syntax-introducer space) stx 'add)
          stx)))

  (define-for-syntax (recur-pre stx modes)
    (syntax-case stx ()
      [(fm out ...)
       (with-syntax ([(out ...) (map (lambda (o)
                                       (pre-expand-export o modes))
                                     (syntax->list #'(out ...)))])
         (syntax/loc stx
           (fm out ...)))]))

  (define-syntax all-defined-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_)
          (let* ([ht (syntax-local-module-defined-identifiers)]
                 [same-ctx? (lambda (free-identifier=?)
                              (lambda (id)
                                (free-identifier=? id
                                                   (datum->syntax
                                                    stx
                                                    (syntax-e id)))))]
                 [modes (if (null? modes)
                            '(0)
                            modes)])
            (apply
             append
             (map (lambda (mode)
                    (let* ([abs-mode (phase+space+ mode (syntax-local-phase-level))]
                           [same-ctx-in-phase?
                            (same-ctx? 
                             (cond
                              [(eq? mode 0) free-identifier=?]
                              [(eq? mode 1) free-transformer-identifier=?]
                              [else (lambda (a b)
                                      (free-identifier=?/mode a b abs-mode))]))]
                           [right-space?
                            (let ([space (phase+space-space mode)])
                              (if space
                                  (let ([intro (make-interned-syntax-introducer space)])
                                    (lambda (id)
                                      (not (free-identifier=? (intro id 'add) (intro id 'remove)))))
                                  (lambda (id) #t)))])
                      (map (lambda (id)
                             (make-export id (syntax-e id) mode #f stx))
                           (filter (lambda (id)
                                     (and (same-ctx-in-phase? id)
                                          (right-space? id)
                                          (let-values ([(v id) (syntax-local-value/immediate
                                                                id
                                                                (lambda () (values #f #f)))])
                                            (not
                                             (and (rename-transformer? v)
                                                  (syntax-property 
                                                   (rename-transformer-target v)
                                                   'not-provide-all-defined))))))
                                   (hash-ref ht (phase+space-phase abs-mode) null)))))
                  modes)))]))))

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
              (let ([idss
                     (apply
                      append
                      (map (lambda (mode)
                             (let ([r (syntax-local-module-required-identifiers (syntax->datum mp) 
                                                                                mode)])
                               (or r
                                   (raise-syntax-error
                                    #f
                                    (format "no corresponding require~a~a"
                                            (let ([phase (phase+space-phase mode)])
                                              (cond
                                                [(eq? phase 0) ""]
                                                [(not phase) " at the label phase level"]
                                                [else (format " at phase level ~a" phase)]))
                                            (let ([space (phase+space-space mode)])
                                              (cond
                                                [(not space) ""]
                                                [else (format " in space ~a" space)])))
                                    stx
                                    mp))))
                           (if (null? modes)
                               '(0)
                               modes)))]
                    [ok-context? (lambda (id id=?)
                                   (id=? id
                                         (datum->syntax mp (syntax-e id))))]) 
                (filter
                 values
                 (apply
                  append
                  (map (lambda (ids)
                         (let ([mode (car ids)])
                           (map (lambda (id)
                                  (and (free-identifier=?/mode id (datum->syntax mp (syntax-e id))
                                                               mode)
                                       (make-export id (syntax-e id) mode #f stx)))
                                (cdr ids))))
                       idss)))))
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
             (map (lambda (mode)
                    (let ([abs-phase (phase+space-phase (phase+space+ mode (syntax-local-phase-level)))])
                      (map (lambda (orig-id bind-id)
                             (unless (list? (identifier-binding (add-mode-scope orig-id mode) abs-phase))
                               (raise-syntax-error
                                #f
                                (format "no binding~a~a for identifier"
                                        (let ([phase (phase+space-phase mode)])
                                          (cond
                                            [(eq? phase 0) ""]
                                            [(not phase) " in the label phase level"]
                                            [(not phase) (format " at phase level ~a" phase)]
                                            [else ""]))
                                        (let ([space (phase+space-space mode)])
                                          (cond
                                            [(not space) ""]
                                            [else (format " in space ~a" space)])))
                                stx
                                orig-id))
                             (make-export orig-id
                                          (syntax-e bind-id)
                                          mode
                                          #f
                                          bind-id))
                           orig-ids bind-ids)))
                  (if (null? modes)
                      '(0)
                      modes))))]))))

  (define-syntax except-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ out spec ...)
          (let ([exports (expand-export #'out modes)]
                [exceptions (apply
                             append
                             (map (lambda (spec)
                                    (expand-export spec modes))
                                  (syntax->list #'(spec ...))))])
            (for-each (lambda (exception)
                        (or (ormap (lambda (export)
                                     (and (equal? (export-mode export)
                                                  (export-mode exception))
                                          (free-identifier=?/mode (export-local-id exception)
                                                                  (export-local-id export)
                                                                  (export-mode export))))
                                   exports)
                            (raise-syntax-error
                             #f
                             (format "identifier to remove `~a' not included in nested provide spec"
                                     (syntax-e (export-local-id exception)))
                             stx
                             #'out)))
                      exceptions)
            (filter (lambda (export)
                      (not (ormap (lambda (exception)
                                    (and (equal? (export-mode export)
                                                 (export-mode exception))
                                         (free-identifier=?/mode (export-local-id exception)
                                                                 (export-local-id export)
                                                                 (export-mode export))))
                                  exceptions)))
                    exports))]))
     (lambda (stx modes)
       (recur-pre stx modes))))

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
                   (equal? '(0) modes))
         (raise-syntax-error
          #f
          "allowed only for relative phase level 0"
          stx))
       (syntax-case stx ()
         [(_ id)
          (let ([id #'id]
                [s-id #'id])
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
                                     (let ((super-ids (and super-ids (filter values super-ids))))
                                       ;Remove all unknown super-ids
                                       (let loop ([ids ids])
                                        (cond
                                         [(not (pair? ids)) null]
                                         [(and (pair? super-ids)
                                               (car ids)
                                               (free-identifier=? (car ids)
                                                                  (car super-ids)))
                                          ;; stop because we got to ids that belong to the supertype
                                          null]
                                         [else (cons (car ids) (loop (cdr ids)))]))))]
                         ;; FIXME: we're building a list of all imports on every expansion
                         ;; of `struct-out'. That could become expensive if `struct-out' is
                         ;; used a lot.
                         [avail-ids (append (hash-ref (syntax-local-module-defined-identifiers)
                                                      (syntax-local-phase-level)
                                                      null)
                                            (let ([idss (syntax-local-module-required-identifiers #f #t)])
                                              (if idss
                                                  (let ([a (assoc (syntax-local-phase-level) idss)])
                                                    (if a
                                                        (cdr a)
                                                        null))
                                                  null)))]
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
                                                           "no binding for structure-type identifier"
                                                           (format "multiple bindings (~a~a~a~a) for structure-type identifier"
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
                                   (make-export (syntax-property
                                                 id
                                                 'disappeared-use
                                                 (combine-prop
                                                  (syntax-local-introduce s-id)
                                                  (syntax-property
                                                   id
                                                   'disappeared-use)))
                                                (syntax-e id)
                                                0
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
                                          (list-ref super-v 4)))))))
                  (raise-syntax-error
                   #f
                   "identifier is not bound to struct type information"
                   stx
                   id))))]))))

  (define-syntax combine-out
    (make-provide-transformer
     (lambda (stx modes)
       (syntax-case stx ()
         [(_ out ...)
          (apply
           append
           (map (lambda (out)
                  (expand-export out modes))
                (syntax->list #'(out ...))))]))
     (lambda (stx modes)
       (recur-pre stx modes))))

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
                 exports))]))
     (lambda (stx modes)
       (recur-pre stx modes))))

  (define-syntax prefix-out
    (let ([check-prefix
           (lambda (stx pfx)
             (unless (identifier? pfx)
               (raise-syntax-error
                #f
                "expected an <id> for prefix, found something else"
                stx
                pfx)))])
      (make-provide-transformer
       (lambda (stx modes)
         (syntax-case stx ()
           [(_ pfx out)
            (check-prefix stx #'pfx)
            (let ([exports (expand-export #'out modes)])
              (map (lambda (e)
                     (make-export
                      (export-local-id e)
                      (string->symbol (format "~a~a"
                                              (syntax-e #'pfx)
                                              (export-out-sym e)))
                      (export-mode e)
                      (export-protect? e)
                      (export-orig-stx e)))
                   exports))]))
       (lambda (stx modes)
         (syntax-case stx ()
           [(_ pfx out)
            (check-prefix stx #'pfx)
            (with-syntax ([out (pre-expand-export #'out modes)])
              (syntax/loc stx (prefix-out pfx out)))])))))
           

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; rename-import : Import Identifier -> Import
  ;; Creates a new import that binds the given identifier, but otherwise acts as
  ;; the original import.
  (define-for-syntax (rename-import i id)
    (make-import id
                 (import-src-sym i)
                 (import-src-mod-path i)
                 (import-mode i)
                 (import-req-mode i)
                 (import-orig-mode i)
                 (import-orig-stx i)))

  ;; import->raw-require-spec : Import -> Syntax
  ;; Constructs a raw-require-spec (suitable for #%require) that should have the
  ;; same behavior as a require-spec that produces the given import.
  (define-for-syntax (import->raw-require-spec i)
    (datum->syntax
     (import-orig-stx i)
     (list #'just-meta
           (phase+space-phase (import-orig-mode i))
           (list #'for-meta
                 (phase+space-phase (import-mode i))
                 (let ([at-space (list #'just-space
                                       (phase+space-space (import-orig-mode i))
                                       (list #'rename
                                             (import-src-mod-path i)
                                             (import-local-id i)
                                             (import-src-sym i)))])
                   ;; avoid a space shift if unnecessary:
                   (if (eq? (phase+space-space (import-orig-mode i))
                            (phase+space-space (import-mode i)))
                       at-space
                       (list #'for-space
                             (phase+space-space (import-mode i))
                             at-space)))))
     (import-orig-stx i)))

  (define-for-syntax (import-local-id-with-space-scope i)
    (add-mode-scope (import-local-id i) (import-mode i)))

  ;; (do-local-require rename spec ...)
  ;; Lifts (require spec ...) to the (module) top level, and makes the imported
  ;; bindings available in the current context via a renaming macro.
  (define-syntax (local-require stx)
    (when (eq? 'expression (syntax-local-context))
      (raise-syntax-error #f "not allowed in an expression context" stx))
    (let ([stx (syntax-local-introduce stx)])
      (syntax-case stx []
        [(_ spec ...)
         (let*-values ([(imports sources)
                        (expand-import
                         (datum->syntax
                          stx
                          (list* #'only-meta-in 0 (syntax->list #'(spec ...)))
                          stx))]
                       [(names) (map import-local-id-with-space-scope imports)]
                       [(reqd-names)
                        ;; Could be just `(generate-temporaries names)`, but using the
                        ;; exported name turns out to be a hint to Check Syntax for binding
                        ;; arrows, for now:
                        (let ([intro (make-syntax-introducer)])
                          (map (lambda (n i) (intro (add-mode-scope (datum->syntax #f (syntax-e n) n)
                                                                    (import-mode i))))
                               names
                               imports))]
                       [(renamed-imports) (map rename-import imports reqd-names)]
                       [(raw-specs) (map import->raw-require-spec renamed-imports)]
                       [(lifts) (map syntax-local-lift-require raw-specs reqd-names)])
           (with-syntax ([(name ...) (map syntax-local-introduce names)]
                         [(lifted ...) (map syntax-local-introduce lifts)])
             (syntax/loc stx (define-syntaxes (name ...)
                               (values (make-rename-transformer (quote-syntax lifted)) ...)))))])))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
