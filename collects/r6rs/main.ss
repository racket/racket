#lang scheme/base

#|
FIXME:
 * Check that exported identifiers are defined in some phase.
 * Check that each identifier is imported only once across phases.
|#

(require (for-syntax scheme/base
                     syntax/kerncase
                     "private/find-version.ss"
                     scheme/provide-transform))

(provide (rename-out [module-begin #%module-begin]))

;; ----------------------------------------
;; Library and top-level forms

(define-for-syntax (syntax-list? stx)
  (syntax->list stx))

(define-syntax (module-begin stx)
  (syntax-case stx (library)
    [(_ (library . rest))
     ;; A library
     (with-syntax ([(_ orig) stx])
       #'(#%plain-module-begin 
          (library-body orig . rest)))]
    [(_ (library . rest) . _)
     (raise-syntax-error 
      #f
      "allowed only in as a top-level module by itself"
      (syntax-case stx ()
        [(_ lib . _) #'lib]))]
    [(_  decl . rest)
     (syntax-list? #'rest)
     ;; A top-level program
     #'(#%plain-module-begin 
        (top-level-body decl . rest))]
    [(_)
     (raise-syntax-error 'r6rs
                         (string-append
                          "must contain a `library' form (for a library)"
                          " or start with `import' (for a top-level program)")
                         stx)]
    [_
     (raise-syntax-error #f "ill-formed module (misuse of `.')" stx)]))

(define-syntax (top-level-body stx)
  (syntax-case stx (import)
    [(_ (import . im) . rest)
     (syntax-list? #'im)
     (with-syntax ([(_ im . _) stx])
       #'(begin
           (r6rs-import im)
           . rest))]
    [(_ (import . im) . rest)
     (raise-syntax-error #f
                         "ill-formed imports (misuse of `.')"
                         (syntax-case stx ()
                           [(_ im . _) #'im]))]
    [(_ thing . _)
     (raise-syntax-error 'top-level-program
                         "expected an `import' declaration, found something else"
                         #'thing)]))

(define-for-syntax (symbolic-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define-for-syntax (valid-library-name? n)
  (syntax-case n ()
    [(id id2 ... (vers ...))
     (and (identifier? #'id)
          (andmap identifier? (syntax->list #'(id2 ...)))
          (andmap (lambda (v)
                    (exact-nonnegative-integer? (syntax-e v)))
                  (syntax->list #'(vers ...))))]
    [(id id2 ...)
     (valid-library-name? #'(id id2 ... ()))]
    [_ #f]))

(define-syntax (library-body stx)
  (syntax-case* stx (export import) symbolic-identifier=?
    [(_ orig name 
        (export . ex)
        (import . im)
        . body)
     (and (valid-library-name? #'name)
          (syntax-list? #'ex)
          (syntax-list? #'im)
          (syntax-list? #'body))
     (with-syntax ([(_ _ _ ex im . _) stx])
       #'(begin
           (r6rs-import im)
           (r6rs-export ex)
           (library-body/defns . body)))]
    [(_ orig name . _)
     (not (valid-library-name? #'name))
     (raise-syntax-error #f
                         "invalid library name"
                         #'orig
                         #'name)]
    [(_ orig name (export . ex) . _)
     (not (syntax-list? #'ex))
     (raise-syntax-error #f
                         "ill-formed export sequence (misuse of `.')"
                         #'orig
                         (syntax-case stx ()
                           [(_ _ _ ex . _) #'ex]))]
    [(_ orig name (export . ex) (import . im) . _)
     (not (syntax-list? #'im))
     (raise-syntax-error #f
                         "ill-formed import sequence (misuse of `.')"
                         #'orig
                         (syntax-case stx ()
                           [(_ _ _ _ im . _) #'im]))]
    [(_ orig name (export . ex) (import . im) . _)
     (raise-syntax-error #f
                         "ill-formed body (misuse of `.')"
                         #'orig)]
    [(_ orig name (export . ex))
     (raise-syntax-error #f
                         "missing `import' clause"
                         #'orig)]
    [(_ orig name (export . ex) im . _)
     (raise-syntax-error #f
                         "expected `import' clause, found something else"
                         #'orig
                         #'im)]
    [(_ orig name)
     (raise-syntax-error #f
                         "missing `export' and `import' clauses"
                         #'orig)]
    [(_ orig name ex . _)
     (raise-syntax-error #f
                         "expected `export' clause, found something else"
                         #'orig
                         #'ex)]
    [(_ orig)
     (raise-syntax-error #f
                         "missing name, `export' clauses, and `import' clause"
                         #'orig)]
    [(_ orig . _)
     (raise-syntax-error #f
                         "ill-formed library"
                         #'orig)]))

(define-syntax (library-body/defns stx)
  (syntax-case stx ()
    [(_ thing . more)
     (let ([a (local-expand
               #'thing
               'module
               (kernel-form-identifier-list))])
       (syntax-case a (begin)
         [(def . _)
          (ormap (lambda (id)
                   (free-identifier=? id #'def))
                 (list #'define-values
                       #'define-syntaxes
                       #'define-values-for-syntax))
          #`(begin #,a (library-body/defns . more))]
         [(begin sub ...)
          #`(library-body/defns sub ... . more)]
         [else
          #`(begin (let () #,a . more))]))]
    [(_) #'(begin)]))

;; ----------------------------------------
;; Imports and exports

(define-for-syntax (is-sub-version-reference? stx)
  (syntax-case* stx (<= >= and or not) symbolic-identifier=?
    [n (exact-nonnegative-integer? (syntax-e #'n)) #t]
    [(>= n) (exact-nonnegative-integer? (syntax-e #'n))]
    [(<= n) (exact-nonnegative-integer? (syntax-e #'n))]
    [(and sv ...) (andmap is-sub-version-reference? (syntax->list #'(sv ...)))]
    [(or sv ...) (andmap is-sub-version-reference? (syntax->list #'(sv ...)))]
    [(not sv) (is-sub-version-reference? #'sv)]
    [_ #f]))

(define-for-syntax (is-version-reference? stx)
  (syntax-case* stx (and or not) symbolic-identifier=?
    [(and vr ...)
     (andmap is-version-reference? (syntax->list #'(vr ...)))]
    [(or vr ...)
     (andmap is-version-reference? (syntax->list #'(vr ...)))]
    [(not vr)
     (is-version-reference? #'vr)]
    [(sv ...)
     (andmap is-sub-version-reference? (syntax->list #'(sv ...)))]
    [_ #f]))

(define-for-syntax (parse-library-reference orig stx)
  (syntax-case stx ()
    [(id1 id2 ... (vers ...))
     (and (identifier? #'id1)
          (andmap identifier? (syntax->list #'(id2 ...)))
          (is-version-reference? #'(vers ...)))
     (let-values ([(coll file)
                   (let ([strs (map (lambda (id)
                                      (symbol->string (syntax-e id)))
                                    (syntax->list #'(id1 id2 ...)))])
                     (if (= 1 (length strs))
                         (values (list (car strs)) "main")
                         (values (reverse (cdr (reverse strs)))
                                 (car (reverse strs)))))])
       (let ([base (build-path (with-handlers ([exn:fail?
                                                (lambda (exn)
                                                  (raise-syntax-error
                                                   #f
                                                   (format 
                                                    "cannot find suitable library installed (exception: ~a)"
                                                    (if (exn? exn)
                                                        (exn-message exn)
                                                        exn))
                                                   orig
                                                   stx))])
                                 (apply collection-path coll))
                               file)])
         (let ([vers (find-version (path->bytes base) (syntax->datum #'(vers ...)))])
           (if vers
               (datum->syntax
                orig
                `(,#'lib ,(apply string-append
                                 (car coll)
                                 (append
                                  (map (lambda (s)
                                         (string-append "/" s))
                                       (append (cdr coll) (list file)))
                                  (map (lambda (v)
                                         (format "-~a" v))
                                       vers)
                                  (list ".ss")))))
               (raise-syntax-error
                #f
                "cannot find suitable installed library"
                orig
                stx)))))]
    [(id1 id2 ...)
     (and (identifier? #'id1)
          (andmap identifier? (syntax->list #'(id2 ...))))
     (parse-library-reference orig #'(id1 id2 ... ()))]
    [_
     (raise-syntax-error #f
                         "ill-formed library reference"
                         orig
                         stx)]))

(define-for-syntax (parse-import-set orig stx)
  (define (bad)
    (raise-syntax-error #f
                        (format "bad `~a' form" 
                                (syntax-e (car (syntax-e stx))))
                        orig
                        stx))
  (define (check-id id)
    (unless (identifier? id)
      (raise-syntax-error #f
                          (format "not an identifier in `~a' form"
                                  (syntax-e (car (syntax-e stx))))
                          orig
                          id)))
  (syntax-case* stx (library only except prefix rename) symbolic-identifier=?
    [(library lib)
     (parse-library-reference orig #'lib)]
    [(library . _) (bad)]
    [(only im id ...)
     (for-each check-id (syntax->list #'(id ...)))
     #`(only-in #,(parse-import-set orig #'im) id ...)]
    [(only . _) (bad)]
    [(except im id ...)
     (for-each check-id (syntax->list #'(id ...)))
     #`(except-in #,(parse-import-set orig #'im) id ...)]
    [(except . _) (bad)]
    [(prefix im id)
     (check-id #'id)
     #`(prefix-in id #,(parse-import-set orig #'im))]
    [(prefix . _) (bad)]
    [(rename im (id id2) ...)
     (for-each check-id 
               (apply
                append
                (map syntax->list
                     (syntax->list #'((id id2) ...)))))
     #`(rename-in #,(parse-import-set orig #'im) [id id2] ...)]
    [(rename . _) (bad)]
    [_ (parse-library-reference orig stx)]))

(define-syntax (r6rs-import stx)
  (let ([orig (syntax-case stx ()
                [(_ orig) #'orig])])
    (syntax-case stx (import)
      [(_ (import im ...))
       (with-syntax ([((im ...) ...)
                      (map (lambda (im)
                             (syntax-case* im (for) symbolic-identifier=?
                               [(for base-im level ...)
                                (let* ([levels
                                        (cons
                                         #f
                                         (map (lambda (level)
                                                (syntax-case* level (run expand meta) symbolic-identifier=?
                                                  [run #'0]
                                                  [expand #'1]
                                                  [(meta 0) #'0]
                                                  [(meta n) #'n]
                                                  [_
                                                   (raise-syntax-error
                                                    #f
                                                    "bad `for' level"
                                                    orig
                                                    level)]))
                                              (syntax->list #'(level ...))))])
                                  (with-syntax ([is (parse-import-set orig #'base-im)])
                                    (with-syntax ([(level ...) levels]
                                                  [prelims (datum->syntax orig
                                                                          'r6rs/private/prelims)])
                                      #`((for-meta level is prelims) ...))))]
                               [(for . _)
                                (raise-syntax-error
                                 #f
                                 "bad `for' import form"
                                 orig
                                 im)]
                               [_ (let ([m (parse-import-set orig im)])
                                    (list m `(for-label ,m)))]))
                           (syntax->list #'(im ...)))]
                     [prelims (datum->syntax orig
                                             'r6rs/private/prelims)])
         #'(require prelims im ... ...))])))

(define-syntax (r6rs-export stx)
  (let ([orig (syntax-case stx ()
                [(_ orig) #'orig])])
    (syntax-case stx (export)
      [(_ (export ex ...))
       (let ([exs (syntax->list #'(ex ...))])
         (for-each (lambda (ex)
                     (unless (identifier? ex)
                       (syntax-case ex (rename)
                         [(rename thing ...)
                          (for-each (lambda (thing)
                                      (syntax-case thing ()
                                        [(id1 id2)
                                         (unless (and (identifier? #'id1)
                                                      (identifier? #'id2))
                                           (raise-syntax-error #f
                                                               "not an identifier"
                                                               orig
                                                               (if (identifier? #'id1)
                                                                   #'id2
                                                                   #'id1)))]
                                        [_ (raise-syntax-error 
                                            #f
                                            "expected `(<id> <id>)' for rename, but found something else"
                                            orig
                                            thing)]))
                                    (syntax->list #'(thing ...)))]
                         [(rename . _)
                          (raise-syntax-error #f
                                              "bad syntax (misuse of `.')"
                                              orig
                                              ex)]
                         [_
                          (raise-syntax-error #f
                                              "not an identifier or `rename' clause"
                                              orig
                                              ex)])))
                   exs)
         (with-syntax ([((ex ...) ...)
                        (map (lambda (ex)
                               (syntax-case ex ()
                                 [(rename . rest)
                                  #'rest]
                                 [one #'((one one))]))
                             exs)]
                       [orig orig])
           #'(provide (all-levels-out orig ex ... ...))))])))

(define-syntax all-levels-out
  (make-provide-transformer
   (lambda (stx mode)
     (syntax-case stx ()
       [(_ orig (local-id ext-id) ...)
        (let* ([table (make-hash-table)]
               [map-id (lambda (phase)
                         (lambda (id)
                           (let ([l (hash-table-get table (syntax-e id) null)])
                             (unless (ormap (lambda (e)
                                              (and (equal? (cdr e) phase)
                                                   (free-identifier=? (car e) id phase)))
                                            l)
                               (hash-table-put! table
                                                (syntax-e id)
                                                (cons (cons id phase) l))))))])
          (let-values ([(ids for-syntax-ids) (syntax-local-module-defined-identifiers)])
            (for-each (map-id 0) ids)
            (for-each (map-id 1) for-syntax-ids))
          (for-each (lambda (l)
                      (for-each (map-id (car l)) (cdr l)))
                    (syntax-local-module-required-identifiers #f #t))
          (apply
           append
           (map (lambda (local-id ext-id)
                  (let* ([l (hash-table-get table (syntax-e local-id)
                                            (lambda ()
                                              (raise-syntax-error
                                               #f
                                               "no binding for exported identifier"
                                               #'orig
                                               local-id)))]
                         [l (filter (lambda (e)
                                      (free-identifier=? (car e) local-id (cdr e)))
                                    l)])
                    (unless l
                      (raise-syntax-error
                       #f
                       "identifier not defined or imported"
                       #'orig
                       local-id))
                    (map (lambda (e)
                           (make-export local-id
                                        (syntax-e ext-id)
                                        (cdr e)
                                        #f
                                        local-id))
                         l)))
                (syntax->list #'(local-id ...))
                (syntax->list #'(ext-id ...)))))]))))
