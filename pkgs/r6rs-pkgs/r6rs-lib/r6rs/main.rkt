#lang scheme/base

#|
FIXME:
 * Check that exported identifiers are defined in some phase.
 * Check that each identifier is imported only once across phases.
|#

(require (for-syntax scheme/base
                     syntax/kerncase
                     "private/parse-ref.rkt"
                     scheme/provide-transform)
         "private/no-set.rkt")

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
               (cons #'#%require 
                     (kernel-form-identifier-list)))])
       (syntax-case a (begin #%require)
         [(def . _)
          (ormap (lambda (id)
                   (and (identifier? #'def)
                        (free-identifier=? id #'def)))
                 (list #'define-values
                       #'define-syntaxes
                       #'begin-for-syntax))
          #`(begin #,a (library-body/defns . more))]
         [(#%require . _)
          ;; We allow `require' mixed with definitions, because it
          ;; might reasonably be introduced by a macro.
          #`(begin #,a (library-body/defns . more))]
         [(begin sub ...)
          #`(library-body/defns sub ... . more)]
         [else
          #`(begin (let () #,a . more))]))]
    [(_) #'(begin)]))

;; ----------------------------------------
;; Imports and exports

(define-syntax (r6rs-import stx)
  (let ([orig (syntax-case stx ()
                [(_ orig) #'orig])])
    (syntax-case stx (import)
      [(_ (import im ...))
       (with-syntax ([((im ...) ...)
                      (map (lambda (im)
                             (parse-import
                              orig
                              im
                              (lambda (msg orig stx)
                                (raise-syntax-error #f
                                                    msg
                                                    orig
                                                    stx))))
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
         (add-no-set!-identifiers (apply 
                                   append
                                   (map (lambda (ex)
                                          (syntax-case ex (rename)
                                            [(rename (id ex-id) ...)
                                             (syntax->list #'(id ...))]
                                            [id (list ex)]))
                                        exs)))
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
        (let* ([table (make-hasheq)]
               [map-id (lambda (phase)
                         (lambda (id)
                           (let ([l (hash-ref table (syntax-e id) null)])
                             (unless (ormap (lambda (e)
                                              (and (equal? (cdr e) phase)
                                                   (free-identifier=? (car e) id phase)))
                                            l)
                               (hash-set! table
                                          (syntax-e id)
                                          (cons (cons id phase) l))))))])
          (for ([(phase ids) (in-hash (syntax-local-module-defined-identifiers))])
            (for-each (map-id phase) ids))
          (for-each (lambda (l)
                      (if (car l)
                          (for-each (map-id (car l)) (cdr l))
                          null))
                    (syntax-local-module-required-identifiers #f #t))
          (apply
           append
           (map (lambda (local-id ext-id)
                  (let* ([l (hash-ref table (syntax-e local-id)
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
