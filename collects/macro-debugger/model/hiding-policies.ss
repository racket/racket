
(module hiding-policies mzscheme
  (require (lib "plt-match.ss")
           (lib "boundmap.ss" "syntax"))
  (provide (all-defined))

  (define-struct hiding-policy
    (opaque-modules opaque-ids opaque-kernel opaque-libs transparent-ids))

  (define (policy-hide-module p m)
    (hash-table-put! (hiding-policy-opaque-modules p) m #t))
  (define (policy-unhide-module p m)
    (hash-table-remove! (hiding-policy-opaque-modules p) m))

  (define (policy-hide-kernel p)
    (set-hiding-policy-opaque-kernel! p #t))
  (define (policy-unhide-kernel p)
    (set-hiding-policy-opaque-kernel! p #f))

  (define (policy-hide-libs p)
    (set-hiding-policy-opaque-libs! p #t))
  (define (policy-unhide-libs p)
    (set-hiding-policy-opaque-libs! p #f))

  (define (policy-hide-id p id)
    (policy-unshow-id p id)
    (module-identifier-mapping-put! (hiding-policy-opaque-ids p) id #t))
  (define (policy-unhide-id p id)
    (module-identifier-mapping-put! (hiding-policy-opaque-ids p) id #f))

  (define (policy-show-id p id)
    (policy-unhide-id p id)
    (module-identifier-mapping-put! (hiding-policy-transparent-ids p) id #t))
  (define (policy-unshow-id p id)
    (module-identifier-mapping-put! (hiding-policy-transparent-ids p) id #f))

  (define (new-hiding-policy)
    (make-hiding-policy (make-hash-table)
                        (make-module-identifier-mapping)
                        #f
                        #f
                        (make-module-identifier-mapping)))
  
  (define (new-standard-hiding-policy)
    (let ([p (new-hiding-policy)])
      (policy-hide-kernel p)
      (policy-hide-libs p)
      p))

  ;; ---
  
  (define-syntax inline
    (syntax-rules ()
      [(inline ([name expr] ...) . body)
       (let-syntax ([name
                     (lambda (x) 
                       (syntax-case x ()
                         [xx (identifier? #'xx) #'expr]))] ...)
         . body)]))

  (define (/false) #f)

  (define (policy-show-macro? policy id)
    (match policy
      [(struct hiding-policy (opaque-modules
                              opaque-identifiers
                              opaque-kernel
                              opaque-libs
                              transparent-identifiers))
       (inline ([not-opaque-id
                 (not (module-identifier-mapping-get opaque-identifiers id /false))]
                [transparent-id
                 (module-identifier-mapping-get transparent-identifiers id /false)])
         (let ([binding (identifier-binding id)])
           (if (list? binding)
               (let-values ([(srcmod srcname nommod nomname _) (apply values binding)])
                 (inline ([opaque-srcmod (hash-table-get opaque-modules srcmod /false)]
                          [opaque-nommod (hash-table-get opaque-modules nommod /false)]
                          ;; FIXME
                          [in-kernel?
                           (and (symbol? srcmod)
                                (eq? #\# (string-ref (symbol->string srcmod) 0)))]
                          [in-lib-module?
                           (lib-module? srcmod)])
                   (or transparent-id
                       (and (not opaque-srcmod)
                            (not opaque-nommod)
                            (not (and in-kernel? opaque-kernel))
                            (not (and in-lib-module? opaque-libs))
                            not-opaque-id))))
               (or transparent-id
                   not-opaque-id))))]))

  (define (lib-module? mpi)
    (and (module-path-index? mpi)
         (let-values ([(path rel) (module-path-index-split mpi)])
           (cond [(pair? path) (memq (car path) '(lib planet))]
                 [(string? path) (lib-module? rel)]
                 [else #f]))))
  )