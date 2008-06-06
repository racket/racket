(module search scheme/base
  (require "struct.ss"
           "basic.ss"
           setup/main-collects
           syntax/modcode
           syntax/modcollapse
           
           ;; Needed to normalize planet version numbers:
           (only-in planet/resolver get-planet-module-path/pkg)
           (only-in planet/private/data pkg-maj pkg-min))

  (provide find-scheme-tag
           intern-taglet
           module-path-index->taglet)

  (define module-info-cache (make-hasheq))

  (define (module-path-index-rejoin mpi rel-to)
    (let-values ([(name base) (module-path-index-split mpi)])
      (cond
       [(not name) rel-to]
       [(not base) mpi]
       [else
        (module-path-index-join name 
                                (module-path-index-rejoin base rel-to))])))

  (define interned (make-weak-hash))
  
  (define (intern-taglet v)
    (let ([v (if (list? v)
                 (map intern-taglet v)
                 v)])
      (if (or (string? v)
              (bytes? v)
              (list? v))
          (let ([b (hash-ref interned v #f)])
            (if b
                (weak-box-value b)
                (begin
                  (hash-set! interned v (make-weak-box v))
                  v)))
          v)))

  (define (module-path-index->taglet mod)
    ;; Derive the name from the module path:
    (let ([p (collapse-module-path-index
              mod
              (current-directory))])
      (if (path? p)
          ;; If we got a path back anyway, then it's best to use the resolved
          ;; name; if the current directory has changed since we 
          ;; the path-index was resolved, then p might not be right
          (intern-taglet 
           (path->main-collects-relative 
            (resolved-module-path-name (module-path-index-resolve mod))))
          (let ([p (if (and (pair? p)
                            (eq? (car p) 'planet))
                       ;; Normalize planet verion number based on current
                       ;; linking:
                       (let-values ([(path pkg)
                                     (get-planet-module-path/pkg p #f #f)])
                         (list* 'planet
                                (cadr p)
                                (list (car (caddr p))
                                      (cadr (caddr p))
                                      (pkg-maj pkg)
                                      (pkg-min pkg))
                                (cdddr p)))
                       ;; Otherwise the path is fully normalized:
                       p)])
            (intern-taglet p)))))

  (define (find-scheme-tag part ri stx/binding phase-level)
    ;; The phase-level argument is used only when `stx/binding'
    ;; is an identifier.
    ;;
    ;; Note: documentation keys currently don't distinguish different
    ;; phase definitions of an identifier from a source module.
    ;; That is, there's no way to document (define x ....) differently
    ;; from (define-for-syntax x ...). This isn't a problem in practice,
    ;; because no one uses the same name for different-phase exported
    ;; bindings.
    ;;
    ;; However, we assume that bidings are defined as originating from some
    ;; module at phase 0. Maybe it's defined at phase 1 and re-exported
    ;; later for phase 0 (after a require-for-template), in which case the
    ;; re-exporting module is the one we find.
    (let ([b (cond
              [(identifier? stx/binding)
               (identifier-binding stx/binding phase-level)]
              [(and (list? stx/binding)
                    (= 7 (length stx/binding)))
               stx/binding]
              [else
               (and (not (symbol? (car stx/binding)))
                    (list #f
                          (cadr stx/binding)
                          (car stx/binding)
                          (cadr stx/binding)
                          (if (= 2 (length stx/binding))
                              0
                              (caddr stx/binding))
                          (if (= 2 (length stx/binding))
                              0
                              (cadddr stx/binding))
                          (if (= 2 (length stx/binding))
                              0
                              (cadddr (cdr stx/binding)))))])])
      (and 
       (pair? b)
       (let ([seen (make-hasheq)]
             [search-key #f])
         (let loop ([queue (list (list (caddr b) (cadddr b) (list-ref b 4) (list-ref b 5) (list-ref b 6)))]
                    [rqueue null])
           (cond
            [(null? queue)
             (if (null? rqueue)
                 ;; Not documented
                 #f
                 (loop (reverse rqueue) null))]
            [else
             (let ([mod (list-ref (car queue) 0)]
                   [id (list-ref (car queue) 1)]
                   [defn-phase (list-ref (car queue) 2)]
                   [import-phase (list-ref (car queue) 3)]
                   [export-phase (list-ref (car queue) 4)]
                   [queue (cdr queue)])
               (let* ([rmp (module-path-index-resolve mod)]
                      [eb (and (equal? 0 export-phase) ;; look for the phase-0 export; good idea?
                               (list (module-path-index->taglet mod)
                                     id))])
                 (when (and eb
                            (not search-key))
                   (set! search-key eb))
                 (let ([v (and eb (resolve-search search-key part ri `(dep ,eb)))])
                   (or (and v
                            (let ([v (resolve-get/tentative part ri `(form ,eb))])
                              (or (and v `(form ,eb))
                                  `(def ,eb))))
                       ;; Maybe it's re-exported from this module...
                       ;; Try a shortcut:
                       (if (eq? rmp (and (car b) (module-path-index-resolve (car b))))
                           ;; Not defined through this path, so keep looking
                           (loop queue rqueue)
                           ;; Check parents, if we can get the source:
                           (if (and (path? (resolved-module-path-name rmp))
                                    (not (hash-ref seen rmp #f)))
                               (let ([exports
                                      (hash-ref
                                       module-info-cache
                                       rmp
                                       (lambda ()
                                         (let-values ([(valss stxess)
                                                       (with-handlers ([exn:fail?
                                                                        (lambda (exn)
                                                                          (values null null))])
                                                         (module-compiled-exports
                                                          (get-module-code (resolved-module-path-name rmp)
                                                                           #:choose (lambda (src zo so) 'zo))))])
                                           (let ([t 
                                                  ;; Merge the two association lists:
                                                  (let loop ([base valss]
                                                             [stxess stxess])
                                                    (cond
                                                     [(null? stxess) base]
                                                     [(assoc (caar stxess) base)
                                                      => (lambda (l)
                                                           (loop (cons (cons (car l)
                                                                             (append (cdar stxess)
                                                                                     (cdr l)))
                                                                       (remq l base))
                                                                 (cdr stxess)))]
                                                     [else (loop (cons (car stxess)
                                                                       base)
                                                                 (cdr stxess))]))])
                                             (hash-set! module-info-cache rmp t)
                                             t))))])
                                 (hash-set! seen rmp #t)
                                 (let ([a (assq id (let ([a (assoc export-phase exports)])
                                                     (if a
                                                         (cdr a) 
                                                         null)))])
                                   (if a
                                       (loop queue
                                             (append (map (lambda (m)
                                                            (if (pair? m)
                                                                (list (module-path-index-rejoin (car m) mod)
                                                                      (list-ref m 2)
                                                                      defn-phase
                                                                      (list-ref m 1)
                                                                      (list-ref m 3))
                                                                (list (module-path-index-rejoin m mod)
                                                                      id
                                                                      0
                                                                      0
                                                                      0)))
                                                          (cadr a))
                                                     rqueue))
                                       (begin
                                         ;; A dead end may not be our fault: the files could
                                         ;; have changed in inconsistent ways. So just say #f
                                         ;; for now.
                                         #;
                                         (error 'find-scheme-tag
                                                "dead end when looking for binding source: ~e"
                                                id)
                                         #f))))
                               ;; Can't get the module source, so continue with queue:
                               (loop queue rqueue)))))))])))))))
