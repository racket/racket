(module search scheme/base
  (require "struct.ss"
           "basic.ss"
           setup/main-collects
           syntax/modcode)

  (provide find-scheme-tag
           intern-taglet)

  (define module-info-cache (make-hash-table))

  (define (module-path-index-rejoin mpi rel-to)
    (let-values ([(name base) (module-path-index-split mpi)])
      (cond
       [(not name) rel-to]
       [(not base) mpi]
       [else
        (module-path-index-join name 
                                (module-path-index-rejoin base rel-to))])))

  (define interned (make-hash-table 'equal 'weak))
  
  (define (intern-taglet v)
    (let ([v (if (list? v)
                 (map intern-taglet v)
                 v)])
      (if (or (string? v)
              (bytes? v)
              (list? v))
          (let ([b (hash-table-get interned v #f)])
            (if b
                (weak-box-value b)
                (begin
                  (hash-table-put! interned v (make-weak-box v))
                  v)))
          v)))
     

  (define (find-scheme-tag part ri stx/binding phase-level)
    ;; The phase-level argument is used only when `stx/binding'
    ;; is an identifier.
    ;;
    ;; Note: documentation key currently don't distinguish different
    ;; phase definitions of an identifier from a source module.
    ;; That is, there's no way to document (define x ....) differently
    ;; from (define-for-syntax x ...). This isn't a problem in practice,
    ;; because no one uses the same name for different-phase exported
    ;; bindings.
    (let ([b (cond
              [(identifier? stx/binding)
               (identifier-binding stx/binding phase-level)]
              [(and (list? stx/binding)
                    (= 7 (length stx/binding)))
               stx/binding]
              [else
               (and (not (symbol? (car stx/binding)))
                    (let ([p (module-path-index-join 
                              (main-collects-relative->path (car stx/binding))
                              #f)])
                      (list #f
                            (cadr stx/binding)
                            p
                            (cadr stx/binding)
                            (if (= 2 (length stx/binding))
                                0
                                (caddr stx/binding))
                            (if (= 2 (length stx/binding))
                                0
                                (cadddr stx/binding))
                            (if (= 2 (length stx/binding))
                                0
                                (cadddr (cdr stx/binding))))))])])
      (and 
       (pair? b)
       (let ([seen (make-hash-table)]
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
                      [eb (and (equal? defn-phase export-phase)
                               (list (let ([p (resolved-module-path-name rmp)])
                                       (if (path? p)
                                           (intern-taglet (path->main-collects-relative p))
                                           p))
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
                                    (not (hash-table-get seen rmp #f)))
                               (let ([exports
                                      (hash-table-get 
                                       module-info-cache
                                       rmp
                                       (lambda ()
                                         (let-values ([(valss stxess)
                                                       (module-compiled-exports
                                                        (get-module-code (resolved-module-path-name rmp)))])
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
                                             (hash-table-put! module-info-cache rmp t)
                                             t))))])
                                 (hash-table-put! seen rmp #t)
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
                                       (error 'find-scheme-tag
                                              "dead end when looking for binding source: ~e"
                                              id))))
                               ;; Can't get the module source, so continue with queue:
                               (loop queue rqueue)))))))])))))))
