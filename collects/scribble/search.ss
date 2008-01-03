(module search scheme/base
  (require "struct.ss"
           "basic.ss"
           setup/main-collects
           syntax/modcode)

  (provide find-scheme-tag)

  (define module-info-cache (make-hash-table))

  (define (module-path-index-rejoin mpi rel-to)
    (let-values ([(name base) (module-path-index-split mpi)])
      (cond
       [(not name) rel-to]
       [(not base) mpi]
       [else
        (module-path-index-join name 
                                (module-path-index-rejoin base rel-to))])))

  ;; mode is #f, 'for-label, or 'for-run
  (define (find-scheme-tag part ri stx/binding mode)
    (let ([b (cond
              [(identifier? stx/binding)
               ((case mode
                  [(for-label) identifier-label-binding]
                  [(for-syntax) identifier-transformer-binding]
                  [else identifier-binding])
                stx/binding)]
              [(and (list? stx/binding)
                    (= 6 (length stx/binding)))
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
                            #f
                            (if (= 2 (length stx/binding))
                                mode
                                (caddr stx/binding)))))])])
      (and 
       (pair? b)
       (let ([seen (make-hash-table)]
             [search-key #f])
         (let loop ([queue (list (list (caddr b) (cadddr b) (eq? mode (list-ref b 5))))]
                    [rqueue null])
           (cond
            [(null? queue)
             (if (null? rqueue)
                 ;; Not documented
                 #f
                 (loop (reverse rqueue) null))]
            [else
             (let ([mod (caar queue)]
                   [id (cadar queue)]
                   [here? (caddar queue)]
                   [queue (cdr queue)])
               (let* ([rmp (module-path-index-resolve mod)]
                      [eb (and here?
                               (format "~a::~a"
                                       (let ([p (resolved-module-path-name rmp)])
                                         (if (path? p)
                                             (path->main-collects-relative p)
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
                                         (let-values ([(run-vals run-stxes
                                                                 syntax-vals syntax-stxes
                                                                 label-vals label-stxes)
                                                       (module-compiled-exports
                                                        (get-module-code (resolved-module-path-name rmp)))])
                                           (let ([t (list (append run-vals run-stxes)
                                                          (append syntax-vals syntax-stxes)
                                                          (append label-vals label-stxes))])
                                             (hash-table-put! module-info-cache rmp t)
                                             t))))])
                                 (hash-table-put! seen rmp #t)
                                 (let ([a (assq id (list-ref exports
                                                             (if here?
                                                                 0
                                                                 (case mode
                                                                   [(for-syntax) 1]
                                                                   [(for-label) 2]
                                                                   [else 0]))))])
                                   (if a
                                       (loop queue
                                             (append (map (lambda (m)
                                                            (if (pair? m)
                                                                (list (module-path-index-rejoin (car m) mod)
                                                                      (caddr m) 
                                                                      (or here?
                                                                          (eq? mode (cadr m))))
                                                                (list (module-path-index-rejoin m mod)
                                                                      id
                                                                      here?)))
                                                          (cadr a))
                                                     rqueue))
                                       (error 'find-scheme-tag
                                              "dead end when looking for binding source: ~e"
                                              id))))
                               ;; Can't get the module source, so continue with queue:
                               (loop queue rqueue)))))))])))))))