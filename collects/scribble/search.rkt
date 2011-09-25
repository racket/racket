(module search racket/base
  (require "struct.rkt"
           "basic.rkt"
           syntax/modcode)

  (provide find-racket-tag
           (rename-out [find-racket-tag find-scheme-tag]))

  (define module-info-cache (make-hasheq))

  (define (module-path-index-rejoin mpi rel-to)
    (let-values ([(name base) (module-path-index-split mpi)])
      (cond
       [(not name) rel-to]
       [(not base) mpi]
       [else
        (module-path-index-join name 
                                (module-path-index-rejoin base rel-to))])))

  (define (try thunk fail-thunk)
    (with-handlers* ([exn:fail? (lambda (exn) (fail-thunk))])
      (thunk)))

  (define (find-racket-tag part ri stx/binding phase-level)
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
    ;; Formerly, we assumed that bidings are defined as originating from some
    ;; module at phase 0. [Maybe it's defined at phase 1 and re-exported
    ;; later for phase 0 (after a require-for-template), in which case the
    ;; re-exporting module is the one we find.] That assumption has been
    ;; lifted, however; search for "GONE" below.
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
       (let ([seen (make-hash)]
             [search-key #f])
         (let loop ([queue (list (list (caddr b) (cadddr b) (list-ref b 4) (list-ref b 5) (list-ref b 6)))]
                    [rqueue null]
                    [need-result? #t])
           (cond
            [(null? queue)
             (if (null? rqueue)
                 ;; Not documented
                 #f
                 (loop (reverse rqueue) null need-result?))]
            [else
             (let ([mod (list-ref (car queue) 0)]
                   [id (list-ref (car queue) 1)]
                   [defn-phase (list-ref (car queue) 2)]
                   [import-phase (list-ref (car queue) 3)]
                   [export-phase (list-ref (car queue) 4)]
                   [queue (cdr queue)])
               (let* ([rmp (module-path-index-resolve mod)]
                      [eb (and ;; GONE: (equal? 0 export-phase) ;; look for the phase-0 export; good idea?
                               (list (module-path-index->taglet mod)
                                     id))])
                 (when (and eb
                            (not search-key))
                   (set! search-key eb))
                 (let ([v (and eb (resolve-search search-key part ri `(dep ,eb)))])
                   (let* ([here-result
                           (and need-result?
                                v
                                (let ([v (resolve-get/tentative part ri `(form ,eb))])
                                  (or (and v `(form ,eb))
                                      `(def ,eb))))]
                          [need-result? (and need-result? (not here-result))])
                     ;; Even if we've found `here-result', look deeper so that we have 
                     ;; consistent `dep' results.
                     (let ([nest-result
                            ;; Maybe it's re-exported from this module...
                            ;; Try a shortcut:
                            (if (eq? rmp (and (car b) (module-path-index-resolve (car b))))
                                ;; Not defined through this path, so keep looking
                                (loop queue rqueue need-result?)
                                ;; Check parents, if we can get the source:
                                (if (and (path? (resolved-module-path-name rmp))
                                         (not (hash-ref seen (cons export-phase rmp) #f)))
                                    (let ([exports
                                           (hash-ref
                                            module-info-cache
                                            rmp
                                            (lambda ()
                                              (let-values ([(valss stxess)
                                                            (try
                                                             (lambda ()
                                                               ;; First, try using bytecode:
                                                               (module-compiled-exports 
                                                                (get-module-code (resolved-module-path-name rmp)
                                                                                 #:choose (lambda (src zo so) 'zo))))
                                                             (lambda ()
                                                               (try
                                                                (lambda ()
                                                                  ;; Bytecode not available. Declaration in the
                                                                  ;; current namespace?
                                                                  (module->exports rmp))
                                                                (lambda ()
                                                                  (values null null)))))])
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
                                      (hash-set! seen (cons export-phase rmp) #t)
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
                                                               (reverse (cadr a)))
                                                          rqueue)
                                                  need-result?)
                                            (begin
                                              ;; A dead end may not be our fault: the files could
                                              ;; have changed in inconsistent ways. So just say #f
                                              ;; for now.
                                              #;
                                              (error 'find-racket-tag
                                                     "dead end when looking for binding source: ~e"
                                                     id)
                                              (loop queue rqueue need-result?)))))
                                    ;; Can't get the module source, so continue with queue:
                                    (loop queue rqueue need-result?)))])
                       (or here-result
                           nest-result))))))])))))))
