#lang racket/base
(require compiler/compilation-path
         compiler/private/dep
         setup/collects)

(provide module-recorded-dependencies)

(define (module-recorded-dependencies path)
  (define collection-cache (make-hash))
  (define (module-dependencies path all-deps)
    (define dep-path (path-add-extension (get-compilation-path path) #".dep"))
    (define deps (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)]
                                 [exn:fail:read? (lambda (exn) #f)])
                   (call-with-input-file* dep-path read)))
    (for/fold ([all-deps all-deps]) ([dep (in-list (if (and (list? deps)
                                                            (pair? deps)
                                                            (pair? (cdr deps))
                                                            (pair? (cddr deps)))
                                                       (cdddr deps)
                                                       '()))])
      (define p (collects-relative*->path (dep->encoded-path dep) collection-cache))
      (cond
        [(hash-ref all-deps p #f) all-deps]
        [else
         (define new-deps (hash-set all-deps p #t))
         (cond
           [(external-dep? dep) new-deps]
           [else (module-dependencies p new-deps)])])))
  (hash-keys (module-dependencies (simplify-path path) #hash())))
  
(define (get-compilation-path path)
  (define-values (dir name) (get-compilation-dir+name path))
  (build-path dir name))

(define (collects-relative*->path p cache)
  (if (bytes? p)
      (bytes->path p)
      (hash-ref! cache p (lambda () (collects-relative->path p)))))
