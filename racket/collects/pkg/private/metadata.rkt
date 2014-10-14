#lang racket/base
(require version/utils
         racket/format
         racket/list
         racket/set
         setup/collection-name
         "../name.rkt"
         "get-info.rkt"
         "print.rkt"
         "dep.rkt")

;; Extracting information from a package's "info.rkt" file.

(provide (all-defined-out))

(define ((check-dependencies which) deps)
  (unless (and (list? deps)
               (for/and ([dep (in-list deps)])
                 (define (package-source? dep)
                   (and (string? dep)
                        (package-source->name dep)))
                 (define (version? s)
                   (and (string? s)
                        (valid-version? s)))
                 (or (package-source? dep)
                     (and (list? dep)
                          (= 2 (length dep))
                          (package-source? (car dep))
                          (version? (cadr dep)))
                     (and (list? dep)
                          ((length dep) . >= . 1)
                          (odd? (length dep))
                          (package-source? (car dep))
                          (let loop ([saw (hash)] [dep (cdr dep)])
                            (cond 
                             [(null? dep) #t]
                             [(hash-ref saw (car dep) #f) #f]
                             [else
                              (define kw (car dep))
                              (define val (cadr dep))
                              (and
                               (cond
                                [(eq? kw '#:version) (version? val)]
                                [(eq? kw '#:platform)
                                 (or (string? val)
                                     (regexp? val)
                                     (memq val '(unix windows macosx)))]
                                [else #f])
                               (loop (hash-set saw (car dep) #t)
                                     (cddr dep)))]))))))
    (pkg-error (~a "invalid `" which "' specification\n"
                   "  specification: ~e")
               deps)))

(define (get-all-deps* metadata-ns pkg-dir)
  (values
   (get-metadata metadata-ns pkg-dir 
                 'deps (lambda () empty)
                 #:checker (check-dependencies 'deps))
   (get-metadata metadata-ns pkg-dir 
                 'build-deps (lambda () empty)
                 #:checker (check-dependencies 'build-deps))))

(define (get-all-deps metadata-ns pkg-dir)
  (define-values (deps build-deps) (get-all-deps* metadata-ns pkg-dir))
  (append deps build-deps))

(define (get-all-deps-subset key metadata-ns pkg-dir deps)
  (get-metadata metadata-ns pkg-dir 
                key (lambda () empty)
                #:checker (lambda (l)
                            (unless (null? l)
                              (define deps-set (list->set
                                                (map dependency->name deps)))
                              (unless (and (list? l)
                                           (andmap (lambda (v)
                                                     (or (string? v)
                                                         (eq? v 'core)))
                                                   l))
                                (pkg-error (~a "invalid `~a' specification\n"
                                               "  specification: ~e")
                                           key
                                           l))
                              (unless (andmap (lambda (i)
                                                (or (eq? i 'core)
                                                    (set-member? deps-set i)))
                                              l)
                                (pkg-error (~a "`~a' is not a subset of dependencies\n"
                                               "  specification: ~e")
                                           key
                                           l))))))

(define (get-all-implies metadata-ns pkg-dir deps)
  (get-all-deps-subset 'implies metadata-ns pkg-dir deps))

(define (get-all-update-implies metadata-ns pkg-dir deps)
  (get-all-deps-subset 'update-implies metadata-ns pkg-dir deps))

(define (pkg-single-collection dir 
                               #:name [pkg-name (let-values ([(base name dir?) (split-path dir)])
                                                  (path-element->string name))]
                               #:namespace [metadata-ns (make-metadata-namespace)])
  (define i (get-pkg-info dir metadata-ns))
  (if (not i)
      pkg-name
      (let ([s (i 'collection (lambda () 'use-pkg-name))])
        (unless (or (collection-name-element? s)
                    (eq? s 'multi)
                    (eq? s 'use-pkg-name))
          (log-error (format (~a "bad `collection' definition in \"info.rkt\";\n"
                                 " definition will be ignored\n"
                                 "  path: ~a\n"
                                 "  found: ~e\n"
                                 "  expected: (or/c collection-name-element? 'multi 'use-pkg-name)")
                             (build-path dir "info.rkt")
                             s)))
        (or (and (collection-name-element? s)
                 s)
            (and (eq? s 'use-pkg-name)
                 pkg-name)))))
