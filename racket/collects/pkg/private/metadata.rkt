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

(define current-dependencies-pkg-dir (make-parameter '|[unknown]|))

(define ((check-dependencies which [pkg-dir (current-dependencies-pkg-dir)]) deps)
  (unless (list? deps)
    (pkg-error (~a "invalid `" which "` specification\n"
                   "  bad specification: ~e\n"
                   "  package directory: ~a")
               deps
               pkg-dir))

  (define (package-source? dep)
    (and (string? dep)
         (package-source->name dep)))

  (define (version? s)
    (and (string? s)
         (valid-version? s)))

  (define (pkg-dep? dep)
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
                         (cddr dep)))])))))

  (define invalid-deps
    (filter (compose1 not pkg-dep?) deps))

  (unless (null? invalid-deps)
    (pkg-error (~a "invalid `" which "` specification\n"
                   "  specification: ~e\n"
                   (if ((length invalid-deps) . = . 1)
                       "  bad dep: ~e"
                       "  bad deps: ~e")
                   "\n  package directory: ~a")
               deps
               (if ((length invalid-deps) . = . 1)
                   (car invalid-deps)
                   invalid-deps)
               pkg-dir)))

(define (get-all-deps* metadata-ns pkg-dir)
  (values
   (get-metadata metadata-ns pkg-dir 
                 'deps (lambda () empty)
                 #:checker (check-dependencies 'deps pkg-dir))
   (get-metadata metadata-ns pkg-dir 
                 'build-deps (lambda () empty)
                 #:checker (check-dependencies 'build-deps pkg-dir))))

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
                                (pkg-error (~a "invalid `~a` specification\n"
                                               "  specification: ~e\n"
                                               "  package directory: ~a")
                                           key
                                           l
                                           pkg-dir))
                              (unless (andmap (lambda (i)
                                                (or (eq? i 'core)
                                                    (set-member? deps-set i)))
                                              l)
                                (pkg-error (~a "`~a` is not a subset of dependencies\n"
                                               "  specification: ~e\n"
                                               "  package directory: ~a")
                                           key
                                           l
                                           pkg-dir))))))

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
          (log-error (format (~a "bad `collection` definition in \"info.rkt\";\n"
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
