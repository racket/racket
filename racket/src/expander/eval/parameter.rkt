#lang racket/base
(require racket/private/check
         "../common/module-path.rkt")

(provide current-eval
         current-compile
         current-load
         current-load/use-compiled
         
         current-library-collection-paths
         current-library-collection-links

         use-compiled-file-paths
         current-compiled-file-roots
         use-compiled-file-check
         use-collection-link-paths
         use-user-specific-search-paths)

(define (replace-me who)
  (lambda args
    (error who "this stub must be replaced")))

(define/who current-eval
  (make-parameter (replace-me who)
                  (lambda (p)
                    (check who (procedure-arity-includes/c 1) p)
                    p)))

(define/who current-compile
  (make-parameter (replace-me who)
                  (lambda (p)
                    (check who (procedure-arity-includes/c 2) p)
                    p)))

(define/who current-load
  (make-parameter (replace-me who)
                  (lambda (p)
                    (check who (procedure-arity-includes/c 2) p)
                    p)))

(define/who current-load/use-compiled
  (make-parameter (replace-me who)
                  (lambda (p)
                    (check who (procedure-arity-includes/c 2) p)
                    p)))

(define/who current-library-collection-paths
  (make-parameter null
                  (lambda (l)
                    (check who (lambda (l)
                                 (and (list? l)
                                      (andmap complete-path-string? l)))
                           #:contract "(listof (and/c path-string? complete-path?))"
                           l)
                    (map to-path l))))

(define/who current-library-collection-links
  (make-parameter null
                  (lambda (l)
                    (check who (lambda (l)
                                 (and (list? l)
                                      (andmap (lambda (p)
                                                (or (not p)
                                                    (complete-path-string? p)
                                                    (and (hash? p)
                                                         (for/and ([(k v) (in-hash p)])
                                                           (and (or (not k)
                                                                    (and (symbol? k) (module-path? k)))
                                                                (list? v)
                                                                (andmap complete-path-string? v))))))
                                              l)))
                           
                           #:contract (string-append
                                       "(listof (or/c #f\n"
                                       "              (and/c path-string? complete-path?)\n"
                                       "              (hash/c (or/c (and/c symbol? module-path?) #f)\n"
                                       "                      (listof (and/c path-string? complete-path?)))))")
                           l)
                    (map (lambda (p)
                           (cond
                             [(not p) #f]
                             [(path? p) p]
                             [(string? p) (string->path p)]
                             [else
                              (for/hash ([(k v) (in-hash p)])
                                (values k (to-path v)))]))
                         l))))

(define/who use-compiled-file-paths
  (make-parameter (list (string->path "compiled"))
                  (lambda (l)
                    (check who (lambda (l)
                                 (and (list? l)
                                      (andmap relative-path-string? l)))
                           #:contract "(listof (and/c path-string? relative-path?))"
                           l)
                    (map to-path l))))

(define/who current-compiled-file-roots
  (make-parameter '(same)
                  (lambda (l)
                    (check who (lambda (l)
                                 (and (list? l)
                                      (andmap (lambda (p)
                                                (or (path-string? p)
                                                    (eq? p 'same)))
                                              l)))
                           #:contract "(listof (or/c path-string? 'same))"
                           l)
                    (map to-path l))))

(define/who use-compiled-file-check
  (make-parameter 'modify-seconds
                  (lambda (v)
                    (check who (lambda (v) (or (eq? v 'modify-seconds) (eq? v 'exists)))
                           #:contract "(or/c 'modify-seconds 'exists)"
                           v)
                    v)))

(define use-collection-link-paths
  (make-parameter #t (lambda (v) (and v #t))))

(define use-user-specific-search-paths
  (make-parameter #t (lambda (v) (and v #t))))

(define (complete-path-string? p)
  (and (path-string? p) (complete-path? p)))

(define (relative-path-string? p)
  (and (path-string? p) (relative-path? p)))

(define (to-path p)
  (if (string? p) (string->path p) p))
