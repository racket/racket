#lang racket/base

(provide get-top-level-collection-paths)

(require racket/string racket/list racket/path setup/dirs setup/link)

(define (add-directory-collections c s)
  (if (directory-exists? c)
      (for/fold ([s s]) ([p (in-list (directory-list c))]
                         #:when (directory-exists? (build-path c p))
                         #:when (regexp-match? #rx#"^[a-zA-Z_%+-]*$" p))
        (hash-set s (build-path c (path-element->string p)) #t))
      s))

(define (get-top-level-collection-paths)
  (hash-keys
   (for/fold ([s (hash)]) ([l (in-list
                               (current-library-collection-links))])
     (cond
      [(not l)
       (for/fold ([s s]) ([c (in-list
                              (current-library-collection-paths))])
         (add-directory-collections c s))]
      [(path? l)
       (let ([s (for*/fold ([s s]) ([c (in-list (links #:file l #:root? #f #:with-path? #t))])
                  (hash-set s (cdr c) #t))])
         (for*/fold ([s s]) ([c (in-list (links #:file l #:root? #t))])
           (add-directory-collections c s)))]
      [else (error 'get-top-level-collection-paths
                   "unexpected value in `current-library-collection-links': ~e"
                   l)]))))

(module+ main
  (define var "RACKET_COMPLETION_CLIENT")
  (define shell (getenv var))
  (cond [(equal? shell "bash")
         (for ([p (get-top-level-collection-paths)])
           (define-values [base name dir?] (split-path p))
           (displayln name))]
        [(equal? shell "zsh")
         (for-each displayln
                   (remove-duplicates
                    (map path-only (get-top-level-collection-paths))))]
        [else (error 'list-collects "unknown shell (missing ~a envvar)" var)]))
