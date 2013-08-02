#lang racket/base
(require racket/string setup/dirs setup/link)

(define (add-directory-collections c s)
  (if (directory-exists? c)
      (for/fold ([s s]) ([p (in-list (directory-list c))]
                         #:when (directory-exists? (build-path c p))
                         #:when (regexp-match? #rx#"^[a-zA-Z_%+-]*$" p))
        (hash-set s (path-element->string p) #t))
      s))

(define (get-all-top-level-collections)
  (hash-keys
   (for/fold ([s (hash)]) ([l (in-list
                               (current-library-collection-links))])
     (cond
      [(not l)
       (for/fold ([s s]) ([c (in-list
                              (current-library-collection-paths))])
         (add-directory-collections c s))]
      [(path? l)
       (let ([s (for*/fold ([s s]) ([c (in-list (links #:file l #:root? #f))])
                  (hash-set s c #t))])
         (for*/fold ([s s]) ([c (in-list (links #:file l #:root? #t))])
           (add-directory-collections c s)))]
      [else (error 'get-all-top-level-collections
                   "unexpected value in `current-library-collection-links': ~e"
                   l)]))))

(for-each displayln (get-all-top-level-collections))
