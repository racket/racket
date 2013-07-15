#lang racket/base
(require racket/string setup/dirs setup/link)

(define (add-directory-collections c s)
  (if (directory-exists? c)
      (for/fold ([s s]) ([p (in-list (directory-list c))]
                         #:when (directory-exists? (build-path c p))
                         #:when (regexp-match? #rx#"^[a-zA-Z_%+-]*$" p))
        (hash-set s (path-element->string p) #t))
      s))

(define (links* m root?)
  (case m
    [(user) (links #:user? #t #:root? root?)]
    [(shared) (links #:shared? #t #:root? root?)]
    [else (links #:file m #:root? root?)]))

(define (get-all-top-level-collections)
  (define link-modes (list* 'user 'shared (get-links-search-files)))

  (let* ([s (hash)]
         [s (for/fold ([s s]) ([c (in-list
                                   (current-library-collection-paths))])
              (add-directory-collections c s))]
         [s (for*/fold ([s s]) ([m (in-list link-modes)]
                                [l (in-list (links* m #f))])
              (hash-set s l #t))]
         [s (for*/fold ([s s]) ([m (in-list link-modes)]
                                [c (in-list (links* m #t))])
              (add-directory-collections c s))])
    (hash-keys s)))

(for-each displayln (get-all-top-level-collections))
