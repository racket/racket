#lang racket/base
(require racket/contract
         setup/link
         racket/match
         setup/collection-name
         racket/list)
(provide collection-paths)

(struct col (name path) #:transparent)

(define (get-linked file user? version?)
  (define version-re
    (and version?
         (regexp-quote (version))))
  (append
   (for/list ([c+p
               (in-list
                (links #:file file #:user? user? #:version-regexp version-re
                       #:with-path? #t))])
     (col (car c+p)
          (cdr c+p)))
   (for/list ([cp
               (in-list
                (links #:file file #:user? user? #:version-regexp version-re
                       #:root? #t))]
              #:when (directory-exists? cp)
              [collection (in-list (directory-list cp))]
              #:when (directory-exists? (build-path cp collection)))
     (col (path->string collection)
          (build-path cp collection)))))

;; A list of `col's, where each collection may be represented
;; by multiple elements of the list, each with its own path.
(define (all-collections)
  (remove-duplicates
   (append*
    (for/list ([cp (current-library-collection-paths)]
               #:when (directory-exists? cp)
               [collection (in-list (directory-list cp))]
               #:when (directory-exists? (build-path cp collection)))
      (col (path->string collection)
           (build-path cp collection)))
    (for*/list ([file (in-list (current-library-collection-links))]
                [user? (in-list '(#t #f))]
                [version? (in-list '(#t #f))])
      (get-linked file user? version?)))))

;; This returns all the collection paths, rather than just the first
;; as collection-path does.
(define (collection-paths c)
  (when (not (collection-name? c))
    (error 'collection-paths "not a collection name in: ~a" c))
  (match-define (list-rest sc more) (map path->string (explode-path c)))
  (append*
   (for/list ([col (all-collections)]
              #:when (string=? sc (col-name col)))
     (define p (col-path col))
     (define cp (apply build-path p more))
     (if (directory-exists? cp)
       (list cp)
       empty))))
