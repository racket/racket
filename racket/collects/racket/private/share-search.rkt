#lang racket/base
(require setup/dirs)

(provide share-spec? share-find)

(define (share-spec? p)
  (and (list? p)
       (= 2 (length p))
       (eq? 'share (car p))
       (string? (cadr p))))

(define (share-find p)
  (define f (cadr p))
  (define (search dir)
    (and dir
         (let ([p (build-path dir f)])
           (and (or (file-exists? p)
                    (directory-exists? p))
                p))))
  (or (search (find-user-share-dir))
      (search (find-share-dir))))
