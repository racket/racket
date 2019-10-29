#lang racket/base
(require setup/setup
         setup/dirs)

(provide materialize-user-docs)

(define (materialize-user-docs [wrap-setup (lambda (t) (t))]
                               #:skip-user-doc-check? [skip-user-doc-check? #f])
  (when (or skip-user-doc-check?
            (not (file-exists? (build-path (find-user-doc-dir) "index.html"))))
    (wrap-setup
     (lambda ()
       (setup #:avoid-main? #t
              #:make-doc-index? #t
              #:force-user-docs? #t)))
    (void)))
