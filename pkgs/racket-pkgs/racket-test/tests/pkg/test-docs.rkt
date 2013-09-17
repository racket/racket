#lang racket/base
(require setup/xref
         scribble/xref
         racket/path
         net/url
         rackunit)

;; The `test-docs-...' functions are meant to be called via "tests-binary.rkt"
(provide test-docs-x
         test-docs-y
         test-docs-r)

(define xref (load-collections-xref))

(define (test here? tag)
  (check-equal? here? (and tag #t))
  (when tag
    (define-values (p a) (xref-tag->path+anchor xref tag))
    (check-true (path? p))
    (call-with-input-file* 
     p
     (lambda (in)
       (define m (regexp-match #rx"<script [^>]*src=\"(?:file://)?([^\"]*)local-redirect.js\"[^>]*>" in))
       (define ref (url->path
                    (string->url
                     (bytes->string/utf-8
                      (if (equal? (cadr m) #"")
                          #"."
                          (cadr m))))))
       (define path (if (absolute-path? ref)
                        (build-path ref
                                    "local-redirect.js")
                        (build-path (path-only p)
                                    ref
                                    "local-redirect.js")))
       (check-true (file-exists? path))))))

(define (test-docs-x here?)
  (when (collection-path "x" #:fail (lambda (x) #f))
    (namespace-require '(for-label x)))
  (test here? (xref-binding->definition-tag xref (eval '#'x) #f)))

(define (test-docs-y here?)
  (when (collection-path "y" #:fail (lambda (x) #f))
    (namespace-require '(for-label y)))
  (test here? (xref-binding->definition-tag xref (eval '#'y) #f)))

(define (test-docs-r here?)
  (namespace-require '(for-label racket/base))
  (test here? (xref-binding->definition-tag xref (eval '#'lambda) #f)))
