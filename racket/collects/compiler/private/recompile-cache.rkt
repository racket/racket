#lang racket/base
(require racket/file
         racket/fasl
         "cm-file.rkt")

(provide load-cached-recompile
         load-cached-recompile-summary
         save-cached-recompile)

(define (load-cached-recompile recompile-cache-dir sha1 orig-path)
  (cond
    [(not recompile-cache-dir) #f]
    [else
     (define p (make-path recompile-cache-dir sha1))
     (cond
       [(file-exists? p) (file->bytes p)]
       [else #f])]))

(define summary-extension #"_opt")

(define (load-cached-recompile-summary recompile-cache-dir sha1 orig-path)
  (define summary-p (path-add-suffix (make-path recompile-cache-dir sha1) summary-extension))
  (call-with-input-file* summary-p fasl->s-exp))

(define (save-cached-recompile recompile-cache-dir sha1 zo-path summary)
  (when (and recompile-cache-dir sha1)
    (define bstr (file->bytes zo-path))
    (define p (make-path recompile-cache-dir sha1))
    (define summary-p (path-add-suffix p summary-extension))
    (define-values (dir name dir?) (split-path p))
    (make-directory* dir)
    (with-compile-output summary-p (lambda (o tmp-name) (s-exp->fasl summary o)))
    (with-compile-output p (lambda (o tmp-name) (write-bytes bstr o)))))

(define (make-path recompile-cache-dir sha1)
  (define mach-dir (build-path recompile-cache-dir (format "~a" (current-compile-target-machine))))
  (define pre (substring sha1 0 2))
  (define post (substring sha1 2))
  (build-path mach-dir pre post))
