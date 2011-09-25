#lang racket/base
; Responsible: Jay McCarthy
(require racket/contract/base)

(define (exn:fail:filesystem:exists? x)
  (and (exn:fail:filesystem? x)
       (regexp-match #rx"exists" (exn-message x))))

(define (make-directory*/ignore-exists-exn dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base)
               (not (directory-exists? base)))
      (make-directory*/ignore-exists-exn base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

(provide/contract
 [make-directory*/ignore-exists-exn (path-string? . -> . void)])
