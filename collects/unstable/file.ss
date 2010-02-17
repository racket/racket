#lang scheme/base
; Responsible: Jay McCarthy
(require scheme/contract)

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

(define (rename-file-or-directory/ignore-exists-exn from to)
  (with-handlers ([exn:fail:filesystem:exists? void])
    (rename-file-or-directory from to)))

(provide/contract
 [make-directory*/ignore-exists-exn (path-string? . -> . void)]
 [rename-file-or-directory/ignore-exists-exn (path-string? path-string? . -> . void)])