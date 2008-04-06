#lang scheme/base

(require (prefix-in scribble: "../reader.ss"))

(provide (rename-out [*read read])
         (rename-out [*read-syntax read-syntax]))

(define (*read [inp (current-input-port)])
  (wrap inp (scribble:read-inside inp)))

(define (*read-syntax [src #f] [port (current-input-port)])
  (wrap port (scribble:read-syntax-inside src port)))

(define (wrap port body)
  (let* ([p-name (object-name port)]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol (path->string (path-replace-suffix name #""))))
                 'page)]
         [id 'doc])
    `(module ,name scribble/doclang
       (#%module-begin ,id () . ,body))))
