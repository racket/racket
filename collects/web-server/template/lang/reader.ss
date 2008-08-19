#lang scheme/base
(require (prefix-in scribble: scribble/reader))

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
         [id 'template])
    `(module ,name web-server/template/lang
       (#%module-begin ,id . ,body))))
