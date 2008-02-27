#lang scheme/base

(require (prefix-in s: "../../reader.ss"))

(provide (rename-out [*read read])
         (rename-out [*read-syntax read-syntax]))

(define (*read [inp (current-input-port)])
  (wrap inp (s:read-inside inp)))

(define (*read-syntax [src #f] [port (current-input-port)])
  (wrap port (s:read-inside-syntax src port)))

(define (wrap port body)
  (define (strip-leading-newlines stxs)
    (if (null? stxs)
      stxs
      (let ([p (syntax-property (car stxs) 'scribble)])
        (if (and (pair? p) (eq? (car p) 'newline))
          (strip-leading-newlines (cdr stxs))
          stxs))))
  (let* ([p-name (object-name port)]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol (path->string (path-replace-suffix
                                                  name #""))))
                 'page)]
         [id 'doc]
         [body (if (syntax? body)
                 (strip-leading-newlines (syntax->list body))
                 body)])
    `(module ,name scribble/text (#%module-begin . ,body))))
