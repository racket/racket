#lang scheme/base

(require "../../text.ss")

(provide (rename-out [*read read])
         (rename-out [*read-syntax read-syntax]))

(define (*read [inp (current-input-port)])
  (wrap inp (at:read-inside inp)))

(define (*read-syntax [src #f] [port (current-input-port)])
  (wrap port (at:read-syntax-inside src port)))

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
    `(module ,name scribble/text . ,body)))
