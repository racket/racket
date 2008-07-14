#lang scheme/base

(require "../parsereq.ss")

(provide (rename-out [planet-read read]
                     [planet-read-syntax read-syntax]))

(define (planet-read-fn in spec->read-data)
  (let* ([spec (read-line in)]
         [parsed-spec
          (with-handlers ([exn:parse-failure? (λ (e) (raise-syntax-error 'read "bad syntax"))])
            (parse-package-string spec))])
    (values 
     `(planet "lang/main.ss" ,parsed-spec)
     (spec->read-data `(planet "lang/reader.ss" ,parsed-spec)))))

(define (wrap port spec read)
  (let* ([body
          (let loop ([a null])
            (let ([v (read port)])
              (if (eof-object? v)
                  (reverse a)
                  (loop (cons v a)))))]
         [p-name (object-name port)]
         [name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol (path->string (path-replace-suffix name #""))))
                   'page)])
    `(module ,name ,spec
       . ,body)))

(define (planet-read [inp (current-input-port)])
  (define-values (spec r) (planet-read-fn inp (λ (spec) (dynamic-require spec 'read))))
  (wrap inp spec r))

(define (planet-read-syntax [src #f] [inp (current-input-port)])
  (define-values (spec r) (planet-read-fn inp (λ (spec) (dynamic-require spec 'read-syntax))))
  (wrap inp spec (lambda (p) (r src p))))

