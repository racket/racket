#lang scheme/base

(provide (rename-out [planet-read read]
                     [planet-read-syntax read-syntax]))

(define (planet-read-fn in spec->read-data)
  (let* ([spec (read-line in)]
         [pkgname (regexp-match #rx"^[ ]*([^ ]+)[ ]*([^ ]+)[ ]*([^ ]*)[ ]*([^ ]*)[ ]*$" spec)])
    (unless pkgname
      (raise-syntax-error 'read "bad syntax, oops"))
    (let-values ([(_ owner pkgname majstr minstr) (apply values pkgname)])
      (let ([maj (string->number majstr)]
            [min (string->number minstr)])
        (unless (or maj (string=? majstr ""))
          (raise-syntax-error 'read "bad maj"))
        (unless (or min (string=? minstr ""))
          (raise-syntax-error 'read "bad min"))
        (unless (or maj (not min))
          (raise-syntax-error 'read "bad version number pair"))
        (values 
         `(planet "lang/main.ss"
                  (,owner
                   ,pkgname
                   ,@(if maj `(,maj) '())
                   ,@(if min `(,min) '())))
         (spec->read-data
          `(planet "lang/reader.ss"
                   (,owner
                    ,pkgname
                    ,@(if maj `(,maj) '())
                    ,@(if min `(,min) '())))))))))

(define (wrap port spec body)
  (let* ([p-name (object-name port)]
         [name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol (path->string (path-replace-suffix name #""))))
                   'page)])
    `(module ,name ,spec
       ,body)))

(define (planet-read [inp (current-input-port)])
  (define-values (spec r) (planet-read-fn inp (λ (spec) (dynamic-require spec 'read))))
  (wrap inp spec (r inp)))

(define (planet-read-syntax [src #f] [inp (current-input-port)])
  (define-values (spec r) (planet-read-fn inp (λ (spec) (dynamic-require spec 'read-syntax))))
  (wrap inp spec (r src inp)))