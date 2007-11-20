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
        (spec->read-data
         `(planet "lang/reader.ss"
                  (,owner
                   ,pkgname
                   ,@(if maj `(,maj) '())
                   ,@(if min `(,min) '()))))))))
         
(define (planet-read in) 
  (planet-read-fn in (λ (spec) ((dynamic-require spec 'read) in)))) 
(define (planet-read-syntax srcname in) 
  (planet-read-fn in (λ (spec) ((dynamic-require spec 'read-syntax) srcname in))))