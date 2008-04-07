#lang scheme/base
(require (prefix-in r: "../typed-reader.ss")
         (only-in syntax/module-reader wrap-read-all))

(define (*read in modpath line col pos)
  (wrap-read-all 'typed-scheme in r:read modpath #f line col pos))

(define (*read-syntax src in modpath line col pos)
  (wrap-read-all
   'typed-scheme in (lambda (in) (r:read-syntax src in))
   modpath src line col pos))

(provide (rename-out [*read read] [*read-syntax read-syntax]))
