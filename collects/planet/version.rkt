#lang racket/base

(provide this-package-version
         this-package-version-name
         this-package-version-owner
         this-package-version-maj
         this-package-version-min
         this-package-version-symbol
         this-package-in)

(require
  planet/util
  (for-syntax
    racket/base
    racket/require-transform
    syntax/parse
    planet/syntax))

(define-syntax this-package-in
  (make-require-transformer
    (syntax-parser
      [(~and src (_ (~or (~datum main) suffix:id) ...))
       (expand-import
         #`(combine-in
             #,@(for/list ([suf (in-list (attribute suffix))])
                  (make-planet-require-spec #'src suf))))])))
