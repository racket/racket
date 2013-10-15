#lang racket/base

(provide this-package-version
         this-package-version-name
         this-package-version-owner
         this-package-version-maj
         this-package-version-min
         this-package-version-symbol
         this-package-in
         make-planet-symbol
         package-version->symbol)

(require
 "private/version.rkt"
 (for-syntax
  racket/base
  racket/require-transform
  syntax/parse
  planet/syntax))

(define-syntax this-package-in
  (make-require-transformer
   (lambda (stx)
     (define-syntax-class suffix
       #:description "module suffix identifier"
       (pattern (~datum main) #:attr suf #f)
       (pattern suf:id))
     (syntax-parse stx
       [(_ s:suffix ...)
        (expand-import
         #`(combine-in
            #,@(for/list ([src (in-list (attribute s))]
                          [suf (in-list (attribute s.suf))])
                 (make-planet-require-spec src suf))))]))))
