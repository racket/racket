#lang racket

(require (for-syntax unstable/planet-syntax)
         unstable/planet-syntax
         unstable/require)

(define-syntax (this-package-version-symbol stx)
  (syntax-case stx ()
    [(tpvi)
     (quasisyntax/loc stx
       '#,(syntax-source-planet-package-symbol stx #f))]
    [(tpvi name)
     (identifier? #'name)
     (quasisyntax/loc stx
       '#,(syntax-source-planet-package-symbol stx #'name))]))

(provide this-package-version-symbol
         this-package-in
         define-planet-package
         make-planet-path
         syntax-source-planet-package
         syntax-source-planet-package-owner
         syntax-source-planet-package-name
         syntax-source-planet-package-major
         syntax-source-planet-package-minor
         syntax-source-planet-package-symbol)
