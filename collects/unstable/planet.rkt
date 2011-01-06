#lang racket/base

(require (for-syntax racket/base unstable/planet-syntax)
         planet/version
         unstable/planet-syntax
         unstable/require)

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
