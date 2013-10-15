#lang racket/base
(require planet/version)
(provide me)
(define me (this-package-version))
