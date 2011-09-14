#lang setup/infotab

;; compiling the wxme files requires `racket/draw' which fails on build
;; machines without libcairo.  So avoid compiling them.
(define compile-omit-paths 'all)
