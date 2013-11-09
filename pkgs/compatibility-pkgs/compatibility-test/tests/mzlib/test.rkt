#lang racket/load

(define quiet-load (collection-file-path "tests.rktl" "tests" "mzlib"))
(load (collection-file-path "quiet.rktl" "tests" "racket"))
