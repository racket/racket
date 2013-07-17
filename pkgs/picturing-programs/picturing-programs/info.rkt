#lang info
  (define categories '(media))
  (define can-be-loaded-with 'all)
  (define required-core-version "5.0.0.1")
  (define primary-file "main.rkt")
  (define scribblings '(("picturing-programs.scrbl" () (teaching -21))))
  (define repositories '("4.x"))
  (define compile-omit-paths '("tests"))
  (define blurb
      `("The picturing-programs collection supersedes the tiles and sb-world collections.  It provides functions to rotate, etc. images, as well as a slightly modified version of the universe teachpack."))
  (define release-note-files '(("Picturing Programs" "HISTORY.txt")))
