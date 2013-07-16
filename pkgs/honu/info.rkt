#lang info

(define collection "honu")
(define deps '("macro-debugger"
               "base"
               "parser-tools-lib"
               "rackunit-lib"))
(define build-deps '("scribble-lib"))

;; Make honu.vim easier to find by copying it to the "lib" directory:
(define copy-foreign-libs '("contrib/honu.vim"))
