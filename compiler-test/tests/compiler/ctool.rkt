#lang racket
(require setup/dirs)

(define raco (build-path (find-console-bin-dir)
                         (if (eq? (system-type) 'windows)
                             "raco.exe"
                             "raco")))

(define tmp (make-temporary-file))

(system* raco
         "ctool"
         "--3m"
         "--c-mods"
         tmp
         "++lib"
         "racket")

(delete-file tmp)
