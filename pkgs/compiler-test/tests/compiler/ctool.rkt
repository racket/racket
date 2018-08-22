#lang racket
(require setup/dirs)

(define raco (build-path (find-console-bin-dir)
                         (if (eq? (system-type) 'windows)
                             "raco.exe"
                             "raco")))

(define tmp (make-temporary-file))
(define tmp-dir (make-temporary-file "mztmp~a" 'directory))

(system* raco
         "ctool"
         "--3m"
         "--c-mods"
         tmp
         "++lib"
         "racket")

(system* raco
         "ctool"
         "--3m"
         "--c-mods"
         tmp
         "++lib"
         "racket/promise"
         "--runtime"
         tmp-dir)

(delete-file tmp)
(delete-directory/files tmp-dir)
