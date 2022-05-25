#lang racket
(require setup/dirs
         compiler/find-exe)

(define racket (find-exe))

(define tmp (make-temporary-file))
(define tmp-dir (make-temporary-file "mztmp~a" 'directory))

(system* racket
         "-l-"
         "raco"
         "ctool"
         "--3m"
         "--c-mods"
         tmp
         "++lib"
         "racket")

(system* racket
         "-l-"
         "raco"
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
