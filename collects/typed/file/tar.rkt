#lang typed/racket/base
;; typed-racket wrapper on file/tar
;; yc 2009/2/25

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic type aliases.
(define-type-alias Path-String (U Path String))

(require/typed file/tar
               ;; tar appears to return exact-nonenegative-integer? instead of void?
               [tar (Path-String Path-String * -> Integer)]
               ;; tar->output appears to take (listof path) instead of (listof path-string?)
               ;; it also appears to return exact-nonenegative-integer?
               [tar->output (case-lambda ((Listof Path) -> Integer)
                                         ((Listof Path) Output-Port -> Integer))]
               ;; tar->gzip
               ;; missing from file/tar but available in mzlib/tar
               ;; actually returns void?
               [tar-gzip (Path-String Path-String * -> Void)]
               )

(provide tar tar->output tar-gzip)
