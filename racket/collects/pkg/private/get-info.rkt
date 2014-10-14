#lang racket/base
(require setup/getinfo
         "print.rkt")

;; Working with "info.rkt" files, typically (but not necessarily)
;; package-level "info.rkt" files.

(provide make-metadata-namespace
         get-pkg-info
         get-metadata)

(define (make-metadata-namespace)
  (make-base-empty-namespace))

(define (get-pkg-info pkg-dir metadata-ns)
  (with-handlers ([exn:fail? (λ (x)
                                (log-exn x "getting info")
                                #f)])
    (get-info/full pkg-dir 
                   #:namespace metadata-ns
                   #:bootstrap? #t)))

(define (get-metadata metadata-ns pkg-dir key get-default
                      #:checker [checker void])
  (define get-info (get-pkg-info pkg-dir metadata-ns))
  (define v
    (if get-info
        (get-info key get-default)
        (get-default)))
  (checker v)
  v)

