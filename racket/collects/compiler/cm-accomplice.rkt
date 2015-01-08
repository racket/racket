#lang racket/base

(provide register-external-file
         register-external-module)

(define (register-external-file f #:indirect? [indirect? #f])
  (register-external 'register-external-file f #f indirect?))
(define (register-external-module f #:indirect? [indirect? #f])
  (register-external 'register-external-module f #t indirect?))

(define (register-external who f module? indirect?)
  (unless (and (path? f) (complete-path? f))
    (raise-type-error who "complete path" f))
  (log-message (current-logger)
               'info
               'cm-accomplice
               (format "file dependency: ~s" f)
               (if indirect?
                   `#s((file-dependency/indirect file-dependency 2) ,f ,module?)
                   `#s(file-dependency ,f ,module?))))
