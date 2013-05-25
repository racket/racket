#lang racket/base

(provide register-external-file
         register-external-module)

(define (register-external-file f)
  (register-external 'register-external-file f #f))
(define (register-external-module f)
  (register-external 'register-external-module f #t))

(define cm-accomplice-logger (make-logger 'cm-accomplice 
                                          (current-logger)))

(define (register-external who f module?)
  (unless (and (path? f) (complete-path? f))
    (raise-type-error who "complete path" f))
  (log-message cm-accomplice-logger
               'info 
               (format "file dependency: ~s" f)
               `#s(file-dependency ,f ,module?)))
