#lang scheme/base

(provide register-external-file)
(define (register-external-file f)
  (unless (and (path? f) (complete-path? f))
    (raise-type-error 'register-external-file "complete path" f))
  (log-message (current-logger) 
               'info 
               (format "file dependency: ~s" f)
               `#s(file-dependency ,f)))
