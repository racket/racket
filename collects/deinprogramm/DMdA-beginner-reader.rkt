#lang scheme/base
(require deinprogramm/DMdA-reader)
(provide (rename-out (-read-syntax read-syntax))
         (rename-out (-read read)))
(define -read-syntax (make-read-syntax '(lib "DMdA-beginner.rkt" "deinprogramm")))
(define -read (make-read '(lib "DMdA-beginner.rkt" "deinprogramm")))
