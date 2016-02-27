#lang racket/base
(provide types)
(define types
  (hash '(real-and/c-name racket/contract/private/misc)
        '(->* () #:rest Contract Contract)
        
        '(or/c-name racket/contract/private/orc)
        '(->* () #:rest Contract Contract)))

;; cast : alpha (<alpha,beta> ctc) -> beta