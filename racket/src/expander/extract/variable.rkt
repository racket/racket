#lang racket/base

(provide (struct-out variable))

;; Represents a variable that is exported by a used linklet:
(struct variable (link   ; link
                  name)  ; symbol
        #:prefab)
