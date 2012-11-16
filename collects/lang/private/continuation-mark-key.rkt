#lang scheme/base

(provide teaching-languages-continuation-mark-key)

; The test code also needs access to this.

;; cm-key : symbol
;; the key used to put information on the continuation
(define teaching-languages-continuation-mark-key (gensym 'teaching-languages-continuation-mark-key))
