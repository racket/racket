#lang scheme/base
(require scribble/manual
         scribble/eval
         (for-label scheme/base
                    scheme/contract
                    scheme/unit))

(define web-server "Web Server")

; XXX Format better
(define (warning . x)
  (apply elem "Warning:" x))

; XXX Actually display link
(define (href-link url label)
  (elem label " (" url ")")) 

(provide (all-from-out scribble/manual)
         (all-from-out scribble/eval)
         (for-label (all-from-out scheme/base
                                  scheme/contract
                                  scheme/unit))
         web-server
         warning
         href-link)
