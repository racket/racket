#lang scheme/base
(require (lib "manual.ss" "scribble")
         (lib "eval.ss" "scribble")
         (for-label scheme/base
                    scheme/contract
                    scheme/unit))

(define web-server "Web Server")

; XXX Format better
(define (author x) 
  (elem (hspace 4)
        (bold x)))

; XXX Format better
(define (warning . x)
  (apply elem "Warning:" x))

; XXX Actually display link
(define (href-link url label)
  (elem label " (" url ")")) 

(provide (all-from-out (lib "manual.ss" "scribble"))
         (all-from-out (lib "eval.ss" "scribble"))
         (for-label (all-from-out scheme/base
                                  scheme/contract
                                  scheme/unit))
         web-server
         author
         warning
         href-link)
