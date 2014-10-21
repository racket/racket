#lang racket 

;; fixes PR 14781

(define-namespace-anchor an)

(with-handlers ([exn:fail:syntax? 
                 ;; succeed quietly if 
                 (lambda (x)
                   (unless (regexp-match? "^to-draw" (exn-message x))
                     (error 'to-draw "got wrong error message: ~e" (exn-message x))))])
  (eval
   '(module test racket
      (require 2htdp/universe)
      to-draw)
   (namespace-anchor->namespace an))
  (error 'to-draw "got no error message"))

;; known problem with the solution: 
;;    (to-draw on-draw)
;; will signal the wrong kind of error 
