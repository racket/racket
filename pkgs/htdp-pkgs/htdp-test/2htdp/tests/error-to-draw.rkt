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

(with-handlers ([exn:fail:syntax? 
                 ;; succeed quietly if 
                 (lambda (x)
                   (unless (regexp-match? "^to-draw" (exn-message x))
                     (error 'to-draw "got wrong error message: ~e" (exn-message x))))])
  (eval
   '(module test racket
      (require 2htdp/universe)
      (to-draw on-draw))
   (namespace-anchor->namespace an))
  (error 'to-draw "got no error message"))

(with-handlers ([exn:fail:syntax? 
                 ;; succeed quietly if 
                 (lambda (x)
                   (unless (regexp-match? "^to-draw" (exn-message x))
                     (error 'to-draw "got wrong error message: ~e" (exn-message x))))])
  (eval
   '(module test racket
      (require 2htdp/universe)
      (big-bang 0 (on-draw to-draw)))
   (namespace-anchor->namespace an))
  (error 'to-draw "got no error message"))

;; it still works okay for on-draw 

(with-handlers ([exn:fail:syntax? 
                 ;; succeed quietly if 
                 (lambda (x)
                   (unless (regexp-match? "^on-draw" (exn-message x))
                     (error 'on-draw "got wrong error message: ~e" (exn-message x))))])
  (eval
   '(module test racket
      (require 2htdp/universe)
      (on-draw to-draw))
   (namespace-anchor->namespace an))
  (error 'on-draw "got no error message"))

(with-handlers ([exn:fail:syntax? 
                 ;; succeed quietly if 
                 (lambda (x)
                   (unless (regexp-match? "^on-draw" (exn-message x))
                     (error 'on-draw "got wrong error message: ~e" (exn-message x))))])
  (eval
   '(module test racket
      (require 2htdp/universe)
      (big-bang 0 (to-draw on-draw)))
   (namespace-anchor->namespace an))
  (error 'to-draw "got no error message"))