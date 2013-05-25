#lang racket

(module shared racket/base
  (require 2htdp/universe 2htdp/image)
  
  (struct s (t) #:prefab)
  (provide s s-t (all-from-out 2htdp/universe 2htdp/image)))

(module client racket
  (require (submod ".." shared))

  ;; Color -> Boolean 
  (define (client c)
    (define count 0)
    (big-bang #true
              (to-draw (lambda (w) (text (if w "hello world" "good bye") 22 c)))
              (register LOCALHOST)
              #;
              (stop-when (lambda (w) (> count 3)))
              (on-receive 
               (lambda (w msg)
                 (set! count (+ count 1))
                 ;; send out a prefabed struct to the server 
                 (make-package (not w) (s count)))))) 
  
  (provide client))

(module server racket 
  (require (submod ".." shared))
  
  (define (server)
    (universe '()
              (on-new (lambda (w n) (make-bundle (cons n w) '() '())))
              (on-tick
               (lambda (w*)
                 (make-bundle w* (map (lambda (w) (make-mail w 'go)) w*) '()))
               1
               3)
              (on-msg 
               (lambda (state iw msg)
                 ;; display the received prefabbed struct's content 
                 (displayln (s-t msg))
                 (make-bundle state '() '())))))
  
  
  (provide server))

(module run racket/base
  (require (submod ".." client) (submod ".." server) (submod ".." shared))
  
  (launch-many-worlds (client 'blue) (client 'red) (server)))

(require (submod "." run))
