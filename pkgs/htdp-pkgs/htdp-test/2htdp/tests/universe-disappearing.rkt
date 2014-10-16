#lang racket

(module universe racket
  (provide
   ;; Boolean -> [Listof IWorld]
   ;; run a univsere server, show state if given #true
   run)
  ;; ---------------------------------------------------------------------------
  
  (require 2htdp/universe)
  (require 2htdp/image)
  
  (define (run show?)
    (universe '()
              (on-tick identity 1 15)
              (state show?)
              (on-new (lambda (u nw) (cons nw u)))
              (on-msg (lambda (u sender msg)
                        (define new-u (remq sender u))
                        (define throw (list sender))
                        (make-bundle new-u (mail2 new-u msg) throw)))
              (on-disconnect (lambda (u gone)
                               (remq gone u)))))
  
  ;; [Listof IWorld] String -> [Listof Mail]
  (define (mail2 lo-iw sender)
    (for/list ((iw lo-iw))
      (make-mail iw (format "~a says good-bye" sender)))))

(module client racket
  (provide 
   ;; Boolean -> ???
   run)
  ;; -----------------------------------------------------------------------------
  
  (require 2htdp/universe)
  (require 2htdp/image)  
  
  ;; String -> Natural 
  ;; run for at most 9 seconds, send your secret to server at t = 1
  (define (run secret)
    (big-bang (random 10)
              (register LOCALHOST)
              (on-receive (lambda (x msg)
                            (displayln msg)
                            x))
              (to-draw (lambda (x) 
                         (overlay (text secret 12 'blue)
                                  (circle (+ 100 (* 10 x)) 'solid 'red))))
              (on-tick (lambda (x)
                         (if (= x 1)
                             (make-package 0 secret)
                             (sub1 x)))
                       1
                       10)
              (stop-when zero?))))

(require (prefix-in server: (submod "." universe))
         (prefix-in client: (submod "." client))
         2htdp/universe)

(define (main show?)
  (launch-many-worlds 
   (server:run show?)
   (client:run "matthias") 
   (client:run "matthew") 
   (client:run "robby") 
   (client:run "shriram")))

(main #f)