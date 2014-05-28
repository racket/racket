#lang racket

(require 2htdp/universe 2htdp/image)

;; Run two client-server pairs
(define (run)
  (launch-many-worlds
   (server 8081) (client 'a 8081 'blue) (client 'b 8081 'blue)
   (server 8082) (client 'c 8082 'green) (client 'd 8082 'green)))

;; Port -> #f
(define (server p)
  (define (noop s msg) (make-bundle s (map (λ (s) (make-mail s msg)) s) '()))
  (universe '()
            (port p)
            (on-tick (λ (u) (make-bundle u '() '())) 1 5)
            (on-new (λ (u iw) (make-bundle (cons iw u) '() '())))
            (on-msg (λ (u _iw msg) (noop u msg)))))

;; Symbol Port ColorSymbol -> #f
(define (client name p c)
  (define mt (place-image (text (symbol->string name) 22 'blue) 100 100 (empty-scene 400 400)))
  (big-bang (list #f 0)
            (port p)
            (on-tick
             (λ (s) (if (first s) 
                        (make-package (list #f (add1 (second s))) (second s))
                        (list #t (second s))))
             1)
            (on-receive 
             (λ (s msg) (displayln msg) (list (first s) (+ (second s) msg))))
            (to-draw 
             (λ (s) (if (first s) (place-image (circle (second s) 'solid c) 200 200 mt) mt)))
            (register LOCALHOST)))
(run)
