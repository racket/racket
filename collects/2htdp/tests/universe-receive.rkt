;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname universe-receive) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

;; Nat Nat ->* World1 World2 [Listof IWorld]
;; launch a sending world, a receiving world, and a connecting universe
(define (main rate limit)
  (local (;; UniSt = [Listof IWorld]
          
          ;; UniSt IWorld -> UniSt 
          (define (accept-another-world u iw)
            (make-bundle (cons iw u) '() '()))
          
          ;; UniSt IWorld Message -> [bundle UniSt [List [Mail IWorld 'reset]] '()]
          (define (forward-message u iw msg)
            (make-bundle u (list (make-mail (other iw u) 'reset)) '()))
          
          ;; IWorld [List IWorld IWorld] -> IWorld 
          ;; given one iworld, pick the other one from a list of two 
          (define (other iw lo-iworld)
            (if (iworld=? (first lo-iworld) iw)
                (second lo-iworld)
                (first lo-iworld))))
    (launch-many-worlds 
     (sending-world rate limit)
     (receiving-world 10)
     ;; a universe that channels all messages from one world to another
     (universe '() (on-new accept-another-world) (on-msg forward-message)))))

;; World1 = Number 

;; Nat -> World1
;; a world that counts down from n to 0, but resets to n for every message 
(define (receiving-world n)
  (local (;; World1 -> World1
          (define (reset _1 _2) n))
    (big-bang n
              (on-tick sub1 1)
              (to-draw (draw 'red))
              (on-receive reset)
              (stop-when zero?)
              (register LOCALHOST))))

;; World2 = Number 

;; Number Number -> World2 
;; a world that counts up to limit at rate r, sending a message at every tick
(define (sending-world r l)
  (local (;; World2 -> [package World2 Nat]
          (define (inc w) (make-package (add1 w) w)))
  (big-bang 0 (on-tick inc r l) (to-draw (draw 'blue)) (register LOCALHOST))))

;; Color -> [Number -> Image]
;; create an image for a number, in a fixed color c 
(define (draw c)
  (lambda (w) 
    (overlay (text (number->string w) 22 c) (empty-scene 100 100))))

;; run universe run 
(main 1 3)
