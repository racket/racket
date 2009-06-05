#lang scheme/gui

(require "../2htdp/universe.ss")

(define-struct unistate (active passive) #:transparent)

;; Universe = (make-unistate Player Player)
;; interpretation: (make-unistate p q) means p is the currently active player, 
;;   and q is the one waiting for the ball 

;; SMessage is 'stop or 'go 

;; Player Player -> (cons Universe [list SMessage])
;; create initial universe and tell p2 to start playing 
(define (create-universe p1 p2)
  (make-bundle (make-unistate p2 p1) 
               (list (make-mail p2 'go)
                     (make-mail p1 'stop))))
  
;; Universe Player Sexp -> (cons Universe [list SMessage])
;; p sent message m in universe u 
(define (switch-players u p m)
  (make-bundle (make-unistate (unistate-passive u) (unistate-active u))
               (list (make-mail (unistate-passive u) 'go))))

;; --- 

(universe2 create-universe switch-players)
