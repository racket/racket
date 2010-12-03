#lang racket/gui

(define SIZE 600)

(define f (new frame%
               [label "Color Bars"]
               [width SIZE]
               [height SIZE]))

(define c (new canvas% [parent f]))

(send f show #t)

;; If sync is turned off, then expect the drawing
;; to flicker horribly:
(define sync? #t)

;; If flush-on-sync is disabled, the expect refresh
;; to starve, so that the image moves very rarely, if
;; at all:
(define flush-on-sync? #t)

(define (start-drawing dc)
  (when sync?
    (send dc suspend-flush)))

(define (end-drawing dc)
  (when sync?
    (send dc resume-flush)
    (when flush-on-sync?
      (send dc flush))))

(define (go)
  (let ([dc (send c get-dc)])
    (for ([d (in-naturals)])
      (start-drawing dc)
      (send dc erase)
      ;; Draw somthing slow that changes with d
      (for ([n (in-range 0 SIZE)])
        (send dc set-pen 
              (make-object color% 
                           (remainder (+ n d) 256)
                           (remainder (* 2 (+ n d)) 256)
                           (remainder (* 3 (+ n d)) 256))
              1
              'solid)
        (send dc draw-line n 0 n SIZE))
      (end-drawing dc))))

(thread go)
