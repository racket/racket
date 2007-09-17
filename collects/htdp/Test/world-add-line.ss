(require (lib "world.ss" "htdp"))

(define plain (empty-scene 100 100))

(add-line plain .5 10.3 -20 80 'red)

'verticals
(equal? (add-line plain -10 90 -10 80 'red) plain) 
(equal? (add-line plain 110 90 110 80 'red) plain)
(equal? (add-line plain +10 90 +10 80 'red) 
        (add-line plain +10 90 +10 80 'red))
#;
(equal? (add-line plain +10 900000 +10 80 'red) 
        (add-line plain +10 100 +10 80 'red))
;; can't make image of this size 

(equal? (add-line plain +10 -10 +10 80 'red) 
        (add-line plain +10 0 +10 80 'red))

'horizontals
(equal? (add-line plain 20 -10 30 -10 'red) plain)
(equal? (add-line plain 20 110 30 110 'red) plain)
(equal? (add-line plain 20 +10 30 +10 'red) 
        (add-line plain 20 +10 30 +10 'red))
#;
(equal? (add-line plain 20 +10 30000 +10 'red) 
        (add-line plain 20 +10 100 +10 'red))
;; can't make image of this size 

'inside-outside
(equal? (add-line plain 10 10 -10 -10 'red) ; upper-left
        (add-line plain 10 10 0 0 'red))
(equal? (add-line plain 10 10 -10 0 'red) ; upper-left
        (add-line plain 10 10 0 5 'red))
(equal? (add-line plain 90 10 110 -10 'red) ; upper-right 
        (add-line plain 90 10 100 0 'red))
(equal? (add-line plain 90 10 110 0 'red) ; upper-left
        (add-line plain 90 10 100 5 'red))
(equal? (add-line plain 90 90 110 110 'red) ; lower-right
        (add-line plain 90 90 100 100 'red))
(equal? (add-line plain 90 90 110 100 'red) ; lower-right
        (add-line plain 90 90 100 95 'red))
(equal? (add-line plain 110 110 10 10 'red) ; lower-right
        (add-line plain 10 10 100 100 'red))
(equal? (add-line plain 10 10 210 110 'red) ; lower-right 
        (add-line plain 10 10 100 55 'red))
(equal? (add-line plain 10 10 -10 30 'red) ; lower-left
        (add-line plain 10 10 0 20 'red))
(equal? (add-line plain 10 10 -10 210 'red) ; lower-left
        (add-line plain 10 10 0 110 'red))

'outside-outside
(equal? (add-line plain  -100 10 300 50 'red) ;; left-right 
        (add-line plain 0 20 100 30 'red))
(equal? (add-line plain -50 0 60 110 'red) ;; left-low
        (add-line plain 0 50 50 100 'red))
(equal? (add-line plain -50 50 60 -5 'red) ;; left-top
        (add-line plain 0 25 50 0 'red))
(equal? (add-line plain -10 -10 110 50 'red) ;; top-right
        (add-line plain 10 0 100 45 'red))
(equal? (add-line plain -10 -10 110 110 'red) ;; top-low
        (add-line plain 0 0 100 100 'red))
(equal? (add-line plain -10 110 110 50 'red) ;; low-right
        (add-line plain 0 105 100 55 'red))

'totally-outside
(equal? (add-line plain -100 -100 -200 -500 'red) plain)
