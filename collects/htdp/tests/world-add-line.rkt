#lang scheme/base
(require htdp/image test-engine/scheme-tests)

(define plain (empty-scene 100 100))

; (scene+line plain .5 10.3 -20 80 'red)

'verticals
(check-expect (scene+line plain -10 90 -10 80 'red) plain) 
(check-expect (scene+line plain 110 90 110 80 'red) plain)
(check-expect (scene+line plain +10 90 +10 80 'red) 
              (scene+line plain +10 90 +10 80 'red))
#;
(check-expect (scene+line plain +10 900000 +10 80 'red) 
              (scene+line plain +10 100 +10 80 'red))
;; can't make image of this size 

(check-expect (scene+line plain +10 -10 +10 80 'red) 
              (scene+line plain +10 0 +10 80 'red))

'horizontals
(check-expect (scene+line plain 20 -10 30 -10 'red) plain)
(check-expect (scene+line plain 20 110 30 110 'red) plain)
(check-expect (scene+line plain 20 +10 30 +10 'red) 
              (scene+line plain 20 +10 30 +10 'red))
#;
(check-expect (scene+line plain 20 +10 30000 +10 'red) 
              (scene+line plain 20 +10 100 +10 'red))
;; can't make image of this size 

'inside-outside
(check-expect (scene+line plain 10 10 -10 -10 'red) ; upper-left
              (scene+line plain 10 10 0 0 'red))
(check-expect (scene+line plain 10 10 -10 0 'red) ; upper-left
              (scene+line plain 10 10 0 5 'red))
(check-expect (scene+line plain 90 10 110 -10 'red) ; upper-right 
              (scene+line plain 90 10 100 0 'red))
(check-expect (scene+line plain 90 10 110 0 'red) ; upper-left
              (scene+line plain 90 10 100 5 'red))
(check-expect (scene+line plain 90 90 110 110 'red) ; lower-right
              (scene+line plain 90 90 100 100 'red))
(check-expect (scene+line plain 90 90 110 100 'red) ; lower-right
              (scene+line plain 90 90 100 95 'red))
(check-expect (scene+line plain 110 110 10 10 'red) ; lower-right
              (scene+line plain 10 10 100 100 'red))
(check-expect (scene+line plain 10 10 210 110 'red) ; lower-right 
              (scene+line plain 10 10 100 55 'red))
(check-expect (scene+line plain 10 10 -10 30 'red) ; lower-left
              (scene+line plain 10 10 0 20 'red))
(check-expect (scene+line plain 10 10 -10 210 'red) ; lower-left
              (scene+line plain 10 10 0 110 'red))

'outside-outside
(check-expect (scene+line plain  -100 10 300 50 'red) ;; left-right 
              (scene+line plain 0 20 100 30 'red))
(check-expect (scene+line plain -50 0 60 110 'red) ;; left-low
              (scene+line plain 0 50 50 100 'red))
(check-expect (scene+line plain -50 50 60 -5 'red) ;; left-top
              (scene+line plain 0 25 50 0 'red))
(check-expect (scene+line plain -10 -10 110 50 'red) ;; top-right
              (scene+line plain 10 0 100 45 'red))

(check-expect (scene+line plain -10 -10 110 110 'red) ;; top-low
              (scene+line plain 0 0 100 100 'red))

(check-expect (scene+line plain -10 110 110 50 'red) ;; low-right
              (scene+line plain 0 105 100 55 'red))

'totally-outside
(check-expect (scene+line plain -100 -100 -200 -500 'red) plain)

(test)
