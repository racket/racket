#lang racket
(require games/cards racket/gui racket/class racket/unit)

(provide game@)

(define game@ (unit (import) (export)

;; Layout width and height:
(define WIDTH 5)
(define HEIGHT 4)
(define MAX-MATCHES (/ (* WIDTH HEIGHT) 2))

;; Randomize
(random-seed (modulo (current-milliseconds) 10000))

;; Set up the table
(define t (make-table "Memory" (+ 2 WIDTH) (+ 1 HEIGHT)))
(send t show #t)
(send t set-double-click-action #f)

;; Get table width & height
(define w (send t table-width))
(define h (send t table-height))

;; Set up the cards
(define deck 
  (let ([cards (map (lambda (name value)
                      (let ([bm (make-object
                                 bitmap%
                                 (build-path
                                  (collection-path "games" "memory" "images")
                                  (format "~a.png" name)))])
                        (make-card bm #f 0 value)))
                    '("club" "heart" "spade" "diamond"
                      "happy" "unhappy"
                      "fish" "two-fish"
                      "jack" "star")
                    '(1 2 3 4 5 6 7 8 9 10))])
    (append cards (map (lambda (c) (send c copy)) cards))))
(for-each (lambda (card)
            (send card user-can-move #f)
            (send card user-can-flip #t))
          deck)

;; Card width & height
(define cw (send (car deck) card-width))
(define ch (send (car deck) card-height))

(define dx (/ cw (+ 2 WIDTH)))
(define dy (/ ch (+ 1 HEIGHT)))

(define match-x (- w cw dx))
(define match-y dy)

(define time-h (+ 12 5 5))
(define time-x match-x)
(define time-y (+ ch dy dy))

;; Put the cards on the table
(send t add-cards deck match-x match-y)

;; Setup
(define (setup)
  (reset-timer)
  (set! deck (shuffle-list deck 7))
  (send t stack-cards deck)
  (send t move-cards deck 0 0
        (lambda (pos)
          (let ([i (modulo pos WIDTH)]
                [j (quotient pos WIDTH)])
            (values (+ dx (* i (+ cw dx)))
                    (+ dy (* j (+ ch dy))))))))

;; Number of matches found so far:
(define matches 0)

;; First card flipped, or #f if non flipped, yet
(define card-1 #f)

(define (flip-and-match c)
  (cond [(eq? c card-1)
         ;; Cancel first card
         (send t flip-card c)
         (set! card-1 #f)]
        [(not (send c face-down?))
         ;; Can't click a matched card, unless the game is over, 
         ;; in which case we reset the game
         (when (= matches MAX-MATCHES)
           (send t flip-cards deck)
           (set! matches 0)
           (setup))]
        [else
         ;; Flip over a card...
         (send t flip-card c)
         (send t card-to-front c)
         (run-timer)
         (cond [(not card-1)
                ;; That was the first card
                (set! card-1 c)]
               [(and (equal? (send card-1 get-value) (send c get-value))
                     (equal? (send card-1 get-suit) (send c get-suit)))
                ;; Match
                (send t pause 0.5)
                (send t move-cards (list card-1 c) match-x match-y)
                (set! card-1 #f)
                (set! matches (add1 matches))]
               [else
                ;; Not a match
                (send t pause 0.5)
                (send t flip-cards (list card-1 c))
                (set! card-1 #f)])]))
(send t set-single-click-action flip-and-match)

;; The timer turns out to be the most difficult part:
(define (make-time-region secs)
  (make-region time-x time-y cw time-h
               (if (>= secs 6000)
                 "XX:XX"
                 (format
                  "~a:~a"
                  (substring (number->string (+ 100 (quotient secs 60))) 1)
                  (substring (number->string (+ 100 (modulo secs 60))) 1)))
               #f))
(define start-time #f)   ; in inexact milliseconds; #f means not started
(define shown-seconds 0) ; used to compute the delay until the next update
(define time-region (make-time-region 0)) ; old region, so we wan remove it
(send t add-region time-region) ; start with the initial region added
(define (show-time n)
  ;; Compute new time to show:
  (set! shown-seconds n)
  ;; Update the time by removing the old region and adding a new one:
  (send t begin-card-sequence)
  (send t remove-region time-region)
  (set! time-region (make-time-region shown-seconds))
  (send t add-region time-region)
  (send t end-card-sequence))
(define (get-update-delta)
  ;; Figure out how many milliseconds to sleep before the next update
  (max 0 (inexact->exact (floor (- (+ start-time (* 1000 shown-seconds) 1000)
                                   (current-inexact-milliseconds))))))
(define time-timer
  (make-object timer%
    (lambda ()
      (unless (= matches MAX-MATCHES)
        (show-time
         (inexact->exact
          (floor (/ (- (current-inexact-milliseconds) start-time) 1000))))
        (send time-timer start (get-update-delta) #t)))))
(define (reset-timer)
  (send time-timer stop)
  (set! start-time #f)
  (show-time 0))
(define (run-timer)
  (unless start-time
    (set! start-time (current-inexact-milliseconds))
    (send time-timer start 1000 #t)))

;; Start the game:
(send t pause 0.25)
(setup)))
