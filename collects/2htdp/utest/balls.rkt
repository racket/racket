#lang scheme

(require 2htdp/universe htdp/testing)

;; rotate through a bunch of players with the ball until nobody is left 

;; -----------------------------------------------------------------------------
;; Universe = [Listof IWorld]
;; BallMail = (make-mail IWorld 'go)
;; Result   = (make-bundle [Listof IWorld] [Listof BallMail] '())

(define Result0 (make-bundle '() '() '()))

;; -----------------------------------------------------------------------------
;; [Listof IWorld] -> Result
;; create bundle with a singleton list of mails to the first world on the list 
(define (mail2 lw)
  (make-bundle lw (list (make-mail (first lw) 'go)) '()))

;; -----------------------------------------------------------------------------
;; Universe IWorld -> Result
;; add w to the list of worlds; get the first one to play 

(check-expect (add-world '() iworld1) (mail2 (list iworld1)))

(define (add-world univ wrld)
  (mail2 (append univ (list wrld))))

;; -----------------------------------------------------------------------------
;; Universe IWorld Sexp -> Result
;; w sent message m in universe u 

(check-expect
 (switch (list iworld1 iworld2) iworld1 'go) (mail2 (list iworld2 iworld1)))

(check-error 
 (switch (list iworld1 iworld2) iworld2 'go) "switch: wrong world sent message")

(check-error 
 (switch (list iworld2 iworld1) iworld2 'stop) "switch: bad message: stop")

(define (switch u w m)
  (local ((define fst (first u))
          (define nxt (append (rest u) (list fst))))
    (cond
      [(and (iworld=? fst w) (symbol=? m 'go)) (mail2 nxt)]
      [(iworld=? fst w) (error 'switch "bad message: ~s" m)]
      [else (error 'switch "wrong world sent message")])))

;; -----------------------------------------------------------------------------
;; [Listof IWorld] Universe IWorld -> Result
;; w disconnected from the universe 

(check-expect (disconnect (list iworld1 iworld2 iworld3) iworld2) 
              (mail2 (list iworld1 iworld3)))
(check-expect (disconnect '() iworld2) Result0)

(define (disconnect u w)
  (local ((define nxt (remq w u)))
    (if (empty? nxt) Result0  (mail2 nxt))))

;; IWorld [Listof IWorld] -> [Listof IWorld]
;; remove w from low

(check-expect (remq 'a '(a b c)) '(b c))
(check-expect (remq 'a '(a b a c)) '(b c))
(check-expect (remq 'b '(a b a c)) '(a a c))

(define (remq w low)
  (cond
    [(empty? low) '()]
    [else (local ((define fst (first low))
                  (define rst (remq w (rest low))))
            (if (eq? fst w) rst (cons fst rst)))]))

;; -- run program run 

(test)

(define (run _)
  (universe '() 
            (on-new add-world)
            (check-with list?)
            (on-msg switch)
            (on-disconnect disconnect)))

(run 'go)
