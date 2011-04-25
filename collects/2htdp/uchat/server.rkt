;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)

;; UniState = [Listof IWorld]
;; interp. the currently participating iworlds

;; Message = (list String String)
;; intep. 
;; incoming message: (list to-address message)
;; outgoing message: (list from-address message)

;; Result = (make-bundle UniState [Listof (make-mail IWorld Message)]) 
;; -----------------------------------------------------------------------------
(define ALL "*")

;; -----------------------------------------------------------------------------
;; Universe IWorld -> Bundle
;; add the name of the new world to the universe

(check-expect (new-chatter '() iworld1) (make-bundle (list iworld1) '() '()))

(define (new-chatter u w)
  (make-bundle (cons w u) '() '()))

;; -----------------------------------------------------------------------------
;; Universe IWorld Message -> Result
;; process the message and forward it to the appropriate addressees 

(define u0 (list iworld1 iworld2 iworld3))
(define name1 (iworld-name iworld1))
(define name2 (iworld-name iworld2))
(define name3 (iworld-name iworld3))

(check-expect
 (forward u0 iworld1 (list name2 "hello"))
 (make-bundle u0 (list (make-mail iworld2 (list name1 "hello"))) '()))

(check-expect 
 (forward u0 iworld1 (list "*" "hello"))
 (make-bundle u0 
              (list
               (make-mail iworld2 (list (string-append name1 "*") "hello"))
               (make-mail iworld3 (list (string-append name1 "*") "hello")))
              '()))

(define (forward u s msg) 
  (local ((define to (first msg)))
    (make-bundle u (transform u (iworld-name s) to (second msg)) '())))

;; Universe String Message -> [Listof Message]
;; transform an incoming message into a list of outgoing ones

(check-expect (transform u0 name1 name2 "hello")
              (list (make-mail iworld2 (list name1 "hello"))))

(check-expect (transform u0 name1 "bob" "hello") '())

(check-expect (transform u0 name1 "*" "hello")
              (list
               (make-mail iworld2 (list (string-append name1 "*") "hello"))
               (make-mail iworld3 (list (string-append name1 "*") "hello"))))

(define (transform univ from to msg)
  (if (string=? ALL to)
      (local ((define true-msg  (list (string-append from "*") msg)))
        (map (lambda (to-world) (make-mail to-world true-msg)) 
             (filter (lambda (to-world) 
                       (not (string=? (iworld-name to-world) from)))
                     univ)))
      (local ((define true-to (lookup to univ)))
        (if (boolean? true-to)
            '()
            (list (make-mail true-to (list from msg)))))))

;; String Universe -> IWorld or false 
;; pick the iworld whose name is n

(check-expect (lookup name1 u0) iworld1)
(check-expect (lookup name2 u0) iworld2)
(check-expect (lookup name3 u0) iworld3)
(check-expect (lookup "*" u0) false)

(define (lookup name univ)
  (cond
    [(empty? univ) false]
    [else (if (string=? (iworld-name (first univ)) name)
              (first univ)
              (lookup name (rest univ)))]))

;; Any -> Universe
;; run the chat server 
(define (run debug)
  (universe '()
            (state debug)
            (on-new new-chatter)
            (on-msg forward)))

(run #true)
