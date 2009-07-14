

;; UniState = (list String)
;; interp. the name of currently participating chatters

;; Message = (list String String)

;; Result = (make-bundle UniState [Listof (make-mail IWorld Message)]) 

;; Universe IWorld -> Universe
;; add the name of the new world to the universe

(check-expect (new-chatter '() iworld1) (list iworld1))

(define (new-chatter u w)
  (cons (iworld-name w) u))

;; Universe IWorld Message -> Result

(define u0 (list iworld1 iworld2 iworld3))
(define name1 (iworld-name iworld1))
(define name2 (iworld-name iworld2))
(define name3 (iworld-name iworld3))

(check-expect (forward u0 iworld1 (list name2 "hello"))
  (make-bundle u0 (list (make-mail name1 "hello")) '()))

(check-expect (forward u0 iworld1 (list "*" "hello"))
  (make-bundle u0 (list
		    (make-mail (string-append name2 "*") "hello")
		    (make-mail (string-append name3 "*") "hello"))))

(define (forward u s msg) (make-bundle u '() '()))
  
