;; TeachPack: gui.ss
;; Language Level: Advanced 

;; type in text, choose, click okay, watch message field change, close

(define msg (make-message "Hello World"))

(define txt (make-text "Enter your password please"))

(define chc (make-choice (list "Choose something" "Or other")))

(define call-back
  (lambda (x)
    (begin
      (draw-message msg (format "~s ~s~n" (choice-index chc) (text-contents txt)))
      (draw-message msg "Bye World"))))

(define (destroy x) (hide-window x))

(create-window
 (list (list txt msg chc)
       (list (make-button "Okay?" call-back))
       (list (make-button "Close" hide-window))))

