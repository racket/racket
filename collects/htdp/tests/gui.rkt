;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname gui) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require htdp/gui)

;; type in text, choose, click okay, watch message field change, close

(define msg (make-message "Hello World"))

(define txt (make-text "Enter your password please"))

(define chc (make-choice (list "Choose something" "Or other")))

(define call-back
  (lambda (x)
    (begin
      (draw-message msg (format "~s ~s\n" (choice-index chc) (text-contents txt)))
      (draw-message msg "Bye World"))))

(define (destroy x) (hide-window x))

(define w 
  (create-window
   (list (list txt msg chc)
         (list (make-button "Okay?" call-back))
         (list (make-button "Close" (lambda (x) (hide-window w)))))))

