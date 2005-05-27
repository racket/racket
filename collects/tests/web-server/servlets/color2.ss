; needs first defined (i.e. use a teaching langauge)
; needs the servlet.ss teachpack

(require (lib "etc.ss"))

; : sym -> str
; to input an opinion about a color
(define (color the-color)
  (extract-binding/single 
   'color 
   (request-bindings
    (send/suspend 
     (build-suspender '("hi") 
                      `((p ,(format
                             "What do you think about the color ~a?" 
                             the-color))
                        (input ([type "text"] [name "color"]))))))))

(define blue (color (identity (first (list 'blue)))))
(define green (color 'green))

(send/finish `(html (body (p "Thoughts about blue: " ,blue)
                          (p "What you wrote about green: " ,green))))
