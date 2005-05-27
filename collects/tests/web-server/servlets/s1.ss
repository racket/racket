; expects the servlet.ss teachpack 
; works in any language above beginner  (including beginner with qq)

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

(define blue (color 'navy))
(define green (color 'green))

(send/finish `(html (body (p "Thoughts about blue: " ,blue)
                          (p "What you wrote about green: " ,green))))
