(require (lib "unit.ss")
         (lib "servlet-sig.ss" "web-server")
         "helper-sig.ss")

(define main@
  (unit
    (import servlet^ my-servlet-helpers^)
    (export)
    
    `(html (head (title "Sum"))
           (body ([bgcolor "white"])
                 (p "The sum is "
                    ,(number->string (+ (get-number "the first number to add")
                                        (get-number "the second number to add"))))))))

(compound-unit (import (S : servlet^))
               (export)
               (link 
                (((H : my-servlet-helpers^)) ((load-relative "helper.ss") S))
                (() main@ S H)))
