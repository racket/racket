(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         "helper-sig.ss")

(define main@
  (unit/sig ()
    (import servlet^ my-servlet-helpers^)
    
    `(html (head (title "Sum"))
           (body ([bgcolor "white"])
                 (p "The sum is "
                    ,(number->string (+ (get-number "the first number to add")
                                        (get-number "the second number to add"))))))))

(compound-unit/sig
  (import (S : servlet^))
  (link
   [H : my-servlet-helpers^ ((load-relative "helper.ss") S)]
   [M : () (main@ S H)])
  (export (open M)))