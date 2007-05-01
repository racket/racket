(module pr5565 mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 120)
  (define (start ireq)
    (define p
      (send/suspend
       (build-suspender `("Test of Page 2")
                        `((input ([type "submit"][value "pls test with and without topping"]))))))    
    (define q
      (send/suspend
       (build-suspender `("Bug")
                        `((input ([type "text"][name "x"]))))))    
    (define r (extract-binding/single `x (request-bindings q)))   
    (send/suspend
     (build-suspender `("Result of test")
                      (list r)))))
