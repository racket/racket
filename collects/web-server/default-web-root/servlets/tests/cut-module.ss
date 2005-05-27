; purpose: to test send/suspend, send/forward, send/back, and send/finish
(module cut-module mzscheme
  (provide interface-version timeout start)
  (require (lib "servlet.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "servlet-sig.ss" "web-server"))
  
  (define interface-version 'v1)
  
  (define timeout (* 7 24 60 60))
  
  ; : request -> response
  (define (start initial-request)
    (let ([order (extract-binding/single
                  'order
                  (request-bindings
                   (send/suspend (let ([question "Place your order"])
                                   (build-suspender
                                    `(,question)
                                    `(,question (input ([type "text"] [name "order"]))))))))])
      (if (string=? "coconut" order)
          (continue-shopping)
          (retry-order))))
  
  ; : -> doesn't
  (define (continue-shopping)
    (let* ([next-request
            (send/forward
             (build-suspender
              '("Keep shopping")
              `((p "Your order has shipped to a random location.  You may not go back.")
                (p (input ([type "submit"] [name "go"] [value "Keep Shopping"])))
                (p (input ([type "submit"] [name "stop"] [value "Logout"]))))))]
           [next (request-bindings next-request)])
      (cond
        [(exists-binding? 'go next)
         (start next-request)]
        [(exists-binding? 'stop next)
         (send/finish goodbye-page)]
        [else
         (send/finish
          `(html (head (title "Oops"))
                 (body ([bgcolor "white"])
                       (p "Oops " ,(format "next = ~v" next)))))])))
  
  ; : -> doesn't
  (define (retry-order)
    (send/back '(html (head (title "oops"))
                      (body (p "This store only sells coconuts.  Please click the browser's back button and type "
                               (code "coconut") " in the field.")))))
  
  (define goodbye-page
    `(html (head (title "Goodbye"))
           (body (p "Thank you for shopping.")))))
  
  