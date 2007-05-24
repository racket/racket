(module mm01 (lib "persistent-interaction.ss" "web-server" "prototype-web-server")
  
  (define (gn which)
    (cadr
     (send/suspend
      (lambda (k)
        (let ([ignore (printf "Please send the ~a number.~n" which)])
          k)))))
  
  (let ([ignore (start-interaction car)])
    (gn "first")))