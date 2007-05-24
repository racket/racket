(module mm00 (lib "persistent-interaction.ss" "prototype-web-server")
  
  (define (gn which)
    (cadr
     (send/suspend
      (lambda (k)
        (let ([ignore (printf "Please send the ~a number.~n" which)])
          k)))))
  
  (let* ([ignore (start-interaction car)]
         [ans (+ (gn "first")
                 (gn "second")
                 (gn "third"))])
    (printf "The answer is: ~s~n" ans)
    ans))