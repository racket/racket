(module mm01 (lib "lang.ss" "web-server")
  (provide start)
  (define (gn which)
    (cadr
     (send/suspend
      (lambda (k)
        (let ([ignore (printf "Please send the ~a number.~n" which)])
          k)))))
  
  (define (start initial)
    (gn "first")))