(module temp (lib "newcont.ss" "web-server" "prototype-web-server" "newcont")
  (provide start)
  
  (define msg (make-parameter "unknown"))
  
  (define (gn should-be i)
    (let/cc k
      (printf "~S == ~S~n" should-be (msg))
      i))
  
  (define (start)
    '(fun . #t)
    (printf "12 + 1 = 13 = ~S~n"
            (+
             (parameterize ([msg "first"])
               (gn "first" 12))
             (parameterize ([msg "second"])
               (gn "second" 1))))))
