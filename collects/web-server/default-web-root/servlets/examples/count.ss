(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "date.ss"))

(let ([count 0]
      [date (date->string (seconds->date (current-seconds)) 'time-too)])
  (unit/sig () (import servlet^)
    (define other-count 0)
    
    (set! other-count (add1 other-count))
    (set! count (add1 count))
    
    `(html (head (title "Counter"))
           (body ([bgcolor "white"])
                 (p "This servlet was called " ,(number->string count)
                    " times and " ,(number->string other-count)
                    " times since loaded on " ,date ".")))))