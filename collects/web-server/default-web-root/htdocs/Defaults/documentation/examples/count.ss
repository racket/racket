(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "date.ss"))

(let ([count 0]
      [date (date->string (seconds->date (current-seconds)) 'time-too)])
  (unit/sig () (import servlet^)
    
  (set! count (add1 count))
    
  `(html (head (title "Counter"))
         (body ([bgcolor "white"])
               (p "This servlet was called " ,(number->string count)
                  " times since loaded on " ,date ".")))))