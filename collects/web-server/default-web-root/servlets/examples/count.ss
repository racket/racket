(module count mzscheme
  (require (lib "date.ss"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define count 0)
  (define a-date (date->string (seconds->date (current-seconds)) 'time-too))
  (define (start initial-request)
    (define other-count 0)
    
    (set! other-count (add1 other-count))
    (set! count (add1 count))
    
    `(html (head (title "Counter"))
           (body ([bgcolor "white"])
                 (p "This servlet was called " ,(number->string count)
                    " times and " ,(number->string other-count)
                    " times since loaded on " ,a-date ".")))))