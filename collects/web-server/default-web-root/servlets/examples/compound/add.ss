(module add mzscheme
  (require "helper.ss")
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    `(html (head (title "Sum"))
           (body ([bgcolor "white"])
                 (p "The sum is "
                    ,(number->string (+ (get-number "the first number to add")
                                        (get-number "the second number to add"))))))))