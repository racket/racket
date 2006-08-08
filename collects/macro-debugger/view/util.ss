
(module util mzscheme
  (require (lib "class.ss"))
  (provide override/return-false)

  (define-syntax override/return-false
    (syntax-rules ()
      [(override/return-false m ...)
       (begin (define/override (m) #f) ...)]))
  
  )