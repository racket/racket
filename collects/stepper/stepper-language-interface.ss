(module stepper-language-interface mzscheme
  
  (require (lib "class.ss"))
  (provide stepper-language<%>)
    
  (define stepper-language<%>
    (interface () stepper:enable-let-lifting?)))