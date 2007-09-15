(module stepper-language-interface mzscheme
  
  (require (lib "class.ss"))
  (provide stepper-language<%>)
    
  (define stepper-language<%>
    (interface ()
      stepper:supported?
      stepper:enable-let-lifting?)))
