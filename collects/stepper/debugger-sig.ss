(module debugger-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide debugger-model^
           debugger-vc^)
  
  (define-signature debugger-model^
    (go-semaphore
     user-custodian
     go))
  
  (define-signature debugger-vc^
    (receive-result)))
