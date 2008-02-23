(module debugger-sig mzscheme
  (require mzlib/unitsig)

  (provide debugger-model^
           debugger-vc^)

  (define-signature debugger-model^
    (go-semaphore
     user-custodian
     go))

  (define-signature debugger-vc^
    (receive-result)))
