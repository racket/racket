(module timer-structs mzscheme
  (require (lib "contract.ss"))
  
  (define-struct timer (expire-seconds))
  (provide/contract
   [struct timer ([expire-seconds number?])]))