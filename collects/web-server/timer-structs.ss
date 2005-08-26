(module timer-structs mzscheme
  (require (lib "contract.ss"))
  
  (define-struct timer (evt expire-seconds action))
  (provide/contract
   [struct timer ([evt evt?]
                  [expire-seconds number?]
                  [action (-> void)])]))