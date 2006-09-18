(module connection-structs mzscheme
  (require (lib "contract.ss"))
  (require "timer-structs.ss")
  
  (define-struct connection (timer i-port o-port custodian close? mutex)
    (make-inspector))
  
  (provide/contract
   [struct connection
           ([timer timer?]
            [i-port input-port?] [o-port output-port?] [custodian custodian?]
            [close? boolean?] [mutex semaphore?])]))