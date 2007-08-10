(module manager mzscheme
  (require (lib "contract.ss"))
  (require "../servlet/servlet-structs.ss")
  
  (define-struct manager (create-instance 
                          adjust-timeout!
                          clear-continuations!
                          continuation-store!
                          continuation-lookup))
  
  (define-struct (exn:fail:servlet-manager:no-instance exn:fail) (expiration-handler))
  (define-struct (exn:fail:servlet-manager:no-continuation exn:fail) (expiration-handler))
  
  (provide/contract
   [struct manager ([create-instance ((-> void) . -> . number?)]
                    [adjust-timeout! (number? number? . -> . void)]
                    [clear-continuations! (number? . -> . void)]
                    [continuation-store! (number? any/c expiration-handler? . -> . (list/c number? number?))]
                    [continuation-lookup (number? number? number? . -> . any/c)])]
   [struct (exn:fail:servlet-manager:no-instance exn:fail) 
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [expiration-handler expiration-handler?])]
   [struct (exn:fail:servlet-manager:no-continuation exn:fail)
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [expiration-handler expiration-handler?])]))