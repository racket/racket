(module manager mzscheme
  (require (lib "contract.ss"))
  (require "../servlet-structs.ss")
  
  (define-struct manager (create-instance 
                          adjust-timeout!
                          instance-lookup-data
                          instance-lock!
                          instance-unlock!
                          clear-continuations!
                          continuation-store!
                          continuation-lookup))
  
  (define-struct (exn:fail:servlet-manager:no-instance exn:fail) (expiration-handler))
  (define-struct (exn:fail:servlet-manager:no-continuation exn:fail) (expiration-handler))
  
  (provide/contract
   [struct manager ([create-instance (any/c (-> void) . -> . number?)]
                    [adjust-timeout! (number? number? . -> . void)]
                    [instance-lookup-data (number? . -> . any/c)]
                    [instance-lock! (number? . -> . void)]
                    [instance-unlock! (number? . -> . void)]
                    [clear-continuations! (number? . -> . void)]
                    [continuation-store! (number? procedure? expiration-handler? . -> . (list/c number? number?))]
                    [continuation-lookup (number? number? number? . -> . procedure?)])]
   [struct (exn:fail:servlet-manager:no-instance exn:fail) 
           ([msg string?]
            [continuation-marks continuation-mark-set?]
            [expiration-handler expiration-handler?])]
   [struct (exn:fail:servlet-manager:no-continuation exn:fail)
           ([msg string?]
            [continuation-marks continuation-mark-set?]
            [expiration-handler expiration-handler?])]))