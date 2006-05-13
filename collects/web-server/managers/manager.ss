(module manager mzscheme
  (require (lib "class.ss"))
  (provide (all-defined))
  
  (define manager<%> 
    (interface ()
      create-instance
      adjust-timeout!
      instance-lookup-data
      clear-continuations!
      continuation-store!
      continuation-lookup))
  
  (define-struct (exn:fail:servlet-manager:no-instance exn:fail) (expiration-handler))
  (define-struct (exn:fail:servlet-manager:no-continuation exn:fail) (expiration-handler)))