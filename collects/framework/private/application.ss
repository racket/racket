(module application (lib "a-unit.ss")
  (require "sig.ss")
  
  (import)
  
  (export framework:application^)
  
  (define current-app-name (make-parameter
                            "MrEd"
                            (λ (x)
                              (unless (string? x)
                                (error 'current-app-name
                                       "the app name must be a string"))
                              x))))