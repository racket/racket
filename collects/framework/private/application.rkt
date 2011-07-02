#lang scheme/unit

  (require "sig.rkt")
  
  (import)
  
  (export framework:application^)
  
  (define current-app-name (make-parameter
                            "GRacket"
                            (λ (x)
                              (unless (string? x)
                                (error 'current-app-name
                                       "the app name must be a string"))
                              x)))
