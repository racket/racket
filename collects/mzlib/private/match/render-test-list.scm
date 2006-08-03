;; This library is used by match.ss
(module render-test-list mzscheme
  
  (provide render-test-list)
  
  (require "render-sigs.ss"
           "render-test-list-impl.ss"
           "getbindings.ss"
           "ddk-handlers.ss"
           (lib "unitsig.ss"))
  
  (define rtl@ 
    (compound-unit/sig
      (import)
      (link (RTL : render-test-list^ (render-test-list@ DDK GET))
            (GET : getbindings^ (getbindings@ RTL))
            (DDK : ddk-handlers^ (ddk-handlers@ GET RTL))
            )
      (export (var (RTL render-test-list)))
      ))
  
  (define-values/invoke-unit/sig render-test-list^ rtl@)
  
  )