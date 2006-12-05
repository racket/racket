;; This library is used by match.ss
(module render-test-list mzscheme
  
  (provide render-test-list)
  
  (require "render-sigs.ss"
           "render-test-list-impl.ss"
           "getbindings.ss"
           "ddk-handlers.ss"
           (lib "unit.ss"))
  
  (define-compound-unit/infer rtl@
    (import)
    (export render-test-list^)
    (link render-test-list@ getbindings@ ddk-handlers@))
  
  (define-values/invoke-unit/infer rtl@)

  )