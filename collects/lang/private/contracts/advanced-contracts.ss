(module advanced-contracts mzscheme
  (require "contracts-helpers.ss")
  (require (lib "list.ss"))
  
  (provide void-contract)
  
  (define void-contract (lambda (stx) (build-flat-contract void? 'void stx))))