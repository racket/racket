
(module time-stamp mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "framework.ss" "framework"))
  
  (require "stamp.ss")
  
  (provide tool@)
  
  (require "stamp.ss")
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      (define (phase1) (void))
      (define (phase2) (void))
      (version:add-spec '-cvs stamp))))