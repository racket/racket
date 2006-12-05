
(module time-stamp mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unit.ss")
           (lib "framework.ss" "framework"))
  
  (require "stamp.ss")
  
  (provide tool@)
  
  (require "stamp.ss")
  
  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      (define (phase1) (void))
      (define (phase2) (void))
      (version:add-spec '-svn stamp))))
