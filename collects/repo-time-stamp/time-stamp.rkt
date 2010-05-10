
(module time-stamp mzscheme
  (require drscheme/tool
           mzlib/unit
           framework)
  
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
