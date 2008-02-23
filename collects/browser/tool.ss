(module tool mzscheme
  (require (lib "external.ss" "browser")
           mzlib/unit
           (lib "tool.ss" "drscheme"))
  (provide tool@)

  ;; to add a preference pannel to drscheme that sets the browser preference
  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      (define phase1 void)
      (define phase2 void)
      
      (install-help-browser-preference-panel))))
