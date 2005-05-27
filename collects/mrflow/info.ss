
(module info (lib "infotab.ss" "setup")
        
  ; for mzc
  (define compile-omit-files '("test.ss" "tests.ss" "primitives.ss"))
  
  ; for DrScheme
  (define name "MrFlow")
  (define tools '(("gui.ss")))
  (define tool-icons '(("mrflow.gif" "icons")))
  ; this name shows up in the "About Drscheme" menu
  (define tool-names '("MrFlow Static Debugger"))
  (define tool-urls '("http://www.plt-scheme.org/software/mrflow/"))
  )
