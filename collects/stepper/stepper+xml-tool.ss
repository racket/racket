(module stepper+xml-tool mzscheme
  (require (lib "unitsig.ss")
           (lib "tool.ss" "drscheme")
           "stepper-tool.ss"
           "xml-tool.ss")

  (provide tool@)
  
  ;; the xml and stepper tools are combined, so that the stepper can create XML snips.
  ;; note that both of these tools provide 'void' for phase1 and phase2 (which together
  ;; make up the tool-exports^), so we can provide either one of these for the compound
  ;; unit.  Doesn't matter.
  
  (define tool@
    (compound-unit/sig
      (import (TOOL-IMPORTS : drscheme:tool^))
      (link (XML-TOOL : (xml-snip% scheme-snip%) (xml-tool@ TOOL-IMPORTS))
            (STEPPER-TOOL :  drscheme:tool-exports^ (stepper-tool@ TOOL-IMPORTS XML-TOOL)))
      (export (open STEPPER-TOOL)))))