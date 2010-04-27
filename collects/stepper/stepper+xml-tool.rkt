(module stepper+xml-tool mzscheme
  (require mzlib/unit
           drscheme/tool
           "stepper-tool.ss"
           "xml-tool.ss"
           "view-controller.ss"
           "private/shared.ss")

  (provide tool@)

  ;; the xml and stepper tools are combined, so that the stepper can create XML
  ;; snips.  note that both of these tools provide 'void' for phase1 and phase2
  ;; (which together make up the tool-exports^), so we can provide either one
  ;; of these for the compound unit.  Doesn't matter.
  
  ;; NNNURRRG!  This is not true any more.  But that should be okay, because the
  ;; stepper-tool phase1 is the non-void one. -- JBC, 2006-09-28

  (define tool@
    (compound-unit/infer
      (import drscheme:tool^)
      (export STEPPER-TOOL)
      (link xml-tool@ 
            view-controller@
            [((STEPPER-TOOL : drscheme:tool-exports^)) stepper-tool@]))))
