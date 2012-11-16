#lang racket/base

(require racket/unit
         drracket/tool
         "stepper-tool.rkt"
         "xml-tool.rkt"
         "private/view-controller.rkt")

(provide tool@)

;; the xml and stepper tools are combined, so that the stepper can create XML
;; snips. 

(define tool@
  (compound-unit/infer
   (import drracket:tool^)
   (export STEPPER-TOOL)
   (link xml-tool@ 
         view-controller@
         [((STEPPER-TOOL : drracket:tool-exports^)) stepper-tool@])))
