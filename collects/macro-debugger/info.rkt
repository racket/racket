#lang setup/infotab

(define drracket-tools '(["tool.rkt"]))
(define drracket-tool-names '("Macro Stepper"))
(define drracket-tool-icons (list '("macro-stepper-32x32.png" "icons")))
(define scribblings '(("macro-debugger.scrbl" () (tool-library))))

(define raco-commands
  '(("check-requires"
     (submod macro-debugger/analysis/check-requires main)
     "check for useless requires"
     #f)
    ("show-dependencies"
     (submod macro-debugger/analysis/show-dependencies main)
     "show module dependencies"
     #f)))
