#lang setup/infotab

(define tools '(("stepper+xml-tool.ss")
                ;; ("debugger-tool.ss")
                ))

(define tool-names (list "The Stepper"
                         ;; "The Debugger"
                         ))

(define tool-icons (list '("foot-up.png" "icons")
                         ;; #f
                         ))

(define compile-omit-paths '("debugger-tool.ss"))
