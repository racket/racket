(module info (lib "infotab.ss" "setup")
  (define name "Stepper")
  (define doc.txt "doc.txt")

  (define tools '(("stepper+xml-tool.ss")
                  ;; ("debugger-tool.ss")
                  ))

  (define tool-names (list "The Stepper"
                           ;; "The Debugger"
                           ))

  (define tool-icons (list '("foot-up.png" "icons")
                           ;; #f
                           ))

  (define compile-omit-files `("debugger-tool.ss"))

  )
