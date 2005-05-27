(module info (lib "infotab.ss" "setup")
  (define name "Stepper")
  (define doc.txt "doc.txt")
  
  (define tools '(("stepper+xml-tool.ss")
                  #;("debugger-tool.ss")
                  ))
  
  (define tool-names (list "The Stepper" 
			   #;"The Debugger"
			   ))
  
  (define tool-icons (list '("foot-up.png" "icons") 
			   #;'("foot-up.png" "icons")
			   ))
  (define compile-omit-files `("private/test-annotate.ss" "debugger-tool.ss"))
)
