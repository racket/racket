(module IllegalThreadStateException mzscheme
  (require "Object-composite.ss")
  (provide
   IllegalThreadStateException
   guard-convert-IllegalThreadStateException
   convert-assert-IllegalThreadStateException
   wrap-convert-assert-IllegalThreadStateException
   dynamic-IllegalThreadStateException/c
   static-IllegalThreadStateException/c))
