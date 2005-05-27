
;; Poor man's call tracer.
;; See doc.txt for information.

(module calltrace mzscheme
  (require "calltrace-lib.ss")
  
  (provide instrumenting-enabled)
  
  (current-eval calltrace-eval-handler))