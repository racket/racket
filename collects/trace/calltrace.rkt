;; Poor man's call tracer.

(module calltrace mzscheme
  (require "calltrace-lib.rkt")
  (provide instrumenting-enabled)
  (current-eval calltrace-eval-handler))
