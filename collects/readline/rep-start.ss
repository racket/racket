;; This module initializes readline unconditionally, "rep.ss" uses it if we're
;; using a `terminal-port?' for input.

(module rep-start mzscheme
  (require "pread.ss")

  ;; Change the input port and readline-prompt hook
  (current-input-port readline-input)
  (current-prompt-read read-cmdline-syntax))
