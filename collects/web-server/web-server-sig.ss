(module web-server-sig mzscheme
  (require "private/dispatch-server-sig.ss")
  (provide (rename dispatch-server^ web-server^)))