
(module embed mzscheme
  (require "interfaces.ss"
           "widget.ss"
           "keymap.ss"
           "params.ss"
           "partition.ss")
  
  (provide (all-from "interfaces.ss")
           (all-from "widget.ss")
           (all-from "keymap.ss")
           (all-from "params.ss")
           identifier=-choices))
