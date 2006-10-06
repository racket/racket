
(module embed mzscheme
  (require "interfaces.ss"
           "implementation.ss"
           "params.ss"
           "partition.ss")
  
  (provide (all-from "interfaces.ss")
           (all-from "implementation.ss")
           (all-from "params.ss")
           identifier=-choices))
