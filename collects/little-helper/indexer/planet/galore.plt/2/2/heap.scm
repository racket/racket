;;; heap.scm  --  Jens Axel Søgaard

; This provides the default heap implementation.

(module heap mzscheme
  (require "leftist-heap.scm")
  (provide (all-from "leftist-heap.scm")))


