;;; list.scm  --  Jens Axel Søgaard

; The default list implementation is skew-binary-random-access-lists.

(module list mzscheme
  (require "private/skew-binary-random-access-list.scm")
  (provide (all-from "private/skew-binary-random-access-list.scm")))