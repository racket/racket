(module sxml-tree-trans mzscheme

  (provide SRV:send-reply
	   post-order pre-post-order replace-range)

  (require "assertions.ss")
  (require (lib "23.ss" "srfi"))

  (require (lib "include.ss"))
  (include "SXML-tree-trans.scm"))
