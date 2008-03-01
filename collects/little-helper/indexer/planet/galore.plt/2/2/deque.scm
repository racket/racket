;;; deque.scm  --  Jens Axel Søgaard

; NOTES
;   For applications which don't need persistence and
;   more than amortized time bounds is uneeded,
;   the batched queue/deque is the best choice.

; This provides the default deque implementation.

(module deque mzscheme
  (require "batched-deque.scm")
  (provide (all-from "batched-deque.scm")))

