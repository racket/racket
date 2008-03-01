;;; queue.scm  --  Jens Axel Søgaard

; NOTES
;   For applications which don't need persistence and
;   more than amortized time bounds is uneeded,
;   the batched queue/deque is the best choice.


; This provides the default queue implementation.

(module queue mzscheme
  (require "batched-queue.scm")
  (provide (all-from "batched-queue.scm")))

