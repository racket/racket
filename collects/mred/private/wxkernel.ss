
;; Adding the "wx:" prefix here can prevent keeping
;;  redundant import information in all the other files.
(module wxkernel mzscheme
  (require (prefix wx: "kernel.ss"))
  (provide (all-from "kernel.ss")))

