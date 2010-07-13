(module in-list typed/scheme 
  (require racket/unsafe/ops)
  (for: ((i : Natural '(1 2 3)))
    (display i)))
