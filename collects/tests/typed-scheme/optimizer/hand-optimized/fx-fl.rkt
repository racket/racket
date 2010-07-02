(module fx-fl typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fx->fl 1))
