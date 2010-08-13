#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(unsafe-string-length "eh")
(unsafe-bytes-length #"eh")
