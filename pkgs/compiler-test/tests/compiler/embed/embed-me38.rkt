#lang racket/base
(require racket/runtime-path
         (for-syntax racket/base))

(define-runtime-path license '(share "LICENSE-libscheme.txt"))

(if (absolute-path? license)
    "found license"
    "not found")



