#lang racket/base
(provide enemy-bug)
(require racket/draw racket/runtime-path)
(define-runtime-path enemy-bug-img "enemy-bug.png")
(define enemy-bug (read-bitmap enemy-bug-img))
