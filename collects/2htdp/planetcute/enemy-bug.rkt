#lang racket/base
(provide enemy-bug)
(require racket/draw racket/runtime-path)
(define-runtime-path enemy-bug-img "Enemy Bug.png")
(define enemy-bug (read-bitmap enemy-bug-img))
