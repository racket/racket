#lang racket/base
(provide stone-block-tall)
(require racket/draw racket/runtime-path)
(define-runtime-path stone-block-tall-img "Stone Block Tall.png")
(define stone-block-tall (read-bitmap stone-block-tall-img))
