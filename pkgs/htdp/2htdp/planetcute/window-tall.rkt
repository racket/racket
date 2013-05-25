#lang racket/base
(provide window-tall)
(require racket/draw racket/runtime-path)
(define-runtime-path window-tall-img "window-tall.png")
(define window-tall (read-bitmap window-tall-img))
