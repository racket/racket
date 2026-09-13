#lang racket/base
(require (submod "scribble.rkt" docs-to-destdir))

(provide move-rendered-docs-to-destdir)

(module+ main
  (require racket/cmdline)
  (command-line
   #:args
   (destdir . dir)
   (move-rendered-docs-to-destdir dir destdir)))
