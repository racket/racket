#lang racket/base

(with-handlers ([exn?
                 (λ (x)
                   (exit 42))])
  (dynamic-require 'pkg-a #f))

(printf "pkg-b dep\n")
(exit 0)
