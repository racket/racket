#lang racket/base
(require "private/search.rkt")

(provide
 (all-from-out "private/search.rkt"))

;; acts like `raco docs` when run directly:
(module main racket/base
  (require "private/command.rkt"))
