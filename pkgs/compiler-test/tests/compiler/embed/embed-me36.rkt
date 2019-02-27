#lang racket/base

;; Make sure this module doesn't get compiled, because the intent is
;; to test `raco exe` on the module in source form.

(module sub racket/base
  'ok-36)

(require 'sub)
