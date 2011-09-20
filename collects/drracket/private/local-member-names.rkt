#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define-local-member-name
  get-visible-defs
  set-visible-defs
  set-focus-d/i
  get-i
  set-i
  insert-auto-text)

;; from module-language-tools.rkt
(define-local-member-name when-initialized #;move-to-new-language get-in-module-language?)
  
;; for keybindings (otherwise private)
(define-local-member-name
  jump-to-previous-error-loc
  jump-to-next-error-loc)
