#lang racket/base
(require racket/class)

(define-syntax-rule
  (define/provide-local-member-name x ...)
  (begin
    (define-local-member-name x ...)
    (provide x ...)))

(define/provide-local-member-name
  get-visible-defs
  set-visible-defs
  set-focus-d/i
  get-i
  set-i
  insert-auto-text)

;; from module-language-tools.rkt
(define/provide-local-member-name
  when-initialized
  ;move-to-new-language
  get-in-module-language?)

;; for keybindings (otherwise private)
(define/provide-local-member-name
  jump-to-previous-error-loc
  jump-to-next-error-loc)

;; defined in module-language.rkt
(define/provide-local-member-name
  set-lang-wants-big-defs/ints-labels?)

;; used by the test suite to tell when the
;; online check syntax has finished
(define/provide-local-member-name
  get-online-expansion-colors)

;; used by the module language
(define/provide-local-member-name
  frame-show-bkg-running
  set-bottom-bar-most-recent-jumped-to-loc
  set-expand-error/status
  update-frame-expand-error
  expand-error-next
  expand-error-prev
  hide-module-language-error-panel
  fetch-data-to-send
  clear-old-error
  set-bottom-bar-status
  
  get-oc-status
  set-oc-status
  
  set-dep-paths
  set-dirty-if-dep)

;; mode changing definitions text mixin
(define-local-member-name 
  maybe-change-language
  change-mode-to-match
  get-current-mode)


(provide (struct-out drracket:language-configuration:language-settings))
;; type language-settings = (language-settings (instanceof language<%>) settings)
(define-struct drracket:language-configuration:language-settings (language settings))

(provide (struct-out drracket:unit:teachpack-callbacks))
(define-struct drracket:unit:teachpack-callbacks (get-names add remove remove-all))

(provide (struct-out drracket:language:simple-settings))
(define-struct drracket:language:simple-settings (case-sensitive 
                                                  printing-style
                                                  fraction-style
                                                  show-sharing
                                                  insert-newlines
                                                  annotations))

(provide (struct-out drracket:language:text/pos))
(define-struct drracket:language:text/pos (text start end))
  

(provide (struct-out drracket:modes:mode))
(struct drracket:modes:mode (name surrogate repl-submit matches-language intended-to-edit-programs?))
