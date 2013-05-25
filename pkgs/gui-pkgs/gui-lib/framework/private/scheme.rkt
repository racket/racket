#lang racket/base

;; these are the names that used to be a part
;; of the framework with the `scheme:' prefix 
;; and now are primarily used with the `racket:'
;; prefix. This list is used to build documentation
;; and renaming exports

(provide racket:ids)
(define racket:ids
  '(text<%>
    text-mixin
    text%
    
    text-mode<%>
    text-mode-mixin
    text-mode%
    
    set-mode-mixin
    
    sexp-snip%
    sexp-snip<%>
    get-wordbreak-map
    init-wordbreak-map
    get-keymap
    setup-keymap
    add-preferences-panel
    add-coloring-preferences-panel
    
    get-color-prefs-table
    get-white-on-black-color-prefs-table
    short-sym->pref-name
    short-sym->style-name
    
    text-balanced?))
