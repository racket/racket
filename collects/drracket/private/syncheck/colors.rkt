#lang racket/base
(require string-constants/string-constant
         framework
         racket/class
         racket/gui/base)
(provide (all-defined-out))

(define cs-my-obligation-color "my obligations")
(define cs-their-obligation-color "my assumptions")
(define cs-both-obligation-color "both obligations")
(define cs-unk-obligation-color "unknown obligations")

(define lexically-bound-variable-style-pref 'drracket:check-syntax:lexically-bound)
(define imported-variable-style-pref 'drracket:check-syntax:imported)
(define set!d-variable-style-pref 'drracket:check-syntax:set!d)

(define lexically-bound-variable-style-name (symbol->string lexically-bound-variable-style-pref))
(define imported-variable-style-name (symbol->string imported-variable-style-pref))
(define set!d-variable-style-name (symbol->string set!d-variable-style-pref))

(define my-obligation-style-pref 'drracket:check-syntax:my-obligation-style-pref)
(define their-obligation-style-pref 'drracket:check-syntax:their-obligation-style-pref)
(define unk-obligation-style-pref 'drracket:check-syntax:unk-obligation-style-pref)
(define both-obligation-style-pref 'drracket:check-syntax:both-obligation-style-pref)

(define my-obligation-style-name (symbol->string my-obligation-style-pref))
(define their-obligation-style-name (symbol->string their-obligation-style-pref))
(define unk-obligation-style-name (symbol->string unk-obligation-style-pref))
(define both-obligation-style-name (symbol->string both-obligation-style-pref))

(define error-style-name (scheme:short-sym->style-name 'error))
;(define constant-style-name (scheme:short-sym->style-name 'constant))

(define (syncheck-add-to-preferences-panel parent)
  (color-prefs:build-color-selection-panel parent
                                           lexically-bound-variable-style-pref
                                           lexically-bound-variable-style-name
                                           (string-constant cs-lexical-variable))
  (color-prefs:build-color-selection-panel parent
                                           imported-variable-style-pref
                                           imported-variable-style-name
                                           (string-constant cs-imported-variable))
  (color-prefs:build-color-selection-panel parent
                                           set!d-variable-style-pref
                                           set!d-variable-style-name
                                           (string-constant cs-set!d-variable))
  (color-prefs:build-color-selection-panel parent
                                           my-obligation-style-pref
                                           my-obligation-style-name
                                           cs-my-obligation-color)
  (color-prefs:build-color-selection-panel parent
                                           their-obligation-style-pref
                                           their-obligation-style-name
                                           cs-their-obligation-color)
  (color-prefs:build-color-selection-panel parent
                                           unk-obligation-style-pref
                                           unk-obligation-style-name
                                           cs-unk-obligation-color)
  (color-prefs:build-color-selection-panel parent
                                           both-obligation-style-pref
                                           both-obligation-style-name
                                           cs-both-obligation-color))

(color-prefs:register-color-preference lexically-bound-variable-style-pref
                                       lexically-bound-variable-style-name
                                       (make-object color% 81 112 203)
                                       (make-object color% 50 163 255))
(color-prefs:register-color-preference set!d-variable-style-pref
                                       set!d-variable-style-name
                                       (send the-color-database find-color "firebrick")
                                       (send the-color-database find-color "pink"))
(color-prefs:register-color-preference imported-variable-style-pref
                                       imported-variable-style-name
                                       (make-object color% 68 0 203)
                                       (make-object color% 166 0 255))
(color-prefs:register-color-preference my-obligation-style-pref
                                       my-obligation-style-name
                                       (send the-color-database find-color "firebrick")
                                       (send the-color-database find-color "pink"))
(color-prefs:register-color-preference their-obligation-style-pref
                                       their-obligation-style-name
                                       (make-object color% 0 116 0)
                                       (send the-color-database find-color "limegreen"))
(color-prefs:register-color-preference unk-obligation-style-pref
                                       unk-obligation-style-name
                                       (send the-color-database find-color "black")
                                       (send the-color-database find-color "white"))
(color-prefs:register-color-preference both-obligation-style-pref
                                       both-obligation-style-name
                                       (make-object color% 139 142 28)
                                       (send the-color-database find-color "khaki"))

