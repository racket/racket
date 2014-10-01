#lang racket/base
(provide (all-defined-out))

(define cs-my-obligation-color "my obligations")
(define cs-their-obligation-color "my assumptions")
(define cs-both-obligation-color "both obligations")
(define cs-unk-obligation-color "unknown obligations")

(define lexically-bound-variable-style-pref 'drracket:check-syntax:lexically-bound)
(define imported-variable-style-pref 'drracket:check-syntax:imported)
(define set!d-variable-style-pref 'drracket:check-syntax:set!d)
(define unused-require-style-pref 'drracket:check-syntax:unused-require)
(define free-variable-style-pref 'drracket:check-syntax:free-variable)

(define lexically-bound-variable-style-name (symbol->string lexically-bound-variable-style-pref))
(define imported-variable-style-name (symbol->string imported-variable-style-pref))
(define set!d-variable-style-name (symbol->string set!d-variable-style-pref))
(define unused-require-style-name (symbol->string unused-require-style-pref))
(define free-variable-style-name (symbol->string free-variable-style-pref))

(define my-obligation-style-pref 'drracket:check-syntax:my-obligation-style-pref)
(define their-obligation-style-pref 'drracket:check-syntax:their-obligation-style-pref)
(define unk-obligation-style-pref 'drracket:check-syntax:unk-obligation-style-pref)
(define both-obligation-style-pref 'drracket:check-syntax:both-obligation-style-pref)

(define my-obligation-style-name (symbol->string my-obligation-style-pref))
(define their-obligation-style-name (symbol->string their-obligation-style-pref))
(define unk-obligation-style-name (symbol->string unk-obligation-style-pref))
(define both-obligation-style-name (symbol->string both-obligation-style-pref))

