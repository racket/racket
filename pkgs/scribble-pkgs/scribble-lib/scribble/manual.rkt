#lang scheme/base
(require "base.rkt"
         "private/manual-style.rkt"
         "private/manual-scheme.rkt"
         "private/manual-code.rkt"
         "private/manual-mod.rkt"
         "private/manual-tech.rkt"
         "private/manual-bib.rkt"
         "private/manual-proc.rkt"
         "private/manual-form.rkt"
         "private/manual-class.rkt"
         "private/manual-unit.rkt"
         "private/manual-vars.rkt"
         "private/manual-bind.rkt"
         "private/manual-utils.rkt"
         "private/manual-defaults.rkt"
         "private/manual-history.rkt")

(provide unsyntax
         make-binding-redirect-elements
         defidentifier
         current-display-width
         manual-doc-style
         (all-from-out "base.rkt"
                       "private/manual-style.rkt"
                       "private/manual-scheme.rkt"
                       "private/manual-code.rkt"
                       "private/manual-mod.rkt"
                       "private/manual-tech.rkt"
                       "private/manual-bib.rkt"
                       "private/manual-form.rkt"
                       "private/manual-class.rkt"
                       "private/manual-unit.rkt"
                       "private/manual-history.rkt")
         (except-out (all-from-out "private/manual-vars.rkt")
                     *deftogether)
         (except-out (all-from-out "private/manual-proc.rkt")
                     *defthing))
