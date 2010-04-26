#lang scheme/base
(require "base.ss"
         "private/manual-style.ss"
         "private/manual-scheme.ss"
         "private/manual-mod.ss"
         "private/manual-tech.ss"
         "private/manual-bib.ss"
         "private/manual-proc.ss"
         "private/manual-form.ss"
         "private/manual-class.ss"
         "private/manual-unit.ss"
         "private/manual-vars.ss"
         "private/manual-bind.ss")

(provide unsyntax
         make-binding-redirect-elements
         defidentifier
         (all-from-out "base.ss"
                       "private/manual-style.ss"
                       "private/manual-scheme.ss"
                       "private/manual-mod.ss"
                       "private/manual-tech.ss"
                       "private/manual-bib.ss"
                       "private/manual-form.ss"
                       "private/manual-class.ss"
                       "private/manual-unit.ss")
         (except-out (all-from-out "private/manual-vars.ss")
                     *deftogether)
         (except-out (all-from-out "private/manual-proc.ss")
                     *defthing))

