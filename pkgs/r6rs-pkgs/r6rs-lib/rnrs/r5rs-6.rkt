#lang scheme/base

(require (prefix-in r5rs: r5rs))

(provide exact->inexact
         inexact->exact
         quotient
         remainder
         modulo
         (rename-out [r5rs:delay delay]
                     [r5rs:force force]
                     [r5rs:null-environment null-environment]
                     [r5rs:scheme-report-environment scheme-report-environment]))
