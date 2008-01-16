#lang scheme/base

(require "xml.ss")
(provide (except-out (all-from-out "xml.ss")
                     pi struct:pi pi? make-pi pi-target-name pi-instruction)
         (rename-out [pi p-i]
                     [struct:pi struct:p-i]
                     [pi? p-i?]
                     [make-pi make-p-i]
                     [pi-target-name p-i-target-name]
                     [pi-instruction p-i-instruction]))
