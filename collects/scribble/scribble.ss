#lang scheme/base

(require "run.ss")

(define files (get-command-line-files
               (current-command-line-arguments)))


(build-docs-files files)
