#lang racket/base

(provide documentation planet packages)

(require "docs.rkt"                   ; for the doc tweaking script
         ;; these need to be copied to the service
         "packages.rkt" "planet.rkt" "blog.rkt" 
         "pre.rkt" "git.rkt"          ; / these are used on the
         "mailman.rkt" "dirlist.rkt"  ; \ server directly
         "wiki.rkt")
