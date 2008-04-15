#lang scheme/base

(require "html.ss")
(provide (except-out (all-from-out "html.ss")
                     link struct:link make-link link?)
         (rename-out [link alink]
                     [struct:link struct:alink]
                     [make-link make-alink]
                     [link? alink?]))
