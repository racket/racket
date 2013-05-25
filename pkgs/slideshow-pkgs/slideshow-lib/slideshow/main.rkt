#lang racket
(require "base.rkt"
         "pict.rkt")
(provide (except-out (all-from-out racket
                                   "base.rkt"
                                   "pict.rkt")
                     printable<%>))

(module reader syntax/module-reader
  slideshow)
