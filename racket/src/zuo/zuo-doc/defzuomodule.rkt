#lang at-exp racket/base
(require scribble/manual)

(provide defzuomodule)

@(define-syntax-rule (defzuomodule zuo/x)
   (begin
     @defmodule[zuo/x #:no-declare #:packages ()]
     @declare-exporting[zuo zuo/x #:packages () #:use-sources (zuo-doc/fake-zuo)]
     @para{The @racketmodname[zuo/x] module is reprovided by @racketmodname[zuo].}))
