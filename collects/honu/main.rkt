#lang honu/private

(require (prefix-in racket: (combine-in racket/base racket/list)))

;; require's and provide's a module
(define-syntax-rule (provide-module module ...)
  (begin
    (begin
      (racket:require module)
      (racket:provide [all-from-out module]))
    ...))

(provide-module "core/main.rkt"
                "private/common.rkt"
                "private/common.honu"
                ;;"private/struct.honu"
                ;;"private/function.honu"
                )

(racket:provide sqr sqrt sin max
         number? symbol?
         null
         null?
         length
         substring
         format
         integer
         cos sin
         random
         values
         (racket:rename-out [honu-cond cond]
                     [null empty]
                     [current-inexact-milliseconds currentMilliseconds]
                     [string-length string_length]
                     [racket:empty? empty?]
                     [racket:first first]
                     [racket:rest rest]))
