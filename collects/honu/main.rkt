#lang honu/private

(require (prefix-in racket: (combine-in racket/base racket/list racket/file)))

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

(racket:provide sqr sqrt sin max min
         number? symbol?
         null
         null?
         length
         substring
         format
         integer
         cos sin
         random
         filter
         append
         values
         hash
         regexp
         error
         (racket:rename-out
           [honu-cond cond]
           [honu-time time]
           [time-apply time_apply]
           [null empty]
           [make-hash mutable_hash]
           [hash-set! hash_update]
           [hash-ref hash_lookup]
           [current-inexact-milliseconds currentMilliseconds]
           [string-length string_length]
           [string-append string_append]
           [current-command-line-arguments commandLineArguments]
           [racket:find-files find_files]
           [racket:empty? empty?]
           [regexp-match regexp_match]
           [racket:rest rest]))
