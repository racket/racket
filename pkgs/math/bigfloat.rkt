#lang typed/racket/base

(require "private/bigfloat/bigfloat-struct.rkt"
         ;; These should be better first:
         ;"private/bigfloat/bigfloat-incomplete-gamma.rkt"
         ;"private/bigfloat/bigfloat-beta.rkt"
         ;"private/bigfloat/bigfloat-incomplete-beta.rkt"
         ;; This needs some things to be renamed first to avoid confusion (e.g. bflog+ isn't
         ;; at all like fllog+), and needs lower relative error near roots, especially bflog+
         ;"private/bigfloat/bigfloat-log-arithmetic.rkt"
         )

(provide (all-from-out
          "private/bigfloat/bigfloat-struct.rkt"
          ;"private/bigfloat/bigfloat-incomplete-gamma.rkt"
          ;"private/bigfloat/bigfloat-beta.rkt"
          ;"private/bigfloat/bigfloat-incomplete-beta.rkt"
          ;"private/bigfloat/bigfloat-log-arithmetic.rkt"
          ))
