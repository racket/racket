#lang racket/base
(require "host/bootstrap-thread.rkt" ; must be before "main.rkt"
         "main.rkt"
         "../thread/main.rkt")

(provide (all-from-out "main.rkt")
         (all-from-out "../thread/main.rkt"))
