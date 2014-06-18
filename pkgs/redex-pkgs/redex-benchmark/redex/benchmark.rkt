#lang racket/base

(require "benchmark/private/rw-defs.rkt"
         "benchmark/private/logging.rkt"
         "benchmark/private/gen-run.rkt")

(provide define-rewrite
         define-rewrite/compose
         include/rewrite
         run-gen-and-check
         run-gen-and-check/mods
         bmark-log-directory
         benchmark-logging-to
         bmark-log
         (struct-out run-results))