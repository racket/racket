#lang racket
(require rnrs/io/ports-6)

;; Make sure that an R6RS port plays ok with sync,
;; particularly when no input is available.
(void (sync/timeout 0 (standard-input-port)))
