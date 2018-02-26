#lang racket/base
(require (only-in '#%linklet primitive-table)
         "../../thread/bootstrap-main.rkt"
         "bootstrap-rktio.rkt")

;; Use the "thread" layer implementation in combination with
;; with the rktio bootstrap bindings.

(primitive-table '#%thread #%thread-instance)
