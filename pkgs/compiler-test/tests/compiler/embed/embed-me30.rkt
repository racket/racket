#lang racket
(require "embed-place2.rkt")
(module+ main
  (void (sync (place-dead-evt (start-place)))))
