#lang racket/base
(require "private/launch.rkt"
         (only-in "web-server.rkt" do-not-return))
(void (serve))
(do-not-return)
