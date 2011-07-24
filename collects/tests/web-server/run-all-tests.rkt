#lang racket/base
(require "all-web-server-tests.rkt")

(require rackunit/text-ui)
(run-tests all-web-server-tests)

#;(require rackunit/gui)
#;(test/gui all-web-server-tests)
