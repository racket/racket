#lang scheme/base

(require rackunit)
(require rackunit/text-ui)
(require "all-srfi-tests.rkt")

(run-tests all-srfi-tests)
