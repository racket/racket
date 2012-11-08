#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         planet2/util
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (shelly-install "remote/github"
                  "github://github.com/jeapostrophe/galaxy/master/tests/planet2/test-pkgs/planet2-test1")
  (shelly-install "remote/github with slash"
                  "github://github.com/jeapostrophe/galaxy/master/tests/planet2/test-pkgs/planet2-test1/")))
