#lang racket/base
(require rackunit
         racket/system
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         pkg/util
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-case
  "Each command has an associated help"
  $ "raco pkg -h" =exit> 1
  $ "raco pkg help"
  $ "raco pkg install -h"
  $ "raco pkg update -h"
  $ "raco pkg remove -h"
  $ "raco pkg show -h"
  $ "raco pkg create -h"
  $ "raco pkg config -h"))
