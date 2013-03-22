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
         pkg/util
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-case
  "Each command has an associated help"
  $ "raco pkg -h"
  $ "raco pkg install -h"
  $ "raco pkg update -h"
  $ "raco pkg remove -h"
  $ "raco pkg show -h"
  $ "raco pkg create -h"
  $ "raco pkg config -h"))
