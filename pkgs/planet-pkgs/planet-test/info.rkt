#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"planet-lib\"")

(define pkg-authors '(mflatt))
(define build-deps '("eli-tester"
                     "planet-lib"
                     "rackunit-lib"
                     "scheme-lib"
                     "scribble-lib"))

(define test-omit-paths
  '("tests/planet/cmdline-tool.rkt"
    "tests/planet/docs-build.rkt"
    "tests/planet/lang.rkt"
    "tests/planet/submod.rkt"
    "tests/planet/test-docs-complete.rkt"
    "tests/planet/thread-safe-resolver.rkt"
    "tests/planet/version.rkt"
    "tests/planet/examples"))
