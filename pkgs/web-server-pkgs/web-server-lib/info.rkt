#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "base"
	       "net-lib"
               "compatibility-lib"
               "scribble-text-lib"
               "unstable-list-lib"
               "unstable-contract-lib"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"web-server\"")

(define pkg-authors '(jay))
