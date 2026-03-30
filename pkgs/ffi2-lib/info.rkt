#lang info

(define collection 'multi)

(define deps '(["base" #:version "9.1.0.12"]))

(define pkg-desc "implementation (no documentation) part of \"ffi2\"")

(define pkg-authors '(mflatt))

(define version "0.1")

(define license
  '(Apache-2.0 OR MIT))

;; disable DrDr testing of files in this directory, for now
(define test-omit-paths 'all)
