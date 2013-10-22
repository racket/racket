#lang info

(define collection 'multi)

(define deps
  '("db-lib" "db-doc" "db-test" "base"))
(define implies
  '("db-lib" "db-doc" "db-test"))

(define pkg-desc "Database connectivity")

(define pkg-authors '(ryanc))
