#lang info

(define collection 'multi)

(define deps '("racket-test"
               "gui-test"
               "db-test"
               "htdp-test"
               "redex-test"
               "drracket-test"
               "profile-test"
               "srfi-test"
               "errortrace-test"
               "r6rs-test"
               "web-server-test"
               "typed-racket-test"
               "xrepl-test"
               "scribble-test"
               "unstable-test"))

(define pkg-desc "tests for \"main-distribution\"")

(define pkg-authors '(eli jay matthias mflatt robby))
