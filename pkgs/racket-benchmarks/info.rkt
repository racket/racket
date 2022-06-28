#lang info
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "r5rs-lib"
               "scheme-lib"
               "racket-test"
               "typed-racket-lib"
               "plot"
               "draw-lib"
               "gui-lib"
               "pict-lib"))

(define pkg-desc "Racket benchmarks")
(define pkg-authors '(eli jay mflatt robby samth stamourv))

(define license
  '((Apache-2.0 OR MIT)
    AND
    (Apache-2.0 ; psyntax : tests/racket/benchmarks/common/psyntax.sch (see file for original license)
     AND
     LGPL-3.0-or-later))) ; SCM : tests/racket/benchmarks/shootout/pidigits1.rkt
