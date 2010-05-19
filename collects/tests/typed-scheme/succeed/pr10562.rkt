#lang typed/racket
(: foo : (Rec this (List Number (Boxof (U #f this)))))
(define foo (list 1 (box #f)))
(set-box! (second foo) foo)
