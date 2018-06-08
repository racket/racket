#lang racket/base

;; Regression test for GitHub issue #2123
;; <https://github.com/racket/racket/issues/2123>
;;
;; - define a struct (with a mutable field)
;; - inherit the struct in two other modules
;; - import the sub-structs in a new module and use struct-out on one

(module private racket/base
  (struct container (a [b #:mutable]))
  (provide (struct-out container)))

(module sub1 racket/base
  (require (submod ".." private))
  (struct sub-container-1 container (c))
  (provide (struct-out sub-container-1)))

(module sub2 racket/base
  (require (submod ".." private))
  (struct sub-container-2 container (d))
  (provide (struct-out sub-container-2)))

(require 'sub1 'sub2)
(provide (struct-out sub-container-1))
