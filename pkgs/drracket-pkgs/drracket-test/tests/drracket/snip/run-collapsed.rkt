#lang racket/base
(require racket/gui/base)

;; Using `racket/gui/base` installs the right load handler:
(dynamic-require "collapsed.rkt" 0)
