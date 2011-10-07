#lang racket/base

(require "common/math.rkt")
(provide bounds->intervals
         linear-seq linear-seq*
         degrees->radians
         radians->degrees)

(require "common/format.rkt")
(provide digits-for-range
         real->plot-label
         ->plot-label
         real->string/trunc)

(require "common/draw.rkt")
(provide color-seq color-seq*
         ->color
         ->pen-color ->brush-color
         ->pen-style ->brush-style)

(require "common/axis-transform.rkt")
(provide (struct-out invertible-function))

(require "common/sample.rkt")
(provide nonlinear-seq
         (struct-out mapped-function))

(require "plot2d/kde.rkt")
(provide kde)
