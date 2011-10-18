#lang racket/base

(require "common/math.rkt")
(require "common/vector.rkt")
(provide bounds->intervals
         linear-seq linear-seq*
         degrees->radians
         radians->degrees
         empty-ivl unknown-ivl ivl-empty? ivl-known? ivl-regular? ivl-meet ivl-join
         empty-rect unknown-rect rect-empty? rect-known? rect-regular? rect-meet rect-join)

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
