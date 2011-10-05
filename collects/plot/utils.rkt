#lang racket/base

(require "common/math.rkt")
(provide bounds->intervals
         linear-seq linear-seq*
         degrees->radians
         radians->degrees)

(require "common/format.rkt")
(provide real->string/trunc
         digits-for-range
         ->plot-label)

(require "common/draw.rkt")
(provide color-seq color-seq*
         ->color
         ->pen-color ->brush-color
         ->pen-style ->brush-style)

(require "common/axis-transform.rkt")
(provide (struct-out invertible-function))

(require "common/sample.rkt")
(provide (struct-out mapped-function)
         (struct-out mapped-function/bounds))

(require "plot2d/kde.rkt")
(provide make-kde)
