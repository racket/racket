#lang racket/base

(require racket/unit racket/class
         racket/gui/base mrlib/switchable-button)

(provide performance-report-drracket-button)

;; DrRacket tool for reporting missed optimizations in the editor.

(define reverse-content-bitmap
  (let* ((bmp (make-bitmap 16 16))
         (bdc (make-object bitmap-dc% bmp)))
    (send bdc erase)
    (send bdc set-smoothing 'smoothed)
    (send bdc set-pen "black" 1 'transparent)
    (send bdc set-brush "blue" 'solid)
    (send bdc draw-ellipse 2 2 8 8)
    (send bdc set-brush "red" 'solid)
    (send bdc draw-ellipse 6 6 8 8)
    (send bdc set-bitmap #f)
    bmp))

(define performance-report-drracket-button
  (list
   "Performance Report"
   reverse-content-bitmap
   (λ (drs-frame)
      (message-box "Performance Report"
                   "Coming Soon!"))))
