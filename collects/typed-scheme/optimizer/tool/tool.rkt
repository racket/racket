#lang racket/base

(require racket/unit racket/class racket/port
         racket/gui/base mrlib/switchable-button)

(require (prefix-in tr: typed-scheme/typed-reader)
         typed-scheme/optimizer/logging)

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

;; performance-report-callback : drracket:unit:frame<%> -> void
(define (performance-report-callback drr-frame)
  (define defs     (send drr-frame get-definitions-text)) ; : text%
  (define portname (send defs      get-port-name))
  (message-box
   "Performance Report"
   (with-output-to-string
     (lambda ()
       (with-tr-logging-to-port
        (current-output-port)
        (lambda ()
          (parameterize ([current-namespace  (make-base-namespace)]
                         [read-accept-reader #t])
            (expand
             (tr:read-syntax portname
                             (open-input-string (send defs get-text)))))))))))

(define performance-report-drracket-button
  (list
   "Performance Report"
   reverse-content-bitmap
   performance-report-callback))
