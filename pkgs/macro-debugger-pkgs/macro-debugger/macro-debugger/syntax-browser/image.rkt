#lang racket/base
(require racket/contract/base
         racket/class
         racket/gui/base
         framework
         "prefs.rkt"
         "controller.rkt"
         "display.rkt"
         "text.rkt")

#|

Code for generating images that look like the contents of a syntax
browser, with the same pretty-printing, mark-based coloring,
suffixing, etc.

TODO: tacked arrows

|#

(provide/contract
 [print-syntax-columns
  (parameter/c (or/c exact-positive-integer? 'infinity))]
 [print-syntax-to-png
  (->* (syntax? path-string?)
       (#:columns (or/c exact-positive-integer? 'infinity))
       any)]
 [print-syntax-to-bitmap
  (->* (syntax?)
       (#:columns (or/c exact-positive-integer? 'infinity))
       (is-a?/c bitmap%))]
 [print-syntax-to-eps
  (->* (syntax? path-string?)
       (#:columns (or/c exact-positive-integer? 'infinity))
       any)])

;; print-syntax-columns : (parameter-of (U number 'infinity))
(define print-syntax-columns (make-parameter 40))

;; print-syntax-to-png : syntax path -> void
(define (print-syntax-to-png stx file
                             #:columns [columns (print-syntax-columns)])
  (let ([bmp (print-syntax-to-bitmap stx #:columns columns)])
    (send bmp save-file file 'png))
  (void))

;; print-syntax-to-bitmap : syntax -> (is-a?/c bitmap%)
(define (print-syntax-to-bitmap stx
                                #:columns [columns (print-syntax-columns)])
  (define t (prepare-editor stx columns))
  (define admin (new dummy-admin%))
  (send t set-admin admin)
  (define dc (new bitmap-dc% (bitmap (make-object bitmap% 1 1))))
  (define char-width
    (let* ([sl (send t get-style-list)]
           [style (send sl find-named-style (editor:get-default-color-style-name))]
           [font (send style get-font)])
      (send dc set-font font)
      (send dc get-char-width)))
  (let ([ew (box 0.0)] 
        [eh (box 0.0)])
    (send t set-min-width (* columns char-width))
    (send t get-extent ew eh)
    (let* ([w (inexact->exact (unbox ew))]
           [h (inexact->exact (unbox eh))]
           [bmp (make-object bitmap% w (+ 1 h))]
           [ps (new ps-setup%)])
      (send dc set-bitmap bmp)
      (send dc set-background (make-object color% "White"))
      (send dc clear)
      (send ps set-margin 0 0)
      (send ps set-editor-margin 0 0)
      (parameterize ((current-ps-setup ps))
        (send t print-to-dc dc 1))
      bmp)))

;; print-syntax-to-eps : syntax path -> void
(define (print-syntax-to-eps stx file
                             #:columns [columns (print-syntax-columns)])
  (define t (prepare-editor stx columns))
  (define ps-setup (new ps-setup%))
  (send ps-setup set-mode 'file)
  (send ps-setup set-file file)
  (send ps-setup set-scaling 1 1)
  (parameterize ((current-ps-setup ps-setup))
    (send t print #f #f 'postscript #f #f #t)))

(define (prepare-editor stx columns)
  (define t (new browser-text%))
  (define sl (send t get-style-list))
  (send t change-style (send sl find-named-style (editor:get-default-color-style-name)))
  (print-syntax-to-editor stx t 
                          (new controller%) (new syntax-prefs/readonly%)
                          columns (send t last-position))
  t)

;; dummy editor-admin
(define dummy-admin%
  (class editor-admin%
    (define the-dc (new bitmap-dc% (bitmap (make-object bitmap% 1 1))))
    (define/override (get-dc [x #f] [y #f])
      (when x (set-box! x 0.0))
      (when y (set-box! y 0.0))
      the-dc)
    (super-new)))
