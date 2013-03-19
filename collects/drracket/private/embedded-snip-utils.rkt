#lang racket/base
(require racket/class
         racket/gui/base)

(provide get-enclosing-editor-frame)

;; get-enclosing-editor-frame: editor<%> -> (or/c frame% #f)
;; Returns the enclosing frame of an-editor, or #f if we can't find it.
(define (get-enclosing-editor-frame an-editor)
  (define (topwin)
    (let ([canvas (send an-editor get-canvas)])
      (and canvas (send canvas get-top-level-window))))
  (let ([admin (send an-editor get-admin)])
    (if (and admin (is-a? admin editor-snip-editor-admin<%>))
        (let ([enclosing-editor-snip (send admin get-snip)])
          (if (get-snip-outer-editor enclosing-editor-snip)
              (get-enclosing-editor-frame (get-snip-outer-editor
                                           enclosing-editor-snip))
              (topwin)))
        (topwin))))

;; get-snip-outer-editor: snip% -> (or/c editor<%> #f)
;; Returns the immediate outer editor enclosing the snip, or false if we
;; can't find it.
(define (get-snip-outer-editor a-snip)
  (let ([admin (send a-snip get-admin)])
    (and admin (send admin get-editor))))
