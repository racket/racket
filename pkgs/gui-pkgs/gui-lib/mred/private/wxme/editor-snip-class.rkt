#lang racket/base

(require racket/class
         racket/file file/convertible
         "../syntax.rkt"
         racket/snip/private/snip-flags
         "private.rkt"
         racket/snip/private/private
         racket/snip/private/snip
         (only-in "cycle.rkt"
                  editor-stream-in% editor-stream-out%
                  extended-text% extended-pasteboard% extended-editor-snip% 
                  get-editor-data-class)
         "../wx/common/event.rkt"
         racket/draw)

(provide the-editor-snip-class)
;; ------------------------------------------------------------

(defclass editor-snip-class% snip-class%
  (inherit set-classname
           set-version)
  (inherit-field s-required?)

  (super-new)
  
  (set-classname "wxmedia")
  (set-version 4)
  (set! s-required? #t)

  (def/override (read [editor-stream-in% f])
    (let ([vers (send f do-reading-version this)])
      (let ([ed% (case (send f get-exact)
                   [(1) extended-text%]
                   [(2) extended-pasteboard%]
                   [else #f])]
            [border? (positive? (send f get-exact))]
            [lm (max 0 (send f get-exact))]
            [tm (max 0 (send f get-exact))]
            [rm (max 0 (send f get-exact))]
            [bm (max 0 (send f get-exact))]
            [li (max 0 (send f get-exact))]
            [ti (max 0 (send f get-exact))]
            [ri (max 0 (send f get-exact))]
            [bi (max 0 (send f get-exact))]
            [min-w (send f get-inexact)]
            [max-w (send f get-inexact)]
            [min-h (send f get-inexact)]
            [max-h (send f get-inexact)]
            [tf? (and (vers . > . 1)
                      (positive? (send f get-exact)))]
            [atl? (and (vers . > . 2)
                       (positive? (send f get-exact)))]
            [ubs? (and (vers . > . 3)
                       (positive? (send f get-exact)))])
        (let ([e (and ed% (new ed%))])
          (let ([snip (make-object extended-editor-snip%
                                   e
                                   border?
                                   lm tm rm bm li ti ri bi
                                   (if (negative? min-w) 'none min-w)
                                   (if (negative? max-w) 'none max-w)
                                   (if (negative? min-h) 'none min-h)
                                   (if (negative? max-h) 'none max-h))])
            (send snip do-set-graphics tf? atl? ubs?)
            (if e
                (begin
                  (send e get-style-list)
                  (send e read-from-file f #t))
                (send snip set-editor #f))
            snip))))))

(define the-editor-snip-class (new editor-snip-class%))

(send (get-the-snip-class-list) add the-editor-snip-class)
