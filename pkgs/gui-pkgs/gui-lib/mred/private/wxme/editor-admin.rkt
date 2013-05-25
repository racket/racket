#lang racket/base
(require racket/class
         "../syntax.rkt"
         "private.rkt"
         racket/snip/private/private
         (only-in "cycle.rkt" popup-menu%))

(provide editor-admin%)

(defclass editor-admin% object%
  (super-new)

  (define standard 0) ; used to recognize standard display
  (define/public (get-s-standard) standard)
  (define/public (set-s-standard v) (set! standard v))
  
  (def/public (get-dc [maybe-box? [x #f]] [maybe-box? [y #f]])
    (when x (set-box! x 0.0))
    (when y (set-box! y 0.0))
    #f)

  (define/private (do-get-view x y w h)
    (when x (set-box! x 0.0))
    (when y (set-box! y 0.0))
    (when w (set-box! w 0.0))
    (when h (set-box! h 0.0)))

  (def/public (get-view [maybe-box? x] [maybe-box? y]
                        [maybe-box? w] [maybe-box? h]
                        [any? [full? #f]])
    (do-get-view x y w h))

  (def/public (get-max-view [maybe-box? x] [maybe-box? y]
                            [maybe-box? w] [maybe-box? h]
                            [any? [full? #f]])
    (get-view x y w h full?))

  (def/public (scroll-to [real? localx] [real? localy] [real? w] [real? h] [any? [refresh? #t]] 
                         [(symbol-in start none end) [bias 'none]])
    (void))

  (def/public (grab-caret [(symbol-in immediate display global) [dist 'global]])
    (void))

  (def/public (resized [any? redraw-now]) (void))
  
  (def/public (needs-update [real? x] [real? y]
                            [nonnegative-real? w] [nonnegative-real? h])
    (void))

  (def/public (update-cursor) (void))

  (def/public (refresh-delayed?) #f)

  (def/public (popup-menu [popup-menu% m] [real? x] [real? y]) #f)

  (def/public (modified [any? mod?]) (void)))
