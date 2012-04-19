#lang racket/base
(require racket/class
         racket/draw/private/syntax
         "snip.rkt")

(provide snip-admin%)

;; for contract only
(define fake-popup-menu% object%)

(defclass snip-admin% object%
  (super-new)

  (def/public (get-editor) #f)
  (def/public (get-dc) #f)
  (def/public (get-view-size [maybe-box? w] [maybe-box? h])
    #f)

  (def/public (get-view [maybe-box? x] [maybe-box? y] [maybe-box? w] [maybe-box? h] 
                        [(make-or-false snip%) [snip #f]])
    #f)

  (def/public (scroll-to [snip% s]
                         [real? x] [real? y]
                         [nonnegative-real? w] [nonnegative-real? h]
                         [any? refresh?]
                         [(symbol-in start end none) [bias 'none]])
    #f)

  (def/public (set-caret-owner [snip% s] [(symbol-in imeditorte display global) dist])
    (void))

  (def/public (resized [snip% s] [any? redraw?]) (void))

  (def/public (recounted [snip% s] [any? redraw?]) (void))

  (def/public (needs-update [snip% s] [real? x] [real? y]
                            [nonnegative-real? w] [nonnegative-real? h])
    (void))

  (def/public (release-snip [snip% s]) #f)

  (def/public (update-cursor) (void))

  (def/public (popup-menu [fake-popup-menu% p][snip% snip][real? x][real? y])
    #f)

  (def/public (modified [snip% s] [any? modified?])
    (void))
  
  (def/public (get-line-spacing)
    0.0)
  
  (def/public (get-selected-text-color)
    #f)
  
  (def/public (call-with-busy-cursor [procedure? thunk])
    (void))
  
  (def/public (get-tabs [maybe-box? [length #f]] [maybe-box? [tab-width #f]] [maybe-box? [in-units #f]])
    #f))

