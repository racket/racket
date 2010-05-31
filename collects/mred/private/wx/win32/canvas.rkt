#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide canvas%)

(defclass canvas% window%
  (def/public-unimplemented get-canvas-background)
  (def/public-unimplemented set-canvas-background)
  (def/public-unimplemented set-background-to-gray)
  (def/public-unimplemented on-scroll)
  (def/public-unimplemented set-scroll-page)
  (def/public-unimplemented set-scroll-range)
  (def/public-unimplemented set-scroll-pos)
  (def/public-unimplemented get-scroll-page)
  (def/public-unimplemented get-scroll-range)
  (def/public-unimplemented get-scroll-pos)
  (def/public-unimplemented scroll)
  (def/public-unimplemented warp-pointer)
  (def/public-unimplemented view-start)
  (def/public-unimplemented set-resize-corner)
  (def/public-unimplemented show-scrollbars)
  (def/public-unimplemented set-scrollbars)
  (def/public-unimplemented get-virtual-size)
  (def/public-unimplemented get-dc)
  (def/public-unimplemented on-char)
  (def/public-unimplemented on-event)
  (def/public-unimplemented on-paint)
  (super-new))
