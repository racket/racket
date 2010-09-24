#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
          "../../syntax.rkt"
          "../common/event.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "hbitmap.rkt"
         "types.rkt")

(provide item%)

(defclass item% window%
  (inherit get-hwnd)

  (super-new)

  (define/override (gets-focus?) #t)

  (define/public (set-label s)
    (SetWindowTextW (get-hwnd) s))

  (def/public-unimplemented get-label)
  (def/public-unimplemented command))
