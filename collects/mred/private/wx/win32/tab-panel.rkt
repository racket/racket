#lang racket/base
(require racket/class
         ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "panel.rkt"
	 "wndclass.rkt"
         "types.rkt")

(provide tab-panel%)

(define TCIF_TEXT            #x0001)
(define TCM_SETUNICODEFORMAT #x2005)
(define TCM_FIRST            #x1300)
(define TCM_INSERTITEMW  (+ TCM_FIRST 62))

(define-cstruct _TCITEMW
  ([mask _UINT]
   [dwState _DWORD]
   [dwStateMask _DWORD]
   [pszText _permanent-string/utf-16]
   [cchTextMax _int]
   [iImage _int]
   [lParam _LPARAM]))

(define tab-panel% 
  (class (panel-mixin window%)
    (init parent
          x y w h
          style
          choices)

    (define callback void)

    (inherit auto-size set-control-font)

    (define hwnd
      (CreateWindowExW 0
                       "SysTabControl32"
                       ""
                       (bitwise-ior WS_CHILD WS_CLIPSIBLINGS)
                       0 0 0 0
                       (send parent get-client-hwnd)
                       #f
                       hInstance
                       #f))

    (define client-hwnd
      (CreateWindowExW 0
                       "PLTTabPanel"
                       #f
                       (bitwise-ior WS_CHILD WS_VISIBLE)
                       0 0 w h
                       hwnd
                       #f
                       hInstance
                       #f))

    (super-new [parent parent]
               [hwnd hwnd]
               [style style])

    (define/override (get-client-hwnd)
      client-hwnd)

    (SendMessageW hwnd TCM_SETUNICODEFORMAT 1 0)

    (atomically
     (let ([item (cast (malloc _TCITEMW 'raw) _pointer _TCITEMW-pointer)])
       (set-TCITEMW-mask! item TCIF_TEXT)
       (for ([i (in-list choices)]
             [pos (in-naturals)])
         (set-TCITEMW-pszText! item i)
         (SendMessageW hwnd TCM_INSERTITEMW pos (cast item _pointer _LPARAM))
         (free (TCITEMW-pszText item)))
       (free item)))

    (define tab-height 0)

    (set-control-font #f)
    (auto-size choices 0 0 0 0 #:combine-width +
               (lambda (w h)
                 (set! tab-height (+ h 6))
                 (set-size -11111 -11111 
                           (+ w (* 6 (length choices))) 
                           (+ h 12))))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (unless (or (= w -1) (= h -1))
        (MoveWindow client-hwnd 1 (+ tab-height 2) (- w 4) (- h tab-height 6) #t)))

    (define/public (set-callback cb)
      (set! callback cb))))

