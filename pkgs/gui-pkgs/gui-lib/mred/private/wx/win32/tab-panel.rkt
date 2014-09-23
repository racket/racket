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

(provide
 (protect-out tab-panel%))

(define TCIF_TEXT            #x0001)
(define TCM_SETUNICODEFORMAT #x2005)
(define TCM_FIRST            #x1300)
(define TCM_INSERTITEMW  (+ TCM_FIRST 62))
(define TCM_SETITEMW     (+ TCM_FIRST 61))
(define TCM_SETCURSEL    (+ TCM_FIRST 12))
(define TCM_GETCURSEL    (+ TCM_FIRST 11))
(define TCM_GETITEMCOUNT (+ TCM_FIRST 4))
(define TCM_DELETEITEM   (+ TCM_FIRST 8))
(define TCM_DELETEALLITEMS (+ TCM_FIRST 9))
(define TCM_GETCURFOCUS (+ TCM_FIRST 47))
(define TCM_SETCURFOCUS (+ TCM_FIRST 48))

(define-cstruct _TCITEMW
  ([mask _UINT]
   [dwState _DWORD]
   [dwStateMask _DWORD]
   [pszText _permanent-string/utf-16]
   [cchTextMax _int]
   [iImage _int]
   [lParam _LPARAM]))

(define tab-panel% 
  (class (item-mixin (panel-mixin window%))
    (init parent
          x y w h
          style
          choices)

    (define callback void)

    (inherit auto-size set-control-font
             is-shown-to-root?)

    (define hwnd
      (CreateWindowExW/control 0
                               "PLTSysTabControl32"
                               ""
                               (bitwise-ior WS_CHILD WS_CLIPSIBLINGS)
                               0 0 0 0
                               (send parent get-content-hwnd)
                               #f
                               hInstance
                               #f))

    (define client-hwnd
      (CreateWindowExW 0
                       "PLTTabPanel"
                       #f
                       (bitwise-ior WS_CHILD WS_VISIBLE)
                       0 0 (->screen w) (->screen h)
                       hwnd
                       #f
                       hInstance
                       #f))

    (super-new [callback (lambda (c) (callback c))]
               [extra-hwnds (list client-hwnd)]
               [parent parent]
               [hwnd hwnd]
               [style style])

    (define/override (get-client-hwnd)
      client-hwnd)

    (SendMessageW hwnd TCM_SETUNICODEFORMAT 1 0)

    (define/private (with-item proc)
      (atomically
       (let ([item (cast (malloc _TCITEMW 'raw) _pointer _TCITEMW-pointer)])
         (set-TCITEMW-mask! item TCIF_TEXT)
         (proc item 
               (lambda () (free (TCITEMW-pszText item)))
               (lambda (msg w)
                 (SendMessageW hwnd msg w (cast item _pointer _LPARAM))))
         (free item))))

    (set choices)

    (define tab-height 0)

    (set-control-font #f)
    (auto-size #f
               (if (null? choices)
                   '("Choice")
                   choices)
               0 0 0 0 #:combine-width +
               (lambda (w h)
                 (set! tab-height (+ h 6))
                 (set-size #f #f
                           (+ w (* 6 (length choices))) 
                           (+ h 12))))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (unless (or (= w -1) (= h -1))
        (MoveWindow client-hwnd (->screen 1) (->screen (+ tab-height 2))
		    (->screen (- w 4)) (->screen (- h tab-height 6))
		    #t)))

    (define/override (is-command? cmd)
      (= cmd -551))

    (define/override (do-command cmd control-hwnd)
      (queue-window-event this (lambda ()
                                 (callback this
                                           (new control-event%
                                                [event-type 'tab-panel]
                                                [time-stamp (current-milliseconds)])))))

    ;; Needed after some actions:
    (define/private (refresh)
      (InvalidateRect hwnd #f #f))

    (define/public (set-label pos str)
      (with-item
       (lambda (item done-str send-msg)
         (set-TCITEMW-pszText! item str)
         (send-msg TCM_SETITEMW pos)
         (done-str)))
      (refresh))

    (define/public (set-selection pos)
      (SendMessageW hwnd TCM_SETCURSEL pos 0)
      (refresh))

    (define/public (get-selection)
      (SendMessageW hwnd TCM_GETCURSEL 0 0))

    (define/public (number)
      (SendMessageW hwnd TCM_GETITEMCOUNT 0 0))

    (define/public (delete pos)
      (SendMessageW hwnd TCM_DELETEITEM pos 0)
      (refresh))

    (public [append* append])
    (define (append* str)
      (with-item
       (lambda (item done-str send-msg)
         (set-TCITEMW-pszText! item str)
         (send-msg TCM_INSERTITEMW (number))
         (done-str)))
      (refresh))

    (define/public (set choices)
      (let ([sel (get-selection)])
        (SendMessageW hwnd TCM_DELETEALLITEMS 0 0)
        (with-item
         (lambda (item done-str send-msg)
           (for ([str (in-list choices)]
                 [pos (in-naturals)])
             (set-TCITEMW-pszText! item str)
             (send-msg TCM_INSERTITEMW pos)
             (done-str))))
        (let ([sel (max 0 (min (length choices) sel))])
          (set-selection sel))))

    (define/override (gets-focus?) #t)
    
    (define/public (button-focus i)
      (if (= i -1)
	  (SendMessageW hwnd TCM_GETCURFOCUS 0 0)
	  (SendMessageW hwnd TCM_SETCURFOCUS i 0)))

    (define/public (set-callback cb)
      (set! callback cb))))

