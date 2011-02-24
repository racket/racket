#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
          "../../syntax.rkt"
          "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "types.rkt")

(provide
 (protect-out slider%))

(define TBS_VERT                #x0002)
(define TBS_HORZ                #x0000)

(define TBM_GETPOS              WM_USER)
(define TBM_GETRANGEMIN         (+ WM_USER 1))
(define TBM_GETRANGEMAX         (+ WM_USER 2))
(define TBM_GETTIC              (+ WM_USER 3))
(define TBM_SETTIC              (+ WM_USER 4))
(define TBM_SETPOS              (+ WM_USER 5))
(define TBM_SETRANGE            (+ WM_USER 6))
(define TBM_SETRANGEMIN         (+ WM_USER 7))
(define TBM_SETRANGEMAX         (+ WM_USER 8))

(define SS_CENTER            #x00000001)

(define THICKNESS 24)
(define MIN_LENGTH 80)

(defclass slider% item%
  (init parent cb
        label
        val lo hi
        x y w
        style
        font)
  (inherit set-control-font
           auto-size)

  (define callback cb)
  (define vertical? (memq 'vertical style))

  (define panel-hwnd
    (if (memq 'plain style)
        #f
        (CreateWindowExW 0
                         "PLTPanel"
                         #f
                         (bitwise-ior WS_CHILD)
                         0 0 0 0
                         (send parent get-content-hwnd)
                         #f
                         hInstance
                         #f)))

  (define slider-hwnd
    (CreateWindowExW/control 0
                             "PLTmsctls_trackbar32"
                             label
                             (bitwise-ior WS_CHILD WS_CLIPSIBLINGS
                                          (if vertical?
                                              TBS_VERT
                                              TBS_HORZ)
                                          (if panel-hwnd
                                              WS_VISIBLE
                                              0))
                             0 0 0 0
                             (or panel-hwnd 
                                 (send parent get-content-hwnd))
                             #f
                             hInstance
                             #f))

  (define value-hwnd
    (and panel-hwnd
         (CreateWindowExW/control 0
                                  "STATIC"
                                  (format "~s" val)
                                  (bitwise-ior SS_CENTER WS_CHILD WS_CLIPSIBLINGS WS_VISIBLE)
                                  0 0 0 0
                                  panel-hwnd
                                  #f
                                  hInstance
                                  #f)))

  (define hwnd (or panel-hwnd slider-hwnd))

  (super-new [callback cb]
             [parent parent]
             [hwnd hwnd]
             [extra-hwnds
              (if panel-hwnd
                  (list slider-hwnd value-hwnd)
                  null)]
             [style style])

  (define/override (is-hwnd? a-hwnd)
    (or (ptr-equal? hwnd a-hwnd)
        (and panel-hwnd
             (or (ptr-equal? slider-hwnd a-hwnd)
                 (ptr-equal? value-hwnd a-hwnd)))))
  
  (when value-hwnd
    (set-control-font font value-hwnd))

  (define value-w 0)
  (define value-h 0)

  (if panel-hwnd
      (auto-size font
                 (list (format "~s" lo)
                       (format "~s" hi))
                 0 0 0 0 (lambda (w h)
                           (set! value-w w)
                           (set! value-h h)
                           (if vertical?
                               (set-size -11111 -11111 (+ THICKNESS w) (max h MIN_LENGTH))
                               (set-size -11111 -11111 (max w MIN_LENGTH) (+ THICKNESS h)))))
      (if vertical?
          (set-size -11111 -11111 THICKNESS MIN_LENGTH)
          (set-size -11111 -11111 MIN_LENGTH THICKNESS)))

  (SendMessageW slider-hwnd TBM_SETRANGE 1 (MAKELPARAM lo hi))
  (set-value val)

  (define/override (set-size x y w h)
    (super set-size x y w h)
    (when panel-hwnd
      (unless (or (= w -1) (= h -1))
        (if vertical?
            (let ([dx (quotient (- w THICKNESS value-w) 2)])
              (MoveWindow slider-hwnd dx 0 THICKNESS h #T)
              (MoveWindow value-hwnd (+ dx THICKNESS) (quotient (- h value-h) 2) value-w value-h #t))
            (let ([dy (quotient (- h THICKNESS value-h) 2)])
              (MoveWindow slider-hwnd 0 dy w THICKNESS #t)
              (MoveWindow value-hwnd (quotient (- w value-w) 2) (+ dy THICKNESS) value-w value-h #t))))))

  (define/override (control-scrolled)
    (when value-hwnd
      (set-text (get-value)))
    (queue-window-event this (lambda ()
                               (callback this
                                         (new control-event%
                                              [event-type 'slider]
                                              [time-stamp (current-milliseconds)])))))

  (define/public (set-value val)
    (SendMessageW slider-hwnd TBM_SETPOS 1 val)
    (when value-hwnd
      (set-text val)))
  
  (define/private (set-text val)
    (SetWindowTextW value-hwnd (format "~s" val)))
    
  (define/public (get-value)
    (SendMessageW slider-hwnd TBM_GETPOS 0 0)))
