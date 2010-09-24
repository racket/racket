#lang scheme/base
(require racket/class
         racket/draw
         ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "hbitmap.rkt"
         "types.rkt")

(provide radio-box%)

(define SEP 4)
(define BM_SETCHECK #x00F1)

(define radio-box% 
  (class item%
    (init parent cb label
          x y w h
          labels
          val
          style
          font)

    (inherit auto-size set-control-font)

    (define callback cb)
    (define current-value val)

    (define hwnd
      (CreateWindowExW 0
                       "PLTPanel"
                       #f
                       (bitwise-ior WS_CHILD)
                       0 0 w h
                       (send parent get-client-hwnd)
                       #f
                       hInstance
                       #f))

    (define radio-hwnds
      (let loop ([y 0] [w 0] [labels labels])
        (if (null? labels)
            (begin
              (MoveWindow hwnd 0 0 w y #t)
              null)
            (let* ([label (car labels)]
                   [bitmap? (and (label . is-a? . bitmap%)
                                 (send label ok?))]
                   [radio-hwnd 
                    (CreateWindowExW 0 "BUTTON"
                                     (if (string? label)
                                         label
                                         "<image>")
                                     (bitwise-ior BS_RADIOBUTTON WS_CHILD WS_CLIPSIBLINGS
                                                  (if bitmap?
                                                      BS_BITMAP
                                                      0))
                                     0 0 0 0
                                     hwnd
                                     #f
                                     hInstance
                                     #f)])
              (when bitmap?
                (SendMessageW radio-hwnd BM_SETIMAGE IMAGE_BITMAP 
                              (cast (bitmap->hbitmap label) _HBITMAP _LPARAM)))
              (ShowWindow radio-hwnd SW_SHOW)
              (set-control-font font radio-hwnd)
              (let-values ([(w h) 
                            (auto-size label 0 0 20 4 (lambda (w h)
                                                        (MoveWindow radio-hwnd 0 (+ y SEP) w h #t)
                                                        (values w h)))])
                (cons radio-hwnd
                      (loop (+ y SEP h) (max w h) (cdr labels))))))))

    (unless (= val -1)
      (SendMessageW (list-ref radio-hwnds val) BM_SETCHECK 1 0))
    
    (super-new [parent parent]
               [hwnd hwnd]
               [extra-hwnds radio-hwnds]
               [style style])

    (define/override (is-hwnd? a-hwnd)
      (or (ptr-equal? hwnd a-hwnd)
          (for/or ([radio-hwnd (in-list radio-hwnds)])
            (ptr-equal? a-hwnd radio-hwnd))))

    (define/override (is-command? cmd)
      (= cmd BN_CLICKED))
    
    (define/public (do-command control-hwnd)
      (let ([val (for/fold ([i 0]) ([radio-hwnd (in-list radio-hwnds)]
                                    [pos (in-naturals)])
                   (if (ptr-equal? control-hwnd radio-hwnd)
                       pos
                       i))])
        (unless (= val current-value)
          (set-selection val)
          (queue-window-event this (lambda ()
                                     (callback this
                                               (new control-event%
                                                    [event-type 'radio-box]
                                                    [time-stamp (current-milliseconds)])))))))


    (def/public-unimplemented button-focus)

    (define/public (set-selection val)
      (atomically
       (unless (= val current-value)
         (unless (= current-value -1)
           (SendMessageW (list-ref radio-hwnds current-value) BM_SETCHECK 0 0))
         (unless (= val -1)
           (SendMessageW (list-ref radio-hwnds val) BM_SETCHECK 1 0))
         (set! current-value val))))

    (define/public (get-selection) current-value)

    (define/public (number) (length radio-hwnds))))

