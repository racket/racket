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

    (inherit auto-size set-control-font
             is-enabled-to-root?
             subclass-control
             set-focus)

    (define callback cb)
    (define current-value val)

    (define hwnd
      (CreateWindowExW 0
                       "PLTTabPanel"
                       #f
                       (bitwise-ior WS_CHILD)
                       0 0 w h
                       (send parent get-client-hwnd)
                       #f
                       hInstance
                       #f))

    (define label-bitmaps null)

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
                    (CreateWindowExW 0
                                     "PLTBUTTON"
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
                (let ([hbitmap (bitmap->hbitmap label)])
                  (set! label-bitmaps (cons hbitmap label-bitmaps))
                  (SendMessageW radio-hwnd BM_SETIMAGE IMAGE_BITMAP 
                                (cast hbitmap _HBITMAP _LPARAM))))
              (ShowWindow radio-hwnd SW_SHOW)
              (set-control-font font radio-hwnd)
              (let-values ([(w1 h) 
                            (auto-size label 0 0 20 4 (lambda (w h)
                                                        (MoveWindow radio-hwnd 0 (+ y SEP) w h #t)
                                                        (values w h)))])
                (cons radio-hwnd
                      (loop (+ y SEP h) (max w1 w) (cdr labels))))))))

    (unless (= val -1)
      (SendMessageW (list-ref radio-hwnds val) BM_SETCHECK 1 0))
    
    (super-new [callback cb]
               [parent parent]
               [hwnd hwnd]
               [extra-hwnds radio-hwnds]
               [style style])
    
    (for ([radio-hwnd (in-list radio-hwnds)])
      (subclass-control radio-hwnd))

    (define/override (is-hwnd? a-hwnd)
      (or (ptr-equal? hwnd a-hwnd)
          (for/or ([radio-hwnd (in-list radio-hwnds)])
            (ptr-equal? a-hwnd radio-hwnd))))

    (define/override (is-command? cmd)
      (= cmd BN_CLICKED))
    
    (define/public (do-command cmd control-hwnd)
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


    (define focused 0)

    (define/public (button-focus i)
      (if (= i -1)
          (min focused (length radio-hwnds))
          (begin
            (set! focused i)
            (set-focus (list-ref radio-hwnds i)))))

    (define/override (get-focus-hwnd)
      (if (= focused -1)
          hwnd
          (list-ref radio-hwnds focused)))

    (define/public (set-selection val)
      (atomically
       (unless (= val current-value)
         (unless (= current-value -1)
           (SendMessageW (list-ref radio-hwnds current-value) BM_SETCHECK 0 0))
         (unless (= val -1)
           (SendMessageW (list-ref radio-hwnds val) BM_SETCHECK 1 0))
         (set! current-value val))))

    (define buttons-enabled (make-vector (length radio-hwnds) #t))
    (define/public (enable-button i on?)
      (unless (eq? (and on? #t) (vector-ref buttons-enabled i))
        (vector-set! buttons-enabled i (and on? #t))
        (when (is-enabled-to-root?)
          (void (EnableWindow (list-ref radio-hwnds i) on?)))))
    (define/override (internal-enable on?)
      (for ([radio-hwnd (in-list radio-hwnds)]
            [radio-on? (in-vector buttons-enabled)])
        (void (EnableWindow radio-hwnd (and on? radio-on?)))))

    (define/public (get-selection) current-value)

    (define/public (number) (length radio-hwnds))))

