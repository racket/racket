#lang racket/base
(require racket/class
         racket/draw
         (only-in racket/list take drop)
         ffi/unsafe
          "../../syntax.rkt"
          "../../lock.rkt"
          "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "types.rkt")

(provide list-box%)

(define WS_EX_CLIENTEDGE        #x00000200)

(define LBS_NOTIFY            #x0001)
(define LBS_MULTIPLESEL       #x0008)
(define LBS_HASSTRINGS        #x0040)
(define LBS_MULTICOLUMN       #x0200)
(define LBS_WANTKEYBOARDINPUT #x0400)
(define LBS_EXTENDEDSEL       #x0800)
(define LBS_DISABLENOSCROLL   #x1000)

(define LBN_SELCHANGE       1)
(define LBN_DBLCLK          2)

(define LB_ERR -1)

(define LB_ADDSTRING          #x0180)
(define LB_RESETCONTENT       #x0184)
(define LB_INSERTSTRING       #x0181)
(define LB_DELETESTRING       #x0182)
(define LB_GETTOPINDEX        #x018E)
(define LB_SETTOPINDEX        #x0197)
(define LB_GETITEMHEIGHT      #x01A1)
(define LB_GETSELCOUNT        #x0190)
(define LB_GETSELITEMS        #x0191)
(define LB_GETCURSEL          #x0188)
(define LB_SETSEL             #x0185)
(define LB_SETCURSEL          #x0186)
(define LB_GETSEL             #x0187)
(define LB_SELITEMRANGE       #x019B)

(define list-box%
  (class item%
    (init parent cb
          label kind x y w h
          choices style
          font label-font)

    (inherit set-size set-control-font
             subclass-control
             get-client-size)

    (define single?
      (and (not (memq 'extended style))
           (not (memq 'mutiple style))))

    (define hwnd
      (CreateWindowExW WS_EX_CLIENTEDGE
                       "PLTLISTBOX"
                       label
                       (bitwise-ior WS_CHILD WS_CLIPSIBLINGS LBS_NOTIFY
                                    WS_VSCROLL
                                    (if (memq 'hscroll style) WS_HSCROLL 0)
                                    (cond
                                     ;; Win32 sense of "multiple" and "extended" is backwards
                                     [(eq? kind 'extended) LBS_MULTIPLESEL]
                                     [(eq? kind 'multiple) LBS_EXTENDEDSEL]
                                     [else 0]))
                       0 0 0 0
                       (send parent get-client-hwnd)
                       #f
                       hInstance
                       #f))

    (for ([s (in-list choices)])
      (SendMessageW/str hwnd LB_ADDSTRING 0 s))

    (super-new [callback cb]
               [parent parent]
               [hwnd hwnd]
               [style style])

    (set-control-font font)
    (set-size -11111 -11111 40 60)

    (subclass-control hwnd)

    (define callback cb)

    (define/override (is-command? cmd)
      (or (= cmd LBN_SELCHANGE)
          (= cmd LBN_DBLCLK)))
    
    (define/public (do-command cmd control-hwnd)
      (queue-window-event this (lambda ()
                                 (callback this
                                           (new control-event%
                                                [event-type (if (= cmd LBN_SELCHANGE)
                                                                'list-box
                                                                'list-box-dclick)]
                                                [time-stamp (current-milliseconds)])))))


    (define num (length choices))
    (define/public (number) num)

    (define data (map (lambda (x) (box #f)) choices))
    (define/public (get-data i) (unbox (list-ref data i)))
    (define/public (set-data i v) (set-box! (list-ref data i) v))

    (define/public (set-string i str)
      (atomically
       (SendMessageW/str hwnd LB_INSERTSTRING i str)
       (SendMessageW hwnd LB_DELETESTRING (add1 i) 0)))

    (define/public (set-first-visible-item i)
      (SendMessageW hwnd LB_SETTOPINDEX i 0))

    (define/public (get-first-item)
      (SendMessageW hwnd LB_GETTOPINDEX 0 0))

    (define/public (number-of-visible-items)
      (let ([ih (SendMessageW hwnd LB_GETITEMHEIGHT 0 0)])
        (let ([w (box 0)]
              [h (box 0)])
          (get-client-size w h)
          (quotient (unbox h) ih))))

    (define/public (clear)
      (atomically
       (set! data null)
       (set! num 0)
       (SendMessageW hwnd LB_RESETCONTENT 0 0)))

    (define/public (set choices)
      (atomically
       (ShowWindow hwnd SW_HIDE)
       (clear)
       (for ([s (in-list choices)])
         (SendMessageW/str hwnd LB_ADDSTRING 0 s))
       (set! data (map (lambda (s) (box #f)) choices))
       (set! num (length choices))
       (ShowWindow hwnd SW_SHOW)))

    (public [append* append])
    (define (append* s [v #f])
      (atomically
       (SendMessageW/str hwnd LB_ADDSTRING 0 s)
       (set! num (add1 num))
       (set! data (append data (list (box v))))))

    (define/public (delete i)
      (atomically
       (set! data (append (take data i) (drop data (add1 i))))
       (set! num (sub1 num))
       (SendMessageW hwnd LB_DELETESTRING i 0)))

    (define/public (get-selections)
      (atomically
       (if single?
           (let ([v (SendMessageW hwnd LB_GETCURSEL 0 0)])
             (if (= v LB_ERR)
                 null
                 (list v)))
           (let ([n (SendMessageW hwnd LB_GETSELCOUNT 0 0)])
             (if (zero? n)
                 null
                 (let ([selections (malloc n _LONG 'raw)])
                   (SendMessageW hwnd LB_GETSELITEMS n (cast selections _pointer _LPARAM))
                   (begin0
                    (for/list ([i (in-range n)])
                      (ptr-ref selections _LONG i))
                    (free selections))))))))
      
    (define/public (get-selection)
      (let ([l (get-selections)])
        (if (null? l)
            -1
            (car l))))

    (define/public (selected? i)
      (not (zero? (SendMessageW hwnd LB_GETSEL i 0))))

    (define/public (select i [on? #t] [extend? #t])
      (if single?
          (SendMessageW hwnd LB_SETCURSEL (if on? i -1) 0)
          (begin
            (when extend?
              (SendMessageW hwnd LB_SELITEMRANGE 0 (MAKELPARAM 0 num)))
            (SendMessageW hwnd LB_SETSEL (if on? 1 0) i))))

    (define/public (set-selection i)
      (select i #t #f))

    (def/public-unimplemented get-label-font)))
