#lang racket/base
(require racket/class
         ffi/unsafe
         racket/draw
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/canvas-mixin.rkt"
         "../common/backing-dc.rkt"
         "../common/event.rkt"
         "../common/freeze.rkt"
         "../common/queue.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt"
         "window.rkt"
         "dc.rkt"
         "item.rkt"
         "theme.rkt")

(provide canvas%)

(define WS_EX_STATICEDGE        #x00020000)
(define WS_EX_CLIENTEDGE        #x00000200)

(define-user32 BeginPaint (_wfun _HWND _pointer -> _HDC))
(define-user32 EndPaint (_wfun _HDC _pointer -> _BOOL))
(define-user32 ShowScrollBar (_wfun _HWND _int _BOOL -> (r : _BOOL)
                                    -> (unless r (failed 'ShowScrollbar))))

(define _HRGN _pointer)
(define-user32 GetDCEx (_wfun _HWND _HRGN _DWORD -> _HDC))
(define DCX_WINDOW           #x00000001)
(define DCX_CACHE            #x00000002)

(define EP_EDITTEXT 1)
(define ETS_NORMAL 1)
(define ETS_DISABLE 4)

(define-cstruct _SCROLLINFO
  ([cbSize _UINT]
   [fMask _UINT]
   [nMin  _int]
   [nMax  _int]
   [nPage _UINT]
   [nPos  _int]
   [nTrackPos _int]))

(define-user32 SetScrollInfo (_wfun _HWND _int _SCROLLINFO-pointer _BOOL -> _int))
(define-user32 GetScrollPos (_wfun _HWND _int -> _int))
(define-user32 SetScrollPos (_wfun _HWND _int _int _BOOL -> _int))
(define-user32 GetScrollInfo (_wfun _HWND _int (i : _SCROLLINFO-pointer
                                                  = (make-SCROLLINFO (ctype-sizeof _SCROLLINFO)
                                                                     (bitwise-ior SIF_RANGE SIF_POS 
                                                                                  SIF_PAGE SIF_TRACKPOS)
                                                                     0 0 0 0 0))
                                    -> (r : _BOOL)
                                    -> (if r i (error 'GetScrollInfo "failed"))))

(define COMBO-WIDTH 18)

(define canvas% 
  (canvas-mixin
   (class (item-mixin window%)
     (init parent
           x y w h
           style
           [ignored-name #f]
           [gl-config #f])

     (inherit get-hwnd
              get-client-size
              get-eventspace
              set-control-font
              subclass-control)

     (define hscroll? (memq 'hscroll style))
     (define vscroll? (memq 'vscroll style))

     (define panel-hwnd
       (and (memq 'combo style)
            (CreateWindowExW 0
                             "PLTTabPanel"
                             #f
                             (bitwise-ior WS_CHILD)
                             0 0 w h
                             (send parent get-client-hwnd)
                             #f
                             hInstance
                             #f)))

     (define canvas-hwnd
       (CreateWindowExW (cond
                         [(memq 'border style) WS_EX_STATICEDGE]
                         [(memq 'control-border style) WS_EX_CLIENTEDGE]
                         [else 0])
                        "PLTCanvas"
                        #f
                        (bitwise-ior WS_CHILD 
                                     (if panel-hwnd WS_VISIBLE 0)
                                     (if hscroll? WS_HSCROLL 0)
                                     (if vscroll? WS_VSCROLL 0))
                        0 0 w h
                        (or panel-hwnd (send parent get-hwnd))
                        #f
                        hInstance
                        #f))
     (define combo-hwnd
       (and panel-hwnd
            (CreateWindowExW 0
                             "PLTCOMBOBOX"
                             ""
                             (bitwise-ior WS_CHILD WS_VISIBLE
                                          CBS_DROPDOWNLIST 
                                          WS_HSCROLL WS_VSCROLL
                                          WS_BORDER WS_CLIPSIBLINGS)
                             0 0 w h
                             panel-hwnd
                             #f
                             hInstance
                             #f)))

     (define hwnd (or panel-hwnd canvas-hwnd))

     (super-new [parent parent]
                [hwnd hwnd]
                [extra-hwnds (if panel-hwnd
                                 (list canvas-hwnd combo-hwnd)
                                 null)]
                [style style])

     (when combo-hwnd
       (set-control-font #f combo-hwnd)
       (subclass-control combo-hwnd))

     (define control-border-theme
       (and (memq 'control-border style)
            (OpenThemeData canvas-hwnd "Edit")))

     (define/override (wndproc w msg wParam lParam default)
       (cond
        [(= msg WM_PAINT)
         (let* ([ps (malloc 128)]
                [hdc (BeginPaint w ps)])
           (unless (positive? paint-suspended)
             (unless (do-backing-flush this dc hdc)
               (queue-paint))
             (do-backing-flush this dc hdc))
           (EndPaint hdc ps))
         0]
        [(= msg WM_NCPAINT)
         (if control-border-theme
             (let* ([r (GetWindowRect canvas-hwnd)]
                    [res (default w msg wParam lParam)]
                    [hdc (GetDCEx canvas-hwnd #f (bitwise-ior DCX_CACHE DCX_WINDOW))]
                    [wr (make-RECT 0 0
                                   (- (RECT-right r) (RECT-left r))
                                   (- (RECT-bottom r) (RECT-top r)))])
               (DrawThemeBackground control-border-theme
                                    hdc
                                    EP_EDITTEXT
                                    ETS_NORMAL ;; or ETS_DISABLED?
                                    wr
                                    #f)
               (ReleaseDC canvas-hwnd hdc)
               1)
             (default w msg wParam lParam))]
        [(= msg WM_HSCROLL)
         (on-scroll-change SB_HORZ (LOWORD wParam))
         0]
        [(= msg WM_VSCROLL)
         (on-scroll-change SB_VERT (LOWORD wParam))
         0]
        [else (super wndproc w msg wParam lParam default)]))
     
     (define/override (wndproc-for-ctlproc w msg wParam lParam default)
       (default w msg wParam lParam))
     
     (define dc (new dc% [canvas this]))
     (send dc start-backing-retained)

     (define/public (get-dc) dc)

     (define/override (on-resized)
       (send dc reset-backing-retained))

     (define/override (get-client-hwnd)
       canvas-hwnd)

     (define/override (set-size x y w h)
       (super set-size x y w h)
       (when panel-hwnd
         (let* ([r (and (or (= w -1) (= h -1))
                        (GetWindowRect hwnd))]
                [w (if (= w -1) (- (RECT-right r) (RECT-left r)) w)]
                [h (if (= h -1) (- (RECT-bottom r) (RECT-top r)) h)])
           (MoveWindow canvas-hwnd 0 0 (max 1 (- w COMBO-WIDTH)) h #t)
           (MoveWindow combo-hwnd 0 0 (max 1 w) (- h 2) #t))))

     ;; The `queue-paint' and `paint-children' methods
     ;; are defined by `canvas-mixin' from ../common/canvas-mixin
     (define/public (queue-paint) (void))
     (define/public (request-canvas-flush-delay)
       (request-flush-delay this))
     (define/public (cancel-canvas-flush-delay req)
       (cancel-flush-delay req))
     (define/public (queue-canvas-refresh-event thunk)
       (queue-window-refresh-event this thunk))

     (define/public (get-flush-window) canvas-hwnd)

     (define/public (begin-refresh-sequence)
       (send dc suspend-flush))
     (define/public (end-refresh-sequence)
       (send dc resume-flush))

     (define/public (on-paint) (void))
     (define/override (refresh) (queue-paint))

     (define/public (queue-backing-flush)
       (InvalidateRect canvas-hwnd #f #f))

     (define/public (make-compatible-bitmap w h)
       (send dc make-backing-bitmap w h))

     (define paint-suspended 0)
     (define/public (suspend-paint-handling)
       (atomically
        (set! paint-suspended (add1 paint-suspended))))
     (define/public (resume-paint-handling)
       (atomically
        (unless (zero? paint-suspended)
          (set! paint-suspended (sub1 paint-suspended)))))

     (define/public (get-virtual-size w h)
       (get-client-size w h))

     (define transparent? (memq 'transparent style))
     (define bg-col (make-object color% "white"))
     (define/public (get-canvas-background) (if transparent?
                                                #f
                                                bg-col))
     (define/public (set-canvas-background col) (set! bg-col col))

     (define h-scroll-visible? hscroll?)
     (define v-scroll-visible? vscroll?)
     (define/public (show-scrollbars h? v?)
       (when hscroll?
         (atomically
          (set! h-scroll-visible? (and h? #t))
          (ShowScrollBar canvas-hwnd SB_HORZ h?)))
       (when vscroll?
         (atomically
          (set! v-scroll-visible? (and v? #t))
          (ShowScrollBar canvas-hwnd SB_VERT v?))))

     (define/public (set-scrollbars h-step v-step
                                    h-len v-len
                                    h-page v-page
                                    h-pos v-pos
                                    auto?)
       (define (make-info len page pos vis?)
         (make-SCROLLINFO (ctype-sizeof _SCROLLINFO)
                          (bitwise-ior (if vis? SIF_DISABLENOSCROLL 0)
                                       SIF_RANGE
                                       SIF_POS
                                       SIF_PAGE)
                          0 (+ len page -1) page pos 0))
       (when hscroll?
         (SetScrollInfo canvas-hwnd SB_HORZ (make-info h-len h-page h-pos h-scroll-visible?) #t))
       (when vscroll?
         (SetScrollInfo canvas-hwnd SB_VERT (make-info v-len v-page v-pos v-scroll-visible?) #t)))

     (def/public-unimplemented set-background-to-gray)

     (define/public (get-scroll-pos which)
       (GetScrollPos canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ)))
     (define/public (get-scroll-range which)
       (let ([i (GetScrollInfo canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ))])
         (+ (- (SCROLLINFO-nMax i)
               (SCROLLINFO-nPage i))
            1)))
     (define/public (get-scroll-page which)
       (let ([i (GetScrollInfo canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ))])
         (SCROLLINFO-nPage i)))

     (define/public (set-scroll-pos which v)
       (void (SetScrollPos canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ) v #t)))
     (define/public (set-scroll-range which v)
       (let ([i (GetScrollInfo canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ))])
         (set-SCROLLINFO-fMask! i (bitwise-ior SIF_RANGE 
                                               (if (if (eq? which 'vertical)
                                                       v-scroll-visible?
                                                       h-scroll-visible?)
                                                   SIF_DISABLENOSCROLL
                                                   0)))
         (set-SCROLLINFO-nMax! i (+ v (SCROLLINFO-nPage i) -1))
         (SetScrollInfo canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ) i #t)))
     (define/public (set-scroll-page which v)
       (let ([i (GetScrollInfo canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ))])
         (set-SCROLLINFO-fMask! i (bitwise-ior SIF_RANGE SIF_PAGE
                                               (if (if (eq? which 'vertical)
                                                       v-scroll-visible?
                                                       h-scroll-visible?)
                                                   SIF_DISABLENOSCROLL
                                                   0)))
         (set-SCROLLINFO-nMax! i (+ (- (SCROLLINFO-nMax i) (SCROLLINFO-nPage i))
                                    v))
         (set-SCROLLINFO-nPage! i v)
         (SetScrollInfo canvas-hwnd (if (eq? which 'vertical) SB_VERT SB_HORZ) i #t)))

     (define/public (on-scroll e) (void))
     (define/private (on-scroll-change dir part)
       (let ([i (GetScrollInfo canvas-hwnd dir)])
         (let ([new-pos
                (cond
                 [(= part SB_TOP) 0]
                 [(= part SB_BOTTOM) (SCROLLINFO-nMax i)]
                 [(= part SB_LINEUP) (max 0 (sub1 (SCROLLINFO-nPos i)))]
                 [(= part SB_LINEDOWN) (min (SCROLLINFO-nMax i) (add1 (SCROLLINFO-nPos i)))]
                 [(= part SB_PAGEUP) (max 0 (- (SCROLLINFO-nPos i) (SCROLLINFO-nPage i)))]
                 [(= part SB_PAGEDOWN) (min (SCROLLINFO-nMax i) (+ (SCROLLINFO-nPos i) (SCROLLINFO-nPage i)))]
                 [(= part SB_THUMBTRACK) (SCROLLINFO-nTrackPos i)]
                 [else (SCROLLINFO-nPos i)])])
           (unless (= new-pos (SCROLLINFO-nPos i))
             (set-SCROLLINFO-nPos! i new-pos)
             (set-SCROLLINFO-fMask! i SIF_POS)
             (SetScrollInfo canvas-hwnd dir i #t)
             (queue-window-event
              this
              (lambda ()
                (on-scroll (new scroll-event%
                                [event-type 'thumb]
                                [direction (if (= dir SB_HORZ) 'horizontal 'vertical)]
                                [position new-pos]))))
             (constrained-reply (get-eventspace)
                                (lambda ()
                                  (let loop () (pre-event-sync #t) (when (yield) (loop))))
                                (void))))))

     (define/override (definitely-wants-event? w e) 
       (or (e . is-a? . key-event%)
           (ptr-equal? w canvas-hwnd)))

     (define/public (on-combo-select i) (void))
     (define/public (set-combo-text s) (void))
     (define/public (append-combo-item s)
       (SendMessageW/str combo-hwnd CB_ADDSTRING 0 s))

     (define/override (is-command? cmd)
       (= cmd CBN_SELENDOK))

     (define/public (do-command control-hwnd)
       (let ([i (SendMessageW combo-hwnd CB_GETCURSEL 0 0)])
         (queue-window-event this (lambda () (on-combo-select i)))))

     (define/override (is-hwnd? a-hwnd)
       (or (ptr-equal? panel-hwnd a-hwnd)
           (ptr-equal? canvas-hwnd a-hwnd)
           (ptr-equal? combo-hwnd a-hwnd)))

     (def/public-unimplemented scroll)
     (def/public-unimplemented warp-pointer)
     (def/public-unimplemented view-start)

     (define/public (set-resize-corner on?)
       (void)))))

