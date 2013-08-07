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

(provide
 (protect-out list-box%))

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

(define LVCF_WIDTH              #x0002)
(define LVCF_TEXT               #x0004)
(define LVCF_MINWIDTH           #x0040)

(define LVS_REPORT              #x0001)
(define LVS_SINGLESEL           #x0004)
(define LVS_NOCOLUMNHEADER      #x4000)
(define LVS_SHOWSELALWAYS       #x0008)

(define LVS_EX_HEADERDRAGDROP   #x00000010)
(define LVS_EX_FULLROWSELECT    #x00000020)

(define LVIF_TEXT               #x0001)
(define LVIF_DI_SETITEM         #x1000)

(define LVM_FIRST               #x1000)
(define LVM_INSERTITEMW         (+ LVM_FIRST 77))
(define LVM_GETSTRINGWIDTHW     (+ LVM_FIRST 87))
(define LVM_INSERTCOLUMNW       (+ LVM_FIRST 97))
(define LVM_DELETECOLUMN        (+ LVM_FIRST 28))
(define LVM_SETCOLUMNW          (+ LVM_FIRST 96))
(define LVM_GETCOLUMNW          (+ LVM_FIRST 95))
(define LVM_SETITEMTEXTW        (+ LVM_FIRST 116))
(define LVM_DELETEALLITEMS      (+ LVM_FIRST 9))
(define LVM_GETTOPINDEX         (+ LVM_FIRST 39))
(define LVM_GETCOUNTPERPAGE     (+ LVM_FIRST 40))
(define LVM_ENSUREVISIBLE       (+ LVM_FIRST 19))
(define LVM_DELETEITEM          (+ LVM_FIRST 8))
(define LVM_GETSELECTEDCOUNT    (+ LVM_FIRST 50))
(define LVM_GETNEXTITEM         (+ LVM_FIRST 12))
(define LVM_GETITEMSTATE        (+ LVM_FIRST 44))
(define LVM_SETITEMSTATE        (+ LVM_FIRST 43))
(define LVM_SETEXTENDEDLISTVIEWSTYLE (+ LVM_FIRST 54))
(define LVM_SETCOLUMNORDERARRAY (+ LVM_FIRST 58))
(define LVM_GETCOLUMNORDERARRAY (+ LVM_FIRST 59))

(define LVN_FIRST               -100)
(define LVN_ITEMCHANGED         (- LVN_FIRST 1))
(define LVN_COLUMNCLICK         (- LVN_FIRST 8))

(define NM_FIRST                0)
(define NM_DBLCLK               (- NM_FIRST 3))

(define LVNI_SELECTED           #x0002)
(define LVIS_SELECTED           #x0002)

(define-cstruct _LVCOLUMN
  ([mask _UINT]
   [fmt _int]
   [cx _int]
   [pszText _permanent-string/utf-16]
   [cchTextMax _int]
   [iSubItem _int]
   [iImage _int]
   [iOrder _int]
   [cxMin _int]
   [cxDefault _int]
   [cxIdeal _int]))

(define (make-lvcolumn flags label)
  (make-LVCOLUMN flags
                 0 0 
                 label
                 0 0 0 0 0 0 0))

(define (free-lvcolumn lv)
  (let ([s (LVCOLUMN-pszText lv)])
    (when s (free s))))

(define column-desc (cast (malloc (ctype-sizeof _LVCOLUMN) 'raw)
                          _pointer 
                          _LVCOLUMN-pointer))
(memset column-desc 0 (ctype-sizeof _LVCOLUMN))

;; Microsoft docs say to add padding to a ListView
;; item's width, but it doesn't say how much padding:
(define COLUMN-PADDING 16)

(define-cstruct _LVITEM
  ([mask _UINT]
   [iItem _int]
   [iSubItem _int]
   [state _UINT]
   [stateMask _UINT]
   [pszText _permanent-string/utf-16]
   [cchTextMax _int]
   [iImage _int]
   [lParam _LPARAM]
   [iIndent _int]
   [iGroupId _int]
   [cColumns _UINT]
   [puColumns _UINT]
   [piColFmt _int]
   [iGroup _int]))

(define (make-lvitem flags pos col label)
  (make-LVITEM flags
               pos col
               0 0 ; state & statemask
               label
               0 0 
               0 ; lParam
               0 0 0
               0 0 0))

(define (free-lvitem lv)
  (let ([s (LVITEM-pszText lv)])
    (when s (free s))))

(define-cstruct _NMLISTVIEW
  ([hdr _NMHDR]
   [iItem _int]
   [iSubItem _int]
   ;; ....
   ))

;; ------------------------------------------------------------

(define list-box%
  (class item%
    (init parent cb
          label kind x y w h
          choices style
          font label-font
          columns column-order)

    (inherit set-size set-control-font
             get-client-size)

    (define num-columns (length columns))
    (define single-column? (and (= 1 num-columns)
                                (not (memq 'column-headers style))
                                (not (memq 'variable-columns style))))
    (define single? (eq? 'single kind))

    (define hwnd
      (CreateWindowExW/control WS_EX_CLIENTEDGE
                               (if single-column?
                                   "PLTLISTBOX"
                                   "PLTSysListView32")
                               label
                               (if single-column?
                                   (bitwise-ior WS_CHILD WS_CLIPSIBLINGS LBS_NOTIFY
                                                WS_VSCROLL
                                                (if (memq 'hscroll style) WS_HSCROLL 0)
                                                (cond
                                                 ;; Win32 sense of "multiple" and "extended" is backwards
                                                 [(eq? kind 'extended) LBS_MULTIPLESEL]
                                                 [(eq? kind 'multiple) LBS_EXTENDEDSEL]
                                                 [else 0]))
                                   (bitwise-ior WS_CHILD WS_CLIPSIBLINGS
                                                LVS_REPORT 
                                                (if (memq 'column-headers style)
                                                    0
                                                    LVS_NOCOLUMNHEADER)
                                                LVS_SHOWSELALWAYS
                                                WS_VSCROLL
                                                (if (memq 'hscroll style) WS_HSCROLL 0)
                                                (cond
                                                 [(eq? kind 'extended) 0]
                                                 [(eq? kind 'multiple) 0]
                                                 [else LVS_SINGLESEL])))
                               0 0 0 0
                               (send parent get-content-hwnd)
                               #f
                               hInstance
                               #f))

    (when single-column?
      (for ([s (in-list choices)])
        (SendMessageW/str hwnd LB_ADDSTRING 0 s)))
    
    (unless single-column?
      (for ([label (in-list columns)]
            [col (in-naturals)])
        (atomically
         (let ([col-desc (make-lvcolumn LVCF_TEXT label)])
           (SendMessageW/ptr hwnd LVM_INSERTCOLUMNW col col-desc)
           (free-lvcolumn col-desc)))
        (let* ([label-width
                (SendMessageW/str hwnd LVM_GETSTRINGWIDTHW 0 label)]
               [max-width
                (if (zero? col)
                    ;; size column based on the content:
                    (for/fold ([w label-width]) ([s (in-list choices)]
                                                 [i (in-naturals)])
                      (atomically
                       (let ([lv (make-lvitem (bitwise-ior LVIF_DI_SETITEM
                                                           LVIF_TEXT)
                                              i
                                              col
                                              s)])
                         (if (zero? col)
                             (SendMessageW/ptr hwnd LVM_INSERTITEMW 0 lv)
                             (SendMessageW/ptr hwnd LVM_SETITEMTEXTW i lv))
                         (free-lvitem lv)))
                      (max w 
                           (SendMessageW/str hwnd LVM_GETSTRINGWIDTHW 0 s)))
                    ;; size column based on the label, only:
                    label-width)])
          (let ([col-desc (make-lvcolumn LVCF_WIDTH #f)])
            (set-LVCOLUMN-cx! col-desc (+ max-width COLUMN-PADDING))
            (SendMessageW/ptr hwnd LVM_SETCOLUMNW col col-desc)))))

    (unless single-column?
      (SendMessageW hwnd LVM_SETEXTENDEDLISTVIEWSTYLE 0 
                    (bitwise-ior LVS_EX_FULLROWSELECT
                                 (if (memq 'reorderable-headers style)
                                     LVS_EX_HEADERDRAGDROP
                                     0))))

    (when column-order
      (set-column-order column-order))

    (super-new [callback cb]
               [parent parent]
               [hwnd hwnd]
               [style style])

    (set-control-font font)
    (set-size #f #f 40 60)

    (define callback cb)

    (define/override (is-command? cmd)
      (if single-column?
          (or (= cmd LBN_SELCHANGE)
              (= cmd LBN_DBLCLK))
          (or (= cmd LVN_ITEMCHANGED)
              (= cmd NM_DBLCLK)
              (= cmd LVN_COLUMNCLICK))))
    
    (define pending-changed (box #f))
    (define/override (do-command cmd control-hwnd)
      ;; LVN_ITEMCHANGED notifications, in particular, get
      ;; set for each item that changes in a selection change.
      ;; Use a box to cancel pending callbacks to collapse the
      ;; multiple callbacks into one.
      (set-box! pending-changed #f)
      (let ([b (box #t)]
            [t (if (if single-column?
                       (= cmd LBN_SELCHANGE)
                       (= cmd LVN_ITEMCHANGED))
                   'list-box
                   'list-box-dclick)])
        (unless (eq? t 'list-box-dclick)
          (set! pending-changed b))
        (queue-window-event 
         this 
         (lambda ()
           (when (unbox b)
             (callback this
                       (new control-event%
                            [event-type t]
                            [time-stamp (current-milliseconds)])))))))
    
    (define/override (do-command-ex cmd control-hwnd nmhdr)
      (if (and (not single-column?)
               (= cmd LVN_COLUMNCLICK))
          (let ([col (NMLISTVIEW-iSubItem
                      (cast nmhdr _pointer _NMLISTVIEW-pointer))])
            (queue-window-event this (lambda ()
                                       (callback this
                                                 (new column-control-event%
                                                      [column col]
                                                      [event-type 'list-box-column]
                                                      [time-stamp (current-milliseconds)])))))
          (super do-command-ex cmd control-hwnd nmhdr)))
    
    (define num (length choices))
    (define/public (number) num)

    (define data (map (lambda (x) (box #f)) choices))
    (define/public (get-data i) (unbox (list-ref data i)))
    (define/public (set-data i v) (set-box! (list-ref data i) v))

    (define/public (set-string i str [col 0])
      (atomically
       (if single-column?
           (begin
             (SendMessageW/str hwnd LB_INSERTSTRING i str)
             (SendMessageW hwnd LB_DELETESTRING (add1 i) 0))
           (let ([lv (make-lvitem 0
                                  0
                                  col
                                  str)])
             (SendMessageW/ptr hwnd LVM_SETITEMTEXTW i lv)
             (free-lvitem lv)))
       (void)))


    (define/public (set-column-order column-order)
      (unless single-column?
        (let* ([count num-columns]
               [a (malloc _int count)])
          (for ([n (in-list column-order)]
                [i (in-range count)])
            (ptr-set! a _int i n))
          (SendMessageW/ptr hwnd LVM_SETCOLUMNORDERARRAY count a)
          (InvalidateRect hwnd #f #f))))

    (define/public (get-column-order)
      (if single-column?
          '(0)
          (let* ([count num-columns]
                 [a (malloc _int count)])
            (SendMessageW/ptr hwnd LVM_GETCOLUMNORDERARRAY count a)
            (cast a _gcpointer (_list o _int count)))))

    (define/public (set-column-label col s)
      (unless single-column?
        (atomically
         (let ([col-desc (make-lvcolumn LVCF_TEXT s)])
           (SendMessageW/ptr hwnd LVM_SETCOLUMNW col col-desc)
           (free-lvcolumn col-desc)))))

    (define min-col-width 0) ; not kept for us by XP
    (define max-col-width 10000)
    (define/public (set-column-size col w mn mx)
      (if single-column?
          (atomically
           (set! min-col-width mn)
           (set! max-col-width mx))
          (atomically
           (let ([col-desc (make-lvcolumn (bitwise-ior LVCF_WIDTH LVCF_MINWIDTH) #f)])
             (set-LVCOLUMN-cx! col-desc w)
             (set-LVCOLUMN-cxMin! col-desc mn)
             (SendMessageW/ptr hwnd LVM_SETCOLUMNW col col-desc)
             (set! min-col-width mn)
             (set! max-col-width mx)))))

    (define/public (get-column-size col)
      (atomically
       (let ([col-desc (make-lvcolumn (bitwise-ior LVCF_WIDTH LVCF_MINWIDTH) #f)])
         (SendMessageW/ptr hwnd LVM_GETCOLUMNW col col-desc)
         (let ([v (LVCOLUMN-cx col-desc)])
           (values (max v min-col-width) ; in XP, may have been sized too small
                   min-col-width
                   max-col-width)))))

    (define/public (append-column label)
      (atomically
       (let ([col-desc (make-lvcolumn (bitwise-ior LVCF_TEXT
                                                   LVCF_WIDTH)
                                      label)])
         (set-LVCOLUMN-cx! col-desc 
                           (SendMessageW/str hwnd LVM_GETSTRINGWIDTHW 0 label))
         (SendMessageW/ptr hwnd LVM_INSERTCOLUMNW num-columns col-desc)
         (free-lvcolumn col-desc))
       (set! num-columns (add1 num-columns))))

    (define/public (delete-column col)
      (atomically
       (SendMessageW hwnd LVM_DELETECOLUMN col 0)
       (set! num-columns (sub1 num-columns))))

    (define/public (set-first-visible-item i)
      (if single-column?
          (void (SendMessageW hwnd LB_SETTOPINDEX i 0))
          (let ([c (SendMessageW hwnd LVM_GETCOUNTPERPAGE 0 0)])
            (unless (= c i)
              (if (> (SendMessageW hwnd LVM_GETTOPINDEX 0 0)
                     i)
                  (void (SendMessageW hwnd LVM_ENSUREVISIBLE i 0))
                  (void (SendMessageW hwnd LVM_ENSUREVISIBLE (sub1 (min num (+ i c))) 0)))))))

    (define/public (get-first-item)
      (SendMessageW hwnd (if single-column? LB_GETTOPINDEX LVM_GETTOPINDEX) 0 0))

    (define/public (number-of-visible-items)
      (if single-column?
          (let ([ih (SendMessageW hwnd LB_GETITEMHEIGHT 0 0)])
            (let ([w (box 0)]
                  [h (box 0)])
              (get-client-size w h)
              (quotient (unbox h) ih)))
          (SendMessageW hwnd LVM_GETCOUNTPERPAGE 0 0)))

    (define/public (clear)
      (atomically
       (set! data null)
       (set! num 0)
       (void (SendMessageW hwnd (if single-column? 
                                    LB_RESETCONTENT 
                                    LVM_DELETEALLITEMS)
                           0 0))))

    (define/public (set choices . more-choices)
      (atomically
       (ShowWindow hwnd SW_HIDE)
       (clear)
       (if single-column?
           (for ([s (in-list choices)])
             (SendMessageW/str hwnd LB_ADDSTRING 0 s))
           (for ([choices (in-list (cons choices more-choices))]
                 [col (in-naturals)])
             (for ([s (in-list choices)]
                   [i (in-naturals)])
             (atomically
              (let ([lv (make-lvitem (bitwise-ior LVIF_DI_SETITEM
                                                           LVIF_TEXT)
                                     i
                                     col
                                     s)])
                (if (zero? col)
                    (SendMessageW/ptr hwnd LVM_INSERTITEMW 0 lv)
                    (SendMessageW/ptr hwnd LVM_SETITEMTEXTW i lv))
                (free-lvitem lv))))))
       (set! data (map (lambda (s) (box #f)) choices))
       (set! num (length choices))
       (ShowWindow hwnd SW_SHOW)))

    (public [append* append])
    (define (append* s [v #f])
      (atomically
       (if single-column?
           (SendMessageW/str hwnd LB_ADDSTRING 0 s)
           (let ([lv (make-lvitem (bitwise-ior LVIF_DI_SETITEM
                                               LVIF_TEXT)
                                  num
                                  0
                                  s)])
             (SendMessageW/ptr hwnd LVM_INSERTITEMW 0 lv)
             (free-lvitem lv)))
       (set! num (add1 num))
       (set! data (append data (list (box v))))))

    (define/public (delete i)
      (atomically
       (set! data (append (take data i) (drop data (add1 i))))
       (set! num (sub1 num))
       (void (SendMessageW hwnd (if single-column?
                                    LB_DELETESTRING
                                    LVM_DELETEITEM)
                           i 0))))

    (define/public (get-selections)
      (atomically
       (if single-column?
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
                        (free selections))))))
           (let loop ([c (SendMessageW hwnd LVM_GETSELECTEDCOUNT 0 0)]
                      [pos -1])
             (cond
              [(zero? c) null]
              [else (let ([pos (SendMessageW hwnd LVM_GETNEXTITEM pos LVNI_SELECTED)])
                      (cons pos (loop (sub1 c) pos)))])))))

    (define/public (get-selection)
      (let ([l (get-selections)])
        (if (null? l)
            -1
            (car l))))

    (define/public (selected? i)
      (if single-column?
          (not (zero? (SendMessageW hwnd LB_GETSEL i 0)))
          (not (zero? (SendMessageW hwnd LVM_GETITEMSTATE i LVIS_SELECTED)))))

    (define/public (select i [on? #t] [one? #t])
      (void
       (if single-column?
           (if single?
               (SendMessageW hwnd LB_SETCURSEL (if on? i -1) 0)
               (begin
                 (unless one?
                   (SendMessageW hwnd LB_SELITEMRANGE 0 (MAKELPARAM 0 num)))
                 (SendMessageW hwnd LB_SETSEL (if on? 1 0) i)))
           (let ([lv (make-lvitem 0 0 0 #f)])
             (define (set-one i on?)
               (set-LVITEM-stateMask! lv LVIS_SELECTED)
               (set-LVITEM-state! lv (if on? LVIS_SELECTED 0))
               (SendMessageW/ptr hwnd LVM_SETITEMSTATE i lv))
             (when (and on? (not single?) (not one?))
               (for ([i (in-list (get-selections))])
                 (set-one i #f)))
             (set-one i on?)))))

    (define/public (set-selection i)
      (void (select i #t #f)))

    (def/public-unimplemented get-label-font)))
