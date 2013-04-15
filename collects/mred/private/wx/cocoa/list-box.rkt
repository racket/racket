#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
         (only-in racket/list take drop)
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "window.rkt"
         "font.rkt"
         "../common/event.rkt")

(provide 
 (protect-out list-box%))

;; ----------------------------------------

(import-class NSScrollView NSTableView NSTableColumn NSCell NSIndexSet)
(import-protocol NSTableViewDataSource)

(define NSLineBreakByTruncatingTail 4)

(define-objc-class RacketTableView NSTableView
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  [-a _id (preparedCellAtColumn: [_NSInteger column] row: [_NSInteger row])
      (let ([wx (->wx wxb)])
        (tell
         (let ([c (tell (tell NSCell alloc) initTextCell: #:type _NSString 
                        (if wx (send wx get-cell column row) "???"))]
               [font (and wx (send wx get-cell-font))])
           (tellv c setLineBreakMode: #:type _NSUInteger NSLineBreakByTruncatingTail)
           (when font
             (tellv c setFont: font))
           c)
         autorelease))]
  [-a _void (doubleClicked: [_id sender])
      (queue-window*-event wxb (lambda (wx) (send wx clicked 'list-box-dclick)))]
  [-a _void (tableViewSelectionDidChange: [_id aNotification])
      (queue-window*-event wxb (lambda (wx) (send wx clicked 'list-box)))]
  [-a _void (tableView: [_id view] didClickTableColumn: [_id col])
      (queue-window*-event wxb (lambda (wx) (send wx clicked-column col)))]
  [-a _void (tableViewColumnDidMove: [_id view])
      (let ([wx (->wx wxb)])
        (when wx (send wx reset-column-order)))])

(define-objc-class RacketDataSource NSObject
  #:protocols (NSTableViewDataSource)
  [wxb]
  [-a _NSInteger (numberOfRowsInTableView: [_id view])
      (let ([wx (->wx wxb)])
        (send wx number))]
  [-a _NSString (tableView: [_id aTableView]
                            objectValueForTableColumn: [_id aTableColumn]
                            row: [_NSInteger rowIndex])
      (let ([wx (->wx wxb)])
        (if wx
            (send wx get-cell aTableColumn rowIndex)
            "???"))])

(define (remove-nth data i)
  (cond
   [(zero? i) (cdr data)]
   [else (cons (car data) (remove-nth (cdr data) (sub1 i)))]))

(defclass list-box% item%
  (init parent cb
        label kind x y w h
        choices style
        font label-font 
        columns column-order)
  (inherit set-size init-font
           register-as-child)

  (define source (as-objc-allocation
                  (tell (tell RacketDataSource alloc) init)))
  (set-ivar! source wxb (->wxb this))

  (define itemss (cons choices
                       (for/list ([i (in-list (cdr columns))])
                         (for/list ([i choices])
                           ""))))
  (define num-columns (length columns))
  (define data (map (lambda (x) (box #f)) choices))
  (define count (length choices))

  (define cocoa (as-objc-allocation
                 (tell (tell NSScrollView alloc) init)))
  (define-values (content-cocoa column-cocoas)
    (let ([content-cocoa 
           (as-objc-allocation
            (tell (tell RacketTableView alloc) init))])
      (tellv content-cocoa setDelegate: content-cocoa)
      (tellv content-cocoa setDataSource: source)
      (define cols
        (for/list ([title (in-list columns)])
          (let ([col (as-objc-allocation
                      (tell (tell NSTableColumn alloc) initWithIdentifier: #:type _NSString title))])
            (tellv content-cocoa addTableColumn: col)
            (tellv (tell col headerCell) setStringValue: #:type _NSString title)
            col)))
      (init-font content-cocoa font)
      (values content-cocoa cols)))
  (set-ivar! content-cocoa wxb (->wxb this))

  (tellv cocoa setDocumentView: content-cocoa)
  (tellv cocoa setHasVerticalScroller: #:type _BOOL #t)
  (tellv cocoa setHasHorizontalScroller: #:type _BOOL #t)
  (unless (memq 'column-headers style)
    (tellv content-cocoa setHeaderView: #f))
  (define allow-multi? (not (eq? kind 'single)))
  (when allow-multi?
    (tellv content-cocoa setAllowsMultipleSelection: #:type _BOOL #t))
  (unless (memq 'reorderable-headers style)
    (tellv content-cocoa setAllowsColumnReordering: #:type _BOOL #f))

  (when column-order
    (set-column-order column-order))
  (define/public (set-column-order column-order)
    (atomically
     (for ([c (in-list column-cocoas)])
       (tellv c retain)
       (tellv content-cocoa removeTableColumn: c))
     (for ([pos (in-list column-order)])
       (let ([c (list-ref column-cocoas pos)])
         (tellv content-cocoa addTableColumn: c)
         (tellv c release)))
     (reset-column-order)))

  (define/public (set-column-label i s)
    (let ([col (list-ref column-cocoas i)])
      (tellv (tell col headerCell) setStringValue: #:type _NSString s)
      (reset)))

  (define/public (set-column-size i w min-w max-w)
    (let ([col (list-ref column-cocoas i)])
      (tellv col setMinWidth: #:type _CGFloat min-w)
      (tellv col setMaxWidth: #:type _CGFloat max-w)
      (tellv col setWidth: #:type _CGFloat w)))

  (define/public (get-column-size i)
    (let ([col (list-ref column-cocoas i)]
          [int (lambda (v) (inexact->exact (round v)))])
      (values
       (int (tell #:type _CGFloat col width))
       (int (tell #:type _CGFloat col minWidth))
       (min 10000 (int (tell #:type _CGFloat col maxWidth))))))

  (define/override (get-cocoa-content) content-cocoa)
  (define/override (get-cocoa-control) content-cocoa)

  (super-new [parent parent]
             [cocoa cocoa]
             [no-show? (memq 'deleted style)]
             [callback cb])

  (set-size 0 0 32 50)
  ; (tellv content-cocoa sizeToFit)

  (tellv content-cocoa setTarget: content-cocoa)
  (tellv content-cocoa setDoubleAction: #:type _SEL (selector doubleClicked:))

  (def/public-unimplemented get-label-font)

  (define cell-font (and font (font->NSFont font)))
  (when cell-font
    (tellv content-cocoa setRowHeight: #:type _CGFloat
           (+ (tell #:type _CGFloat cell-font defaultLineHeightForFont) 2)))

  (define/public (get-cell-font)
    cell-font)

  (define/public (get-selection)
    (if allow-multi?
        (let ([l (get-selections)])
          (if (null? l)
              -1
              (car l)))
        (tell #:type _NSInteger content-cocoa selectedRow)))
  (define/public (get-selections)
    (atomically
     (with-autorelease
      (let ([v (tell content-cocoa selectedRowIndexes)])
        (begin0
         (let loop ([i (tell #:type _NSInteger v firstIndex)])
           (cond
            [(= i NSNotFound) null]
            [else (cons i (loop (tell #:type _NSInteger v 
                                      indexGreaterThanIndex: #:type _NSInteger i)))])))))))
    
  (define/private (header-height)
    (let ([hv (tell content-cocoa headerView)])
      (if hv
          (NSSize-height (NSRect-size (tell #:type _NSRect hv frame)))
          0)))

  (define/public (number-of-visible-items)
    (define doc (tell #:type _NSRect cocoa documentVisibleRect))
    (define h (+ (tell #:type _CGFloat content-cocoa rowHeight)
                 (NSSize-height (tell #:type _NSSize content-cocoa intercellSpacing))))
    (define doc-h (- (NSSize-height (NSRect-size doc))
                     (header-height)))
    (define n (floor (/ doc-h h)))
    (if (rational? n)
        (max 1 (inexact->exact n))
        1))
  (define/public (get-first-item)
    (define doc (tell #:type _NSRect cocoa documentVisibleRect))
    (define h (header-height))
    (NSRange-location (tell #:type _NSRange content-cocoa 
                            rowsInRect: #:type _NSRect 
                            (if (zero? h)
                                doc
                                (make-NSRect (NSRect-origin doc)
                                             (make-NSSize (NSSize-width (NSRect-size doc))
                                                          (- (NSSize-height (NSRect-size doc)) h)))))))

  (define/public (set-first-visible-item i)
    (define num-vis (number-of-visible-items))
    (define start (max 0 (min i (- count num-vis))))
    (tellv content-cocoa scrollRowToVisible: #:type _NSInteger start)
    (tellv content-cocoa scrollRowToVisible: #:type _NSInteger (+ start (sub1 num-vis))))

  (define/private (replace items i s)
    (append (take items i)
            (list s)
            (drop items (add1 i))))

  (define/public (set-string i s [col 0])
    (let ([new-itemss (replace 
                       itemss
                       col
                       (replace (list-ref itemss col)
                                i
                                s))])
      (set! itemss new-itemss))
    (reset))

  (define/public (number)
    ;; Can be called by event-handling thread
    count)
  (define/public (get-cell col n)
    ;; Can be called by event-handling thread
    (let ([col (if (number? col)
                   (order->number col)
                   (col->number col))])
      (if (col . > . num-columns) ; can happen as column is deleted
          ""
          (list-ref (list-ref itemss col) n))))

  (define/private (col->number col)
    (let loop ([l column-cocoas] [pos 0])
      (cond
       [(null? l) #f]
       [(ptr-equal? (car l) col) pos]
       [else (loop (cdr l) (add1 pos))])))

  ;; When columns are rearranged, we have to be able to map
  ;; from current column numbers to original column numbers
  (define order-vector #f)
  (define/private (order->number col)
    (prep-order-vector)
    (vector-ref order-vector col))
  (define/private (prep-order-vector)
    (unless order-vector
      (let ([vec (make-vector (length column-cocoas))])
        (let ([array (tell content-cocoa tableColumns)])
          (for/list ([i (in-range (tell #:type _NSUInteger array count))])
            (let ([col (tell array objectAtIndex: #:type _NSUInteger i)])
              (vector-set! vec i (col->number col)))))
        (set! order-vector vec))))
  (define/public (reset-column-order)
    (set! order-vector #f))

  (define/public (get-column-order)
    (prep-order-vector)
    (vector->list order-vector))

  (define/public (append-column title)
    (atomically
     (let ([col (as-objc-allocation
                 (tell (tell NSTableColumn alloc) initWithIdentifier: content-cocoa))])
       (tellv content-cocoa addTableColumn: col)
       (tellv (tell col headerCell) setStringValue: #:type _NSString title)
       (set! column-cocoas (append column-cocoas (list col)))
       (set! itemss (append itemss
                            (list (for/list ([i (in-list (car itemss))])
                                    ""))))
       (set! num-columns (add1 num-columns))
       (reset-column-order)))
    (reset))

  (define/public (delete-column i)
    (atomically
     (let ([c (list-ref column-cocoas i)])
       (define (drop-nth l i)
         (cond
          [(zero? i) (cdr l)]
          [else (cons (car l) (drop-nth (cdr l) (sub1 i)))]))
       (set! num-columns (sub1 num-columns))
       (tellv content-cocoa removeTableColumn: c)
       (set! column-cocoas (drop-nth column-cocoas i))
       (set! itemss (drop-nth itemss i))
       (reset-column-order)))
    (reset))

  (define callback cb)
  (define/public (clicked event-type)
    (unless (zero? count)
      (callback this (new control-event%
                          [event-type event-type]
                          [time-stamp (current-milliseconds)]))))

  (define can-click-column? (memq 'clickable-headers style))
  (define/public (clicked-column col)
    (when can-click-column?
      (let ([pos (col->number col)])
        (callback this (new column-control-event%
                            [event-type 'list-box-column]
                            [time-stamp (current-milliseconds)]
                            [column pos])))))

  (define/public (set-data i v) (set-box! (list-ref data i) v))
  (define/public (get-data i) (unbox (list-ref data i)))

  (define/public (selected? i)
    (tell #:type _BOOL content-cocoa isRowSelected: #:type _NSInteger i))

  (define/public (select i [on? #t] [extend? #t])
    (if on?
        (atomically
         (with-autorelease
          (let ([index (tell (tell NSIndexSet alloc) initWithIndex: #:type _NSUInteger i)])
            (tellv content-cocoa 
                   selectRowIndexes: index
                   byExtendingSelection: #:type _BOOL (and extend? allow-multi?)))))
        (tellv content-cocoa deselectRow: #:type _NSInteger i)))
  (define/public (set-selection i)
    (select i #t #f))

  (define/public (delete i)
    (atomically
     (set! count (sub1 count))
     (set! itemss (for/list ([items (in-list itemss)])
                    (remove-nth items i)))
     (set! data (remove-nth data i)))
    (reset))
  (define/public (clear)
    (atomically
     (set! count 0)
     (set! itemss (for/list ([items (in-list itemss)])
                    null))
     (set! data null))
    (reset))
  (define/public (set choices . more-choices)
    (atomically
     (set! itemss (cons choices more-choices))
     (set! data (map (lambda (x) (box #f)) choices))
     (set! count (length choices)))
    (reset))

  (public [append* append])
  (define (append* s [v #f])
    (atomically
     (set! count (add1 count))
     (set! itemss (cons (append (car itemss) (list s))
                        (for/list ([items (in-list (cdr itemss))])
                          (append items (list "")))))
     (set! data (append data (list (box v)))))
    (reset))

  (define/public (reset)
    (tellv content-cocoa noteNumberOfRowsChanged)
    (tellv content-cocoa reloadData))

  (define/override (maybe-register-as-child parent on?)
    (register-as-child parent on?)))
