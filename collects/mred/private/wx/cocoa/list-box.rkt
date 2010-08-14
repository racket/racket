#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
         (only-in scheme/list take drop)
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "window.rkt"
         "../common/event.rkt")
(unsafe!)
(objc-unsafe!)

(provide list-box%)

;; ----------------------------------------

(import-class NSScrollView NSTableView NSTableColumn NSCell NSIndexSet)
(import-protocol NSTableViewDataSource)

(define-objc-class MyTableView NSTableView
  #:mixins (FocusResponder KeyMouseResponder)
  [wxb]
  [-a _id (preparedCellAtColumn: [_NSInteger column] row: [_NSInteger row])
      (let ([wx (->wx wxb)])
        (tell (tell NSCell alloc) initTextCell: #:type _NSString 
              (if wx (send wx get-row row) "???")))]
  [-a _void (doubleClicked: [_id sender])
      (queue-window*-event wxb (lambda (wx) (send wx clicked 'list-box-dclick)))]
  [-a _void (tableViewSelectionDidChange: [_id aNotification])
      (queue-window*-event wxb (lambda (wx) (send wx clicked 'list-box)))])

(define-objc-class MyDataSource NSObject
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
            (send wx get-row rowIndex)
            "???"))])

(define (remove-nth data i)
  (cond
   [(zero? i) (cdr data)]
   [else (cons (car data) (remove-nth (cdr data) (sub1 i)))]))

(defclass list-box% item%
  (init parent cb
        label kind x y w h
        choices style
        font label-font)
  (inherit set-size init-font)

  (define source (as-objc-allocation
                  (tell (tell MyDataSource alloc) init)))
  (set-ivar! source wxb (->wxb this))

  (define items choices)
  (define data (map (lambda (x) (box #f)) choices))
  (define count (length choices))

  (define cocoa (as-objc-allocation
                 (tell (tell NSScrollView alloc) init)))
  (define content-cocoa (let ([content-cocoa 
                              (as-objc-allocation
                               (tell (tell MyTableView alloc) init))])
                         (tellv content-cocoa setDelegate: content-cocoa)
                         (tellv content-cocoa setDataSource: source)
                         (tellv content-cocoa addTableColumn:
                                (as-objc-allocation
                                 (tell (tell NSTableColumn alloc) initWithIdentifier: content-cocoa)))
                         (init-font content-cocoa font)
                         content-cocoa))
  (set-ivar! content-cocoa wxb (->wxb this))

  (tellv cocoa setDocumentView: content-cocoa)
  (tellv cocoa setHasVerticalScroller: #:type _BOOL #t)
  (tellv content-cocoa setHeaderView: #f)
  (unless (eq? kind 'single)
    (tellv content-cocoa setAllowsMultipleSelection: #:type _BOOL #t))

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

  (define/public (get-selection)
    (tell #:type _NSInteger content-cocoa selectedRow))
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
    
  (define/private (visible-range)
    (tell #:type _NSRange content-cocoa 
          rowsInRect: #:type _NSRect (tell #:type _NSRect cocoa documentVisibleRect)))

  (define/public (get-first-item)
    (NSRange-location (visible-range)))
  (define/public (number-of-visible-items)
    (NSRange-length (visible-range)))
  (define/public (set-first-visible-item i)
    ;; FIXME: visble doesn't mean at top:
    (tellv content-cocoa scrollRowToVisible: #:type _NSInteger i))

  (define/public (set-string i s)
    (set! items
          (append (take items i)
                  (list s)
                  (drop items (add1 i))))
    (reset))

  (define/public (number)
    ;; Can be called by event-handling thread
    count)
  (define/public (get-row n)
    ;; Can be called by event-handling thread
    (list-ref items n))

  (define callback cb)
  (define/public (clicked event-type)
    (unless (zero? count)
      (callback this (new control-event%
                          [event-type event-type]
                          [time-stamp (current-milliseconds)]))))

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
                   byExtendingSelection: #:type _BOOL extend?))))
        (tellv content-cocoa deselectRow: #:type _NSInteger i)))
  (define/public (set-selection i)
    (select i #t #f))

  (define/public (delete i)
    (set! count (sub1 count))
    (set! items (remove-nth items i))
    (set! data (remove-nth data i))
    (reset))
  (define/public (clear)
    (set! count 0)
    (set! items null)
    (set! data null)
    (reset))
  (define/public (set choices)
    (set! items choices)
    (set! data (map (lambda (x) (box #f)) choices))
    (set! count (length choices))
    (reset))

  (public [append* append])
  (define (append* s [v #f])
    (set! count (add1 count))
    (set! items (append items (list s)))
    (set! data (append data (list (box v))))
    (reset))

  (define/public (reset)
    (tellv content-cocoa noteNumberOfRowsChanged)
    (tellv content-cocoa reloadData)))
