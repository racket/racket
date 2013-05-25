#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         "../../syntax.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide 
 (protect-out menu-item%
              set-menu-item-shortcut))

(import-class NSMenuItem)

(define-objc-class RacketMenuItem NSMenuItem
  [wxb]
  (-a _void (selected: [_id sender]) 
      (let ([wx (->wx wxb)])
        (when wx
          (send wx selected))))
  (-a _void (selectedCheckable: [_id sender]) 
      (let ([wx (->wx wxb)])
        (when wx
          (send wx selected-checkable self)))))


(defclass menu-item% object%
  (define/public (id) this)
  
  (define parent #f)
  (define/public (selected)
    ;; called in Cocoa thread
    (send parent item-selected this))
  (define/public (selected-checkable cocoa)
    ;; called in Cocoa thread
    (set! checked? (not checked?))
    (tellv cocoa setState: #:type _int (if checked? 1 0))
    (send parent item-selected this))

  (define/public (set-parent p)
    (set! parent p))

  (define label #f)
  (define/public (set-label l) (set! label l))
  (define/public (get-label) label)

  (define checked? #f)
  (define/public (set-checked c?) (set! checked? c?))
  (define/public (get-checked) checked?)

  (define enabled? #t)
  (define/public (set-enabled-flag e?) (set! enabled? e?))
  (define/public (get-enabled-flag) enabled?)

  (define submenu #f)
  (define/public (set-submenu m) (set! submenu m))

  (define/public (install menu checkable?)
    (if submenu
        (send submenu install menu label enabled?)
        (let ([item (as-objc-allocation
                     (tell (tell RacketMenuItem alloc) 
                           initWithTitle: #:type _NSString (clean-menu-label (regexp-replace #rx"\t.*" label ""))
                           action: #:type _SEL #f
                           keyEquivalent: #:type _NSString ""))])
          (set-ivar! item wxb (->wxb this))
          (tellv menu addItem: item)
          (tellv item setEnabled: #:type _BOOL enabled?)
          (when checked?
            (tellv item setState: #:type _int 1))
          (tellv item setTarget: item)
          (tellv item setAction: #:type _SEL (if checkable?
                                                 (selector selectedCheckable:)
                                                 (selector selected:)))
          (set-menu-item-shortcut item label)
          (release item))))

  (super-new))

(define (set-menu-item-shortcut item label)
  (let ([shortcut (regexp-match #rx"\tCut=(.)(.*)" label)])
    (if shortcut
        (let* ([s (string-downcase (string (integer->char (string->number (caddr shortcut)))))]
               [flags (- (char->integer (string-ref (cadr shortcut) 0))
                         (char->integer #\A))]
               [mods (+ (if (positive? (bitwise-and flags 1))
                            NSShiftKeyMask
                            0)
                        (if (positive? (bitwise-and flags 2))
                            NSAlternateKeyMask
                            0)
                        (if (positive? (bitwise-and flags 4))
                            NSControlKeyMask
                            0)
                        (if (positive? (bitwise-and flags 8))
                            0
                            NSCommandKeyMask))])
          (tellv item setKeyEquivalent: #:type _NSString s)
          (tellv item setKeyEquivalentModifierMask: #:type _NSUInteger mods))
        (tellv item setKeyEquivalent: #:type _NSString ""))))
