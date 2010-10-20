#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/alloc
         racket/draw/bstr
         "../common/queue.rkt"
         "../../lock.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt"
         "../../syntax.rkt"
         "wndclass.rkt")

(provide clipboard-driver%
         has-x-selection?)

(define (has-x-selection?) #f)

;; Dummy window to own the clipboard:
(define clipboard-owner-hwnd 
  (CreateWindowExW 0 "PLTFrame" ""
                   WS_POPUP
                   0 0 10 10
                   #f
                   #f
                   hInstance
                   #f))

(define CF_UNICODETEXT      13)

(define-user32 GetClipboardOwner (_wfun -> _HWND))
(define-user32 OpenClipboard (_wfun _HWND -> _BOOL))
(define-user32 CloseClipboard (_wfun -> _BOOL))
(define-user32 EmptyClipboard (_wfun -> (r : _BOOL) -> (unless r (failed 'EmptyClipboard))))

(define-user32 RegisterClipboardFormatW (_wfun _string/utf-16 -> (r : _UINT)
                                               -> (if (zero? r)
                                                      (failed 'RegisterClipboardFormatW)
                                                      r)))

(define-kernel32 GlobalFree (_wfun _HANDLE -> (r : _HANDLE)
                                   -> (unless r (failed 'GlobalFree)))
  #:wrap (deallocator))
(define-kernel32 GlobalAlloc (_wfun _UINT _SIZE_T -> (r : _HANDLE)
                                    -> (or r (failed 'GlobalAlloc)))
  #:wrap (allocator GlobalFree))

(define-kernel32 GlobalLock (_wfun _HANDLE -> (r : _pointer)
                                   -> (or r (failed 'GlobalLock))))
(define-kernel32 GlobalUnlock (_wfun _HANDLE -> _BOOL))
(define-kernel32 GlobalSize (_wfun _HANDLE -> (r : _SIZE_T)
                                   -> (if (zero? r)
                                          (failed 'GlobalSize)
                                          r)))

(define-user32 SetClipboardData (_wfun _UINT _HANDLE -> (r : _HANDLE)
                                       -> (unless r (failed 'SetClipboardData)))
  ;; SetClipboardData accepts responsibility for the handle:
  #:wrap (deallocator cadr))

(define-user32 GetClipboardData (_wfun _UINT -> _HANDLE))

(define GHND #x0042)

(defclass clipboard-driver% object%
  (init x-selection?) ; always #f

  (define client #f)
  (define counter -1)

  (define/public (clear-client)
    ;; called in event-pump thread
    (set! client #f))

  (define/public (get-client) 
    (and client
         (if (ptr-equal? clipboard-owner-hwnd
                         (GetClipboardOwner))
             client
             (let ([c client])
               (set! client #f)
               (drop-client c)
               #f))))

  (define/private (drop-client c)
    (queue-event (send c get-client-eventspace)
                 (lambda ()
                   (send c on-replaced))))

  (define/public (set-client c types)
    (let* ([type-ids (for/list ([t (in-list types)])
                       (if (string=? t "TEXT")
                           CF_UNICODETEXT
                           (RegisterClipboardFormatW t)))]
           [all-data (for/list ([t (in-list types)]
                                [t-id (in-list type-ids)])
                       (let ([d (send c get-data t)])
                         (cond
                          [(equal? t-id CF_UNICODETEXT)
                           ;; convert UTF-8 to UTF-16:
                           (let ([p (cast (bytes->string/utf-8 d #\?)
                                          _string/utf-16 
                                          _gcpointer)])
                             (let ([len (let loop ([i 0])
                                          (if (and (zero? (ptr-ref p _byte i))
                                                   (zero? (ptr-ref p _byte (add1 i))))
                                              (+ i 2)
                                              (loop (+ i 2))))])
                               (scheme_make_sized_byte_string p
                                                              len
                                                              0)))]
                          [else
                           ;; no conversion:
                           d])))]
           [all-handles (for/list ([d (in-list all-data)])
                          (let ([h (GlobalAlloc GHND (bytes-length d))])
                            (let ([p (GlobalLock h)])
                              (memcpy p d (bytes-length d)))
                            (GlobalUnlock h)
                            h))])
      (if (null? types)
          (drop-client c)
          (atomically
           (if (OpenClipboard clipboard-owner-hwnd)
               (begin
                 (EmptyClipboard)
                 (for ([t (in-list type-ids)]
                       [h (in-list all-handles)])
                   (SetClipboardData t h))
                 (if (CloseClipboard)
                     (set! client c)
                     (drop-client c)))
               (drop-client c))))))

  (define/public (get-data format [as-text? #f])
    (let ([t (if (string=? format "TEXT")
                 CF_UNICODETEXT
                 (RegisterClipboardFormatW format))])
      (atomically
       (and (OpenClipboard clipboard-owner-hwnd)
            (let ([d (GetClipboardData t)])
              (begin0
               (and d
                    (let ([hsize (GlobalSize d)]
                          [p (GlobalLock d)])
                      (begin0
                       (if as-text?
                           (cast p _pointer _string/utf-16)
                           (scheme_make_sized_byte_string p hsize 1))
                       (GlobalUnlock d))))
               (CloseClipboard)))))))

  (define/public (get-text-data)
    (or (get-data "TEXT" #t) ""))

  (define/public (get-bitmap-data)
    #f)
                
  (super-new))
