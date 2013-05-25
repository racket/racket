#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/alloc
         racket/draw/unsafe/bstr
         "../common/queue.rkt"
         "../../lock.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt"
         "../../syntax.rkt"
         "wndclass.rkt"
         "hbitmap.rkt"
         "../common/local.rkt")

(provide 
 (protect-out clipboard-driver%
              has-x-selection?))

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
(define CF_BITMAP           2)
(define CF_DIB              8)

(define DIB_RGB_COLORS      0)
(define SRCCOPY             #x00CC0020)
(define BI_BITFIELDS        3)

(define-cstruct _BITMAPINFOHEADER
  ([biSize _DWORD]
   [biWidth _LONG]
   [biHeight _LONG]
   [biPlanes _WORD]
   [biBitCount _WORD]
   [biCompression _DWORD]
   [biSizeImage _DWORD]
   [biXPelsPerMeter _LONG]
   [biYPelsPerMeter _LONG]
   [biClrUsed _DWORD]
   [biClrImportant _DWORD]))

(define-cstruct _BITMAPCOREHEADER
  ([bcSize _DWORD]
   [bcWidth _LONG]
   [bcHeight _LONG]
   [bcPlanes _WORD]
   [bcBitCount _WORD]))

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

(define-gdi32 StretchDIBits(_wfun _HDC _int _int _int _int _int _int _int _int
                                  _pointer _BITMAPINFOHEADER-pointer _UINT _DWORD
                                  -> _int))

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
                       (let ([d (let ([d (send c get-data t)])
                                  (if (string? d)
                                      (string->bytes/utf-8 d)
                                      d))])
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
			  (and d
			       (let ([h (GlobalAlloc GHND (bytes-length d))])
				 (let ([p (GlobalLock h)])
				   (memcpy p d (bytes-length d)))
				 (GlobalUnlock h)
				 h)))])
      (if (null? types)
          (drop-client c)
          (atomically
           (if (OpenClipboard clipboard-owner-hwnd)
               (begin
                 (EmptyClipboard)
                 (for ([t (in-list type-ids)]
                       [h (in-list all-handles)])
		   (when h
		     (SetClipboardData t h)))
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
    (atomically
     (and (OpenClipboard clipboard-owner-hwnd)
          (begin0
           (get-bitmap-from-clipboard)
           (CloseClipboard)))))

  (define/public (set-bitmap-data bm timestamp)
    (define h (bitmap->hbitmap bm))
    (set-cpointer-tag! h '(HBITMAP HANDLE))
    (atomically
     (when (OpenClipboard clipboard-owner-hwnd)
       (EmptyClipboard)
       (SetClipboardData CF_BITMAP h)
       (CloseClipboard)
       (void))))

  (super-new))


(define (get-bitmap-from-clipboard)
  ;; atomic mode
  (cond
   ;; I think we should be able to use CF_BITMAP always, but
   ;; it doesn't work right under Windows XP with a particular 
   ;; image created by copying in Firefox. So, we do things the
   ;; hard way.
   [(GetClipboardData CF_DIB)
    => (lambda (bits)
         (let ([bmi (cast (GlobalLock bits) _pointer _BITMAPINFOHEADER-pointer)])
           (let ([w (BITMAPINFOHEADER-biWidth bmi)]
                 [h (BITMAPINFOHEADER-biHeight bmi)]
                 [bits/pp (BITMAPINFOHEADER-biBitCount bmi)])
             (let* ([screen-hdc (GetDC #f)]
                    [hdc (CreateCompatibleDC screen-hdc)]
                    [hbitmap (if (= bits/pp 1)
                                 (CreateBitmap w h 1 1 #f)
                                 (CreateCompatibleBitmap screen-hdc w h))]
                    [old-hbitmap (SelectObject hdc hbitmap)]
                    [psize (PaletteSize bmi)])
               (ReleaseDC #f screen-hdc)
               (StretchDIBits hdc 0 0 w h
                              0 0 w h
                              (ptr-add bmi (+ (BITMAPINFOHEADER-biSize bmi) psize
					      (if (= (BITMAPINFOHEADER-biCompression bmi) BI_BITFIELDS)
						  12
						  0)))
                              bmi DIB_RGB_COLORS SRCCOPY)
               (SelectObject hdc old-hbitmap)
               (GlobalUnlock bits)
               (DeleteDC hdc)
               (begin0
                (hbitmap->bitmap hbitmap)
                (DeleteObject hbitmap))))))]
   [(GetClipboardData CF_BITMAP)
    => (lambda (hbitmap)
         (hbitmap->bitmap hbitmap))]
   [else #f]))

;; Copied from MS example:

(define (DibNumColors bmc? bmi)
  ;; /*  With the BITMAPINFO format headers, the size of the palette
  ;;  *  is in biClrUsed, whereas in the BITMAPCORE - style headers, it
  ;;  *  is dependent on the bits per pixel ( = 2 raised to the power of
  ;;  *  bits/pixel).
  ;;  */
  (if (and (not bmc?)
           (not (zero? (BITMAPINFOHEADER-biClrUsed bmi))))
      (BITMAPINFOHEADER-biClrUsed bmi)
      (let ([bits (BITMAPINFOHEADER-biBitCount bmi)])
        (case bits
          [(1) 2]
          [(4) 16]
          [(8) 256]
          [else
           ;; A 24 bitcount DIB has no color table
           0]))))

(define (PaletteSize bmi)
  (let* ([bmc? (= (BITMAPINFOHEADER-biSize bmi)
                  (ctype-sizeof _BITMAPCOREHEADER))]
         [num-colors (DibNumColors bmc? bmi)])
    (if bmc?
        (* num-colors 3)
        (* num-colors 4))))
