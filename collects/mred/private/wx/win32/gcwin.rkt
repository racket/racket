#lang racket/base
(require ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt")

(provide scheme_add_gc_callback
         scheme_remove_gc_callback
         create-gc-window
         make-gc-show-desc
         make-gc-hide-desc)

(define-mz scheme_add_gc_callback (_fun _racket _racket -> _racket))
(define-mz scheme_remove_gc_callback (_fun _racket -> _void))

(define-user32 ShowWindow/raw _fpointer
  #:c-id ShowWindow)
(define-gdi32 BitBlt/raw _fpointer
  #;
  (_wfun _HDC _int _int _int _int _HDC _int _int _DWORD -> _BOOL)
  #:c-id BitBlt)
(define-gdi32 SelectObject/raw _fpointer
  #:c-id SelectObject)

(define-gdi32 CreateCompatibleDC (_wfun _HDC -> _HDC))

(define SRCCOPY #x00CC0020)

(define blit-hdc (CreateCompatibleDC #f))

(define (create-gc-window parent-hwnd x y w h)
  (CreateWindowExW 0 
                   "PLTBlitTarget"
                   ""
                   (bitwise-ior WS_CHILD)
                   x y w h
                   parent-hwnd
                   #f
                   hInstance
                   #f))

(define (make-draw hwnd hbitmap w h)
  (let ([hdc (GetDC hwnd)])
    null
    (list
     (vector 'osapi_ptr_ptr->void SelectObject/raw blit-hdc hbitmap)
     (vector 'osapi_ptr_int_int_int_int_ptr_int_int_long->void
             BitBlt/raw hdc 0 0 w h blit-hdc 0 0 SRCCOPY)
     (vector 'ptr_ptr->void SelectObject/raw blit-hdc #f))))

(define (make-gc-show-desc hwnd hbitmap w h)
  (list->vector
   (append
    (list
     (vector 'osapi_ptr_int->void ShowWindow/raw hwnd SW_SHOW))
    (make-draw hwnd hbitmap w h))))

(define (make-gc-hide-desc hwnd hbitmap w h)
  (list->vector
   (append
    ;; draw the ``off'' bitmap so it changes immediately:
    (make-draw hwnd hbitmap w h)
    ;; hide the window; it may take a while for the underlying canvas
    ;; to refresh:
    (list
     (vector 'osapi_ptr_int->void ShowWindow/raw hwnd SW_HIDE)))))

