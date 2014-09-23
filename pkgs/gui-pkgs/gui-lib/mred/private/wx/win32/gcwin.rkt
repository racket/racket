#lang racket/base
(require ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt")

(provide 
 (protect-out scheme_add_gc_callback
              scheme_remove_gc_callback
              create-gc-dc
              make-gc-show-desc
              make-gc-hide-desc))

(define-mz scheme_add_gc_callback (_fun _racket _racket -> _racket))
(define-mz scheme_remove_gc_callback (_fun _racket -> _void))

(define-gdi32 BitBlt/raw _fpointer
  #;
  (_wfun _HDC _int _int _int _int _HDC _int _int _DWORD -> _BOOL)
  #:c-id BitBlt)
(define-gdi32 SelectObject/raw _fpointer
  #:c-id SelectObject)

(define SRCCOPY #x00CC0020)

(define blit-hdc (CreateCompatibleDC #f))

(define (create-gc-dc hwnd)
  (GetDC hwnd))

(define (make-draw hdc hbitmap x y w h)
  (vector
   (vector 'osapi_ptr_ptr->void SelectObject/raw blit-hdc hbitmap)
   (vector 'osapi_ptr_int_int_int_int_ptr_int_int_long->void
           BitBlt/raw hdc (->screen x) (->screen y) (->screen w) (->screen h) blit-hdc 0 0 SRCCOPY)
   (vector 'ptr_ptr->void SelectObject/raw blit-hdc #f)))

(define (make-gc-show-desc hdc hbitmap x y w h)
  (make-draw hdc hbitmap x y w h))

(define (make-gc-hide-desc hdc hbitmap x y w h)
  (make-draw hdc hbitmap x y w h))
