#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out scheme_add_gc_callback
              scheme_remove_gc_callback
              make-gc-action-desc))

(define objc-lib (ffi-lib "libobjc"))

(define msg-send-proc (get-ffi-obj 'objc_msgSend objc-lib _fpointer))

(define-mz scheme_add_gc_callback (_fun _racket _racket -> _racket))
(define-mz scheme_remove_gc_callback (_fun _racket -> _void))

(define (make-gc-action-desc win sel val)
  (vector
   (vector (if (= (ctype-sizeof _CGFloat) 4)
               'ptr_ptr_float->void
               'ptr_ptr_double->void)
           msg-send-proc 
           win
           sel
           val)))
