#lang racket/base
(require ffi/unsafe
         ffi/unsafe/alloc
         "../unsafe/cairo.ss"
         "../unsafe/bstr.ss")

(provide write_port_bytes
         make-immobile)

(define (write-port-bytes port-box bytes len)
  (write-bytes (scheme_make_sized_byte_string bytes len 0) 
               (ptr-ref port-box _racket))
  CAIRO_STATUS_SUCCESS)

(define write_port_bytes (function-ptr write-port-bytes _cairo_write_func_t))

(define make-immobile ((allocator free-immobile-cell) malloc-immobile-cell))
