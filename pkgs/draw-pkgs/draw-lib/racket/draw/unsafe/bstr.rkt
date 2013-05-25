#lang racket/base
(require ffi/unsafe)

(provide (protect-out scheme_make_sized_byte_string))


(define scheme_make_sized_byte_string 
  (get-ffi-obj 'scheme_make_sized_byte_string #f (_fun _pointer _long _int -> _scheme)))

