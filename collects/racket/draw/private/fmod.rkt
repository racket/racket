#lang racket/base
(require ffi/unsafe)

(provide fmod)
(define fmod (get-ffi-obj 'fmod #f (_fun _double _double -> _double)))
