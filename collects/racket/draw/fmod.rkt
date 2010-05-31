#lang scheme/base
(require scheme/foreign)
(unsafe!)

(provide fmod)

(define fmod (get-ffi-obj 'fmod #f (_fun _double _double -> _double)))
