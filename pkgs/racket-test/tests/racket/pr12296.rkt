#lang racket/base
(require ffi/unsafe rackunit)
(define-cpointer-type _foo)
(check-equal? (object-name foo?) 'foo?)
