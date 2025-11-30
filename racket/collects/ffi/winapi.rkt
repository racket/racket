#lang racket/base

(provide win64? win-msvc? winapi)

(define win64? 
  (and (eq? 'windows (system-type))
       (eqv? 64 (system-type 'word))))

(define win-msvc? (system-type 'msvc))

(define winapi (if (or win64?
                       (not (eq? 'windows (system-type))))
                   'default
                   'stdcall))
