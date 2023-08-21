#lang racket/base

(provide win64? winapi)

(define win64? 
  (and (eq? 'windows (system-type))
       (eqv? 64 (system-type 'word))))

(define winapi (if (or win64?
                       (not (eq? 'windows (system-type))))
                   'default
                   'stdcall))
