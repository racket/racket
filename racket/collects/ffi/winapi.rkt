#lang racket/base

(provide win64? winapi)

(define win64? 
  (and (eq? 'windows (system-type))
       (equal? "win32\\x86_64" (path->string (system-library-subpath #f)))))

(define winapi (if win64? 'default 'stdcall))
