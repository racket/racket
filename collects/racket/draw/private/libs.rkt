#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         ffi/winapi
         (for-syntax racket/base
                     ffi/winapi))

(provide define-runtime-lib
         win64?
         (for-syntax win64?))

(define-syntax define-runtime-lib
  ;; the ids macosx unix windows don't appear to be bound here, but I added win32 and win64 anyways
  (syntax-rules (macosx unix windows win32 win64 ffi-lib)
    [(_ lib-id
        [(unix) unix-lib]
        [(macosx) (ffi-lib mac-lib) ...]
        [(windows) (ffi-lib windows-lib) ...])
     (begin
       (define-runtime-path-list libs
         (case (system-type)
           [(macosx) '((so mac-lib) ...)]
           [(unix) null]
           [(windows) `((so windows-lib) ...)]))

       (define lib-id
         (if (null? libs)
             unix-lib
             (for/fold ([v #f]) ([lib (in-list libs)])
               (ffi-lib lib)))))]
    [(_ lib-id
        [(unix) unix-lib]
        [(macosx) (ffi-lib mac-lib) ...]
        [(win32) (ffi-lib win32-lib) ...]
        [(win64) (ffi-lib win64-lib) ...])
     (begin
       (define-runtime-path-list libs
         (case (system-type)
           [(macosx) '((so mac-lib) ...)]
           [(unix) null]
           [(windows)
            (if win64?
                `((so win64-lib) ...)
                `((so win32-lib) ...))]))

       (define lib-id
         (if (null? libs)
             unix-lib
             (for/fold ([v #f]) ([lib (in-list libs)])
               (ffi-lib lib)))))]))


