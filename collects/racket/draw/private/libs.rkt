#lang scheme/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base))

(provide define-runtime-lib
	 win64?
	 (for-syntax win64?))

(define win64?
  (and (eq? 'windows (system-type))
       (equal? "win32\\x86_64" 
	       (path->string (system-library-subpath #f)))))
(define-for-syntax win64?
  (and (eq? 'windows (system-type))
       (equal? "win32\\x86_64" 
	       (path->string (system-library-subpath #f)))))

(define-syntax define-runtime-lib
  (syntax-rules (macosx unix windows ffi-lib)
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
               (ffi-lib lib)))))]))


