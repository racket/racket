#lang racket/base
(require ffi/unsafe)

(provide path->alias-bytes)

(define CoreServices
  (ffi-lib "/System/Library/Frameworks/CoreServices.framework/CoreServices"
           #:fail (lambda () #f)))

(define (make-unavailable)
  (lambda args (error "unavailable")))

(define FSNewAliasFromPath
  (get-ffi-obj 'FSNewAliasFromPath
               CoreServices
               (_fun _path _path _int (h : (_ptr o _pointer)) (_ptr io _int) -> (r : _int) -> (if (zero? r) h #f))
               (lambda () #f)))

;; Fallback when FSNewAliasFromPath is not available:
(define _FSRef _pointer) ; 80 bytes
(define FSPathMakeRef
  (get-ffi-obj 'FSPathMakeRef 
               CoreServices
               (_fun _path _FSRef (_pointer = #f) -> (r : _int) 
                     -> (unless (zero? r)
                          (error 'FSPathMakeRef "failed")))
               make-unavailable))
(define FSNewAliasUnicode
  (get-ffi-obj 'FSNewAliasUnicode
               CoreServices
               (_fun _FSRef
                     _FSRef
                     _uint 
                     _string/utf-16
                     (h : (_ptr o _pointer)) 
                     (_ptr io _int)
                     -> (r : _int)
                     -> (if (zero? r) h #f))
               make-unavailable))

(define GetAliasSize
  (get-ffi-obj 'GetAliasSize
               CoreServices
               (_fun _pointer -> _long)
               make-unavailable))
(define DisposeHandle
  (get-ffi-obj 'DisposeHandle
               CoreServices
               (_fun _pointer -> _void)
               make-unavailable))

(define (path->alias-bytes file
                           #:wrt [wrt #f])
  (define h
    (if FSNewAliasFromPath
        (FSNewAliasFromPath wrt
                            file
                            0
                            0)
        (let ([wrt-fs (and wrt (malloc 80))]
              [fs (malloc 80)])
          (when wrt (FSPathMakeRef wrt wrt-fs))
          (define-values (base name dir?) (split-path file))
          (FSPathMakeRef base fs)
          (FSNewAliasUnicode wrt-fs
                             fs
                             (string-length (path->string name)) ; FIXME: should be utf-16 count
                             (path->string name)
                             0))))
  (and h
       (let ([sz (GetAliasSize h)])
         (define bstr (make-bytes sz))
         (memcpy bstr (ptr-ref h _pointer) sz)
         (begin0
          bstr
          (DisposeHandle h)))))
