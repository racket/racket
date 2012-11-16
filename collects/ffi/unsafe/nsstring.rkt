#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/atomic
         "nsalloc.rkt")

(provide (protect-out _NSString))

(import-class NSString)

(define-syntax-rule (atomically e)
  (begin
    (start-atomic)
    (begin0
     e
     (end-atomic))))

;; Access this table on only in atomic mode, so
;; that _NSString can be used in atomic mode:
(define strings (make-weak-hash))

(define (release-string s) 
  (tellv s release))

(define allocate-string
  ((allocator release-string)
   (lambda (v)
     (with-autorelease
      (tell (tell NSString alloc)
            initWithUTF8String:
            #:type _string
            v)))))

(define _NSString (make-ctype _id
                              (lambda (v)
                                (or (atomically
                                     (hash-ref strings v #f))
                                    (let ([s (allocate-string v)])
                                      (atomically (hash-set! strings v s))
                                      s)))
                              (lambda (v)
                                (with-autorelease
                                 (let ([s (tell #:type _bytes v UTF8String)])
                                   (bytes->string/utf-8 s))))))
