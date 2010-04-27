#lang racket/base

(provide bytes->utf-16-bytes
         utf-16-bytes->bytes)

;; Convert bytes for a path into a UTF-16 encoding
;;  (which can have unpaired surrogates)
(define (bytes->utf-16-bytes b)
  (let ([c (bytes-open-converter "platform-UTF-8-permissive" "platform-UTF-16")])
    (let-values ([(s n status) (bytes-convert c b)])
      (if (eq? status 'complete)
          s
          ;; Must be a trailing unpaired surrogate.
          ;; Force it to convert by adding an "a" suffix, then
          ;; strip the "a" back off:
          (bytes-append
           s
           (let-values ([(s n status) 
                         (bytes-convert c (bytes-append (subbytes b n) #"a"))])
             (subbytes s 0 (- (bytes-length s) 2))))))))

;; Convert a UTF-16 encoding (which can have unpaired surrogates)
;;  into bytes for a path
(define (utf-16-bytes->bytes b)
  (let ([c (bytes-open-converter "platform-UTF-16" "platform-UTF-8")])
    (let-values ([(s n status) (bytes-convert c b)])
      (if (eq? status 'complete)
          s
          ;; Must be a trailing unpaired surrogate.
          ;; Force it to convert by adding an "a" suffix, then
          ;; strip the "a" back off:
          (bytes-append
           s
           (let-values ([(s n status) 
                         (bytes-convert c (bytes-append (subbytes b n) #"a\0"))])
             (subbytes s 0 (sub1 (bytes-length s)))))))))
