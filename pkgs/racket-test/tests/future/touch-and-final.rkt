#lang racket/base
(require racket/future
         racket/fixnum
         ffi/unsafe)

;; Regression test aimed at a race that was most easily exposed
;; by finalization.

;; Based on an example supplied by Dominik Joe Pantůček.

(define width 800)
(define height 600)

(define framebuffer (make-fxvector (* width height)))
(define pixels (make-bytes (* width height 4)))

(define max-depth 9)

(define (single-run)
  (define (do-bflip start end (depth 0))
    (cond ((fx< depth max-depth)
           (define cnt (fx- end start))
           (define cnt2 (fxrshift cnt 1))
           (define mid (fx+ start cnt2))
           (let ((f (future
                     (λ ()
                       (do-bflip start mid (fx+ depth 1))))))
             (do-bflip mid end (fx+ depth 1))
             (touch f)))
          (else
           (for ((i (in-range start end)))
             (define c (fxvector-ref framebuffer i))
             (bytes-set! pixels (+ (* i 4) 0) #xff)
             (bytes-set! pixels (+ (* i 4) 1) (fxand (fxrshift c 16)
                                                     #xff))
             (bytes-set! pixels (+ (* i 4) 2) (fxand (fxrshift c 8) #xff))
             (bytes-set! pixels (+ (* i 4) 3) (fxand c #xff))))))
  (do-bflip 0 (* width height)))

(void
 (thread
  (lambda ()
    (let loop ()
      (define bstr (make-bytes 1000))
      (register-finalizer bstr void)
      (loop)))))

(for ([i 10])
  (single-run))
