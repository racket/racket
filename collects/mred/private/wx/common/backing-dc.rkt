#lang racket/base
(require racket/class
         racket/draw/dc
         racket/draw/bitmap-dc
         racket/draw/bitmap
         racket/draw/local)

(provide backing-dc%
         
         ;; scoped method names:
         get-backing-size
         flush-backing
         start-on-paint
         end-on-paint)

(define-local-member-name
  get-backing-size
  flush-backing
  start-on-paint
  end-on-paint)

(define backing-dc%
  (class (dc-mixin bitmap-dc-backend%)
    (inherit call-with-cr-lock
             internal-get-bitmap
             internal-set-bitmap)

    (super-new)

    ;; Override this method to get the right size
    (define/public (get-backing-size xb yb)
      (set-box! xb 1)
      (set-box! yb 1))

    ;; override this method to push the bitmap to
    ;;  the device that it backs
    (define/public (flush-backing bm)
      (void))

    (define on-paint-cr #f)

    (define/public (start-on-paint)
      (call-with-cr-lock
       (lambda () 
         (if on-paint-cr 
             (log-error "nested start-on-paint")
             (set! on-paint-cr (get-cr))))))

    (define/public (end-on-paint)
      (call-with-cr-lock
       (lambda () 
         (if (not on-paint-cr)
             (log-error "unbalanced end-on-paint")
             (let ([cr on-paint-cr])
               (set! on-paint-cr #f)
               (release-cr cr))))))

    (define/override (get-cr)
      (or on-paint-cr
          (let ([w (box 0)]
                [h (box 0)])
            (get-backing-size)
            (let ([bm (get-backing-bitmap (unbox w) (unbox h))])
              (internal-set-bitmap bm))
            (super get-cr))))

    (define/override (release-cr cr)
      (unless (eq? cr on-paint-cr)
        (let ([bm (internal-get-bitmap)])
          (internal-set-bitmap #f)
          (flush-backing bm)
          (release-backing-bitmap bm))))))

(define (get-backing-bitmap w h)
  (make-object bitmap% w h #f #t))

(define (release-backing-bitmap bm)
  (send bm release-bitma-storage))
