#lang scheme/unit
  (require (for-syntax scheme/base)
           scheme/promise
           (lib "class.ss")
           (lib "include-bitmap.ss" "mrlib")
           "bday.ss"
           "sig.ss"
           (lib "mred-sig.ss" "mred"))
  
  (import mred^)
  (export framework:icon^)
  
  (define eof-bitmap (delay (include-bitmap (lib "eof.gif" "icons"))))
  (define (get-eof-bitmap) (force eof-bitmap))
  
  (define anchor-bitmap (delay (include-bitmap (lib "anchor.gif" "icons"))))
  (define (get-anchor-bitmap) (force anchor-bitmap))
  
  (define lock-bitmap (delay (include-bitmap (lib "lock.gif" "icons"))))
  (define (get-lock-bitmap) (force lock-bitmap))
  (define unlock-bitmap (delay (include-bitmap (lib "unlock.gif" "icons"))))
  (define (get-unlock-bitmap) (force unlock-bitmap))
  
  (define autowrap-bitmap (delay (include-bitmap (lib "return.xbm" "icons"))))
  (define (get-autowrap-bitmap) (force autowrap-bitmap))
  (define paren-highlight-bitmap (delay (include-bitmap (lib "paren.xbm" "icons"))))
  (define (get-paren-highlight-bitmap) (force paren-highlight-bitmap))
  
  (define-syntax (make-get-cursor stx)
    (syntax-case stx ()
      [(_ name mask fallback)
       (syntax
        (let ([ans (delay 
                     (let* ([msk-b (include-bitmap (lib mask "icons"))]
                            [csr-b (include-bitmap (lib name "icons"))])
                       (if (and (send msk-b ok?)
                                (send csr-b ok?))
                           (let ([csr (make-object cursor% msk-b csr-b 7 7)])
                             (if (send csr ok?)
                                 csr
                                 (make-object cursor% fallback)))
                           (make-object cursor% fallback))))])
          (λ ()
            (force ans))))]))
  
  (define get-up/down-cursor (make-get-cursor "up-down-cursor.xbm" "up-down-mask.xbm" 'size-n/s))
  (define get-left/right-cursor (make-get-cursor "left-right-cursor.xbm" "left-right-mask.xbm" 'size-e/w))
  
  (define mrf-on-bitmap (delay (include-bitmap (lib "mrf.png" "icons"))))
  (define gc-on-bitmap (delay (include-bitmap (lib "recycle.png" "icons"))))
  
  (define (make-off-bitmap onb)
    (let* ([bitmap (make-object bitmap%
                     (send onb get-width)
                     (send onb get-height))]
           [bdc (make-object bitmap-dc% bitmap)])
      (send bdc clear)
      (send bdc set-bitmap #f)
      bitmap))
  
  (define mrf-off-bitmap (delay (make-off-bitmap (force mrf-on-bitmap))))
  (define gc-off-bitmap (delay (make-off-bitmap (force gc-on-bitmap))))
  
  (define (get-gc-on-bitmap)
    (force
     (if (mrf-bday?)
         mrf-on-bitmap
         gc-on-bitmap)))
  
  (define (get-gc-off-bitmap)
    (force
     (if (mrf-bday?)
         mrf-off-bitmap
         gc-off-bitmap)))
