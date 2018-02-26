#lang racket/base

;; When places are implemented by plain old threads,
;; place channels need to be shared across namespaces,
;; so `#%place-struct' is included in builtins.

(provide place-struct-primitives)

(define-values (struct:TH-place-channel TH-place-channel TH-place-channel? 
                                        TH-place-channel-ref TH-place-channel-set!)
  (make-struct-type 'TH-place-channel #f 2 0 #f (list (cons prop:evt (lambda (x) (TH-place-channel-ref x 0))))))

(define-values (TH-place-channel-in TH-place-channel-out) 
  (values
   (lambda (x) (TH-place-channel-ref x 0))
   (lambda (x) (TH-place-channel-ref x 1))))

(define place-struct-primitives
  (hasheq 'struct:TH-place-channel struct:TH-place-channel
          'TH-place-channel TH-place-channel 
          'TH-place-channel? TH-place-channel? 
          'TH-place-channel-in TH-place-channel-in
          'TH-place-channel-out TH-place-channel-out))
