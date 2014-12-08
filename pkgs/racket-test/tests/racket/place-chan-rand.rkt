#lang racket/base
(require racket/place 
         redex/reduction-semantics
         racket/runtime-path)

(define-language L
  (legal-message m i)
  
  (m (vector f ...)
     (shared-flvector fl ...)
     (shared-fxvector fx ...)
     (shared-bytes byte ...)
     str)
  
  (i (cons i i)
     (vector-immutable i ...)
     (string->immutable-string str) 
     (make-prefab-struct pf-type legal-message ...)
     (string->path (string-append (if (path-string? string)
                                      string
                                      "?")
                                  "x"))
     (let-values ([(a b) (place-channel)]) a)
     (let-values ([(a b) (place-channel)]) b)
     f)
  
  (f '()
     #f
     #t
     num
     (string-ref one-len-str 0))
  
  (pf-type sym
           ;; these ones place constraints on the number of fields (why?)
           ;; so the generator needs to be smarter here
           #;(list sym byte (list byte legal-message) (vector))
           #;(list* sym byte (list byte legal-message) (vector) pf-type))
  
  (str (string-append one-len-str ...))
  (sym (string->symbol str))
  (one-len-str "a" "b" "λ" "龍")
  (num fx
       fl
       1/2 1/3 1/4
       (sqrt num)
       (+ num num)
       (* num 0+1i)
       (- num))
  (fx byte (- byte) 
      (- (expt 2 30) 1)
      (- (expt 2 30))
      (if (eq? (- (expt 2 62))
               (- (expt 2 62)))
          (- (expt 2 62))
          17)
      (if (eq? (- (expt 2 62))
               (- (expt 2 62)))
          (- (expt 2 62) 1)
          17))
  (fl 1.1 1.2 1.3 0.0 1e12 -1e33 1e-11 -1e-22 +inf.0 -inf.0 +nan.0)
  (byte 0 1 2 8 100 255))
  
(define-runtime-path return-place.rkt "place-chan-rand-help.rkt")
(define pch (dynamic-place return-place.rkt 'start))

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/fixnum)
  (namespace-require 'racket/place))

(define (try-message msg-code)
  ;; (printf "trying ~s\n" msg-code) ;; helpful when crashing ...
  (define msg (eval msg-code ns))
  (unless (place-message-allowed? msg-code)
    #f)
  (if (zero? (random 10))
      ;; put message into a channel to abandon to test finalization:
      (let-values ([(i o) (place-channel)])
        (place-channel-put i msg)
        #t)
      ;; normal round-trip checking:
      (equal? msg (place-channel-put/get pch msg))))

(redex-check L legal-message 
             (try-message (term legal-message))
             #:attempts 10000)

