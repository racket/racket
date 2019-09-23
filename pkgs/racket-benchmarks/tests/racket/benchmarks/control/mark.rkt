#lang racket/base
(require racket/include)

(include "config.rktl")

(define (f x) x)
(set! f f)

'----------------------------------------

;; Continuation marks around tail call for a loop
;; Should be similar to 'attachment-consume-pair-set-loop
;;  from "attach.ss", but extra overhead is call to an "update"
;;  function instead of an inline `cons`
'mark-set-loop
(times
 (let loop ([i N])
   (if (zero? i)
       (continuation-mark-set-first #f 'key)
       (with-continuation-mark
        'key i
        (loop (sub1 i))))))

;; Analogous to 'attachment-set-nontail-easy
;; Racket CS: in large-M configuration, nearly all
;;  time is GC due grouping attachment list
'mark-set-nontail-easy
(times
 (let loop ([i M])
   (if (zero? i)
       (continuation-mark-set-first #f 'key)
       (add1
        (with-continuation-mark
         'key i
         (add1 (loop (sub1 i))))))))

;; Racket: 1/3 time is GC
;; Racket CS: nearly all time is GC, because
;;  this creates a chain of continuation records
'mark-set-nontail-outside
(times
 (let loop ([i M])
   (if (zero? i)
       (continuation-mark-set-first #f 'key)
       ;; Note: traditional Racket uses a kind of `begin0` here
       (with-continuation-mark
        'key i
        (add1 (loop (sub1 i)))))))

;; Racket: 1/2 time is GC
;; Racket CS: nearly all time is GC, because
;;  this creates a chain of continuation records
'mark-set-nontail-inside
(times
 (let loop ([i M])
   (if (zero? i)
       (continuation-mark-set-first #f 'key)
       (add1
        (with-continuation-mark
         'key i
         (loop (sub1 i)))))))

;; Racket CS time is 25% slower than CS time due to
;;  `#%app` plus the `cons` of 'key and `i`
'mark-nontail-argument-loop
(times
 (let ([f f])
   (let loop ([i N])
     (if (zero? i)
         0
         (loop (with-continuation-mark
                'key i
                (f (sub1 i))))))))
 
;; Racket CS time is 45% slower than CS time...
'mark-nontail-argument-loop-easy
(times
 (let ([f f])
   (let loop ([i N])
     (if (zero? i)
         0
         (loop (f (with-continuation-mark
                   'key i
                   (sub1 i))))))))
