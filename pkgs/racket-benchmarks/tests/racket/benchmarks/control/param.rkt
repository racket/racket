#lang racket/base
(require racket/include
         (only-in '#%paramz
                  parameterization-key))

(include "config.rktl")

(define p (make-parameter #f))

'----------------------------------------

;; Twice as long as a typical key, because we look for the
;; root prompt (never there) instead of the default prompt
'get-paramz
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (continuation-mark-set-first
                       #f
                       parameterization-key)))))

'current-paramz
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (current-parameterization)))))

'current-paramz/local
(times
 (parameterize ([p 10])
   (let loop ([i L] [a #f])
     (if (zero? i)
         a
         (loop (sub1 i) (current-parameterization))))))

'param-ref
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (p)))))

'param-ref/local
(times
 (parameterize ([p 10])
   (let loop ([i L] [a #f])
     (if (zero? i)
         a
         (loop (sub1 i) (p))))))

'param-set!
(times
 (let loop ([i L])
   (if (zero? i)
       (p)
       (begin
         (p i)
         (loop (sub1 i))))))

'param-ref-after-set
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (p)))))

'param-bind-loop
(times
 (let loop ([i M])
   (if (zero? i)
       (p)
       (parameterize ([p i])
         (loop (sub1 i))))))

'param-bind-prim-loop
(times
 (let ([insp (make-inspector)])
   (let loop ([i M])
     (if (zero? i)
         (p)
         (parameterize ([current-inspector insp])
           (loop (sub1 i)))))))

'param-bind-nontail
(times
 (let loop ([i M])
   (if (zero? i)
       (p)
       (parameterize ([p i])
         (add1 (loop (sub1 i)))))))
