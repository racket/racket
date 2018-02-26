#lang racket/base
(require "config.rkt")

;; An `accum-string` is a buffer for accumulating characters.
;; We cache the buffer in the config record so that it can
;; be reused after the buffered results are extracted.

(provide accum-string-init!
         accum-string-add!
         accum-string-convert!
         accum-string-count
         set-accum-string-count!
         accum-string-get!
         accum-string-get-bytes!
         accum-string-abandon!)

(struct accum-string ([pos #:mutable]
                      [str #:mutable]))

(define (accum-string-init! config)
  (define st (read-config-st config))
  (define a (read-config-state-accum-str st))
  (cond
   [a
    (set-read-config-state-accum-str! st #f)
    (set-accum-string-pos! a 0)
    a]
   [else
    (accum-string 0 (make-string 32))]))

(define (accum-string-add! a c)
  (define pos (accum-string-pos a))
  (define str (accum-string-str a))
  (define str2
    (cond
     [(pos . < . (string-length str))
      str]
     [else
      (define str2 (make-string (* (string-length str) 2)))
      (string-copy! str2 0 str)
      (set-accum-string-str! a str2)
      str2]))
  (string-set! str2 pos c)
  (set-accum-string-pos! a (add1 pos)))

(define (accum-string-count a)
  (accum-string-pos a))

(define (set-accum-string-count! a pos)
  (set-accum-string-pos! a pos))

;; Replace `start-pos` up to `pos` with a converted
;; string. Case folding can change the string length.
(define (accum-string-convert! a convert start-pos)
  (define str (accum-string-str a))
  (define s (convert
             (substring str
                        start-pos
                        (accum-string-pos a))))
  (define len (string-length s))
  (unless ((+ len start-pos) . < . (string-length str))
    (define str2 (make-string (+ start-pos len)))
    (string-copy! str2 0 str 0 start-pos)
    (set-accum-string-str! a str2))
  (string-copy! (accum-string-str a) start-pos s)
  (set-accum-string-pos! a (+ start-pos len)))

(define (accum-string-get! a config #:start-pos [start-pos 0])
  (define s (substring (accum-string-str a)
                       start-pos
                       (accum-string-pos a)))
  (accum-string-abandon! a config)
  s)

(define (accum-string-get-bytes! a config #:start-pos [start-pos 0])
  (define bstr (string->bytes/latin-1 (accum-string-str a)
                                      #f
                                      start-pos
                                      (accum-string-pos a)))
  (accum-string-abandon! a config)
  bstr)

(define (accum-string-abandon! a config)
  (set-read-config-state-accum-str! (read-config-st config) a))
