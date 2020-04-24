#lang racket/base
(require "../port/string-output.rkt"
         "../port/bytes-output.rkt"
         "../port/port.rkt"
         "../port/max-output-port.rkt"
         "more-pending.rkt")

(provide write-string/max
         write-bytes/max
         
         make-output-port/max
         output-port/max-max-length)

;; A `max-length` is either
;;   #f => no limit
;;   <n> => can still write <n> more characters
;;   'full => can't write any more
;;   (cons <n> <bytes>) => pending bytes that may be replaced by "..."

(define (write-string/max str o max-length [start 0] [end (string-length str)])
  (cond
   [(eq? max-length 'full) 'full]
   [(not max-length)
    (write-string str o start end)
    #f]
   [(pair? max-length)
    (more-pending max-length start end str)]
   [else
    (define len (- end start))
    (cond
     [(len . <= . max-length)
      (write-string str o start end)
      (- max-length len)]
     [else
      (write-string str o start (+ start max-length))
      (more-pending '(0 . #"") (+ start max-length) end str)])]))

;; For measuring purposes, just treat bytes as characters:
(define (write-bytes/max bstr o max-length [start 0] [end (bytes-length bstr)])
  (cond
   [(eq? max-length 'full) 'full]
   [(not max-length)
    (write-bytes bstr o start end)
    #f]
   [(pair? max-length)
    (more-pending max-length start end bstr)]
   [else
    (define len (- end start))
    (cond
     [(len . <= . max-length)
      (write-bytes bstr o start end)
      (- max-length len)]
     [else
      (write-bytes bstr o start (+ start max-length))
      (more-pending '(0 . #"") (+ start max-length) end bstr)])]))

(define (make-output-port/max o max-length)
  (make-max-output-port o max-length))

(define (output-port/max-max-length o max-length)
  (and max-length
       (max-output-port-max-length o)))
