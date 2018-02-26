#lang racket/base
(require "../port/string-output.rkt"
         "../port/bytes-output.rkt"
         "../port/port.rkt"
         "../port/output-port.rkt")

(provide write-string/max
         write-bytes/max
         
         make-output-port/max
         output-port/max-max-length)

(define (write-string/max str o max-length [start 0] [end (string-length str)])
  (cond
   [(eq? max-length 'full) 'full]
   [(not max-length)
    (write-string str o start end)
    #f]
   [else
    (define len (- end start))
    (cond
     [(len . < . max-length)
      (write-string str o start end)
      (- max-length len)]
     [else
      (write-string str o start (+ start max-length))
      'full])]))

;; For measuring purposes, just treat bytes as characters:
(define (write-bytes/max bstr o max-length [start 0] [end (bytes-length bstr)])
  (cond
   [(eq? max-length 'full) 'full]
   [(not max-length)
    (write-bytes bstr o start end)
    #f]
   [else
    (define len (- end start))
    (cond
     [(len . < . max-length)
      (write-bytes bstr o start end)
      (- max-length len)]
     [else
      (write-bytes bstr o start (+ start max-length))
      'full])]))

(define (make-output-port/max o max-length)
  (make-core-output-port
   #:name (object-name o)
   #:data (lambda () max-length)
   #:evt o
   #:write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
     (cond
       [max-length
        (define len (- src-end src-start))
        (unless (eq? max-length 'full)
          (define write-len (min len max-length))
          (define wrote-len (write-bytes src-bstr o src-start (+ src-start write-len)))
          (if (= max-length wrote-len)
              (set! max-length 'full)
              (set! max-length (- max-length wrote-len))))
        len]
       [else
        (write-bytes src-bstr o src-start src-end)]))
   #:close void))

(define (output-port/max-max-length o max-length)
  (and max-length
       ((core-port-data o))))
