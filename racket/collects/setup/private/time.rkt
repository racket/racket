#lang racket/base

(provide add-time)

(define (add-time s)
  (define now (seconds->date (current-seconds)))
  (string-append
   s
   (make-string (max 0 (- 55 (string-length s))) #\space)
   (format "[~a:~a~a:~a~a]"
           (date-hour now)
           (if ((date-minute now) . < . 10)
               "0"
               "")
           (date-minute now)
           (if ((date-second now) . < . 10)
               "0"
               "")
           (date-second now))))

