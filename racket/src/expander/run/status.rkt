#lang racket/base

(provide log-status
         lines)

(define stdout (current-output-port))

(define (log-status fmt . args)
  (apply fprintf stdout (string-append fmt "\n") args))

(define (lines prefix vals)
  (apply
   string-append
   prefix
   (let loop ([col (string-length prefix)] [vals vals])
     (cond
      [(null? vals) null]
      [else
       (define s (format " ~a" (car vals)))
       (define slen (string-length s))
       (define new-col (+ col slen))
       (cond
        [(new-col . < . 80)
         (cons s (loop new-col (cdr vals)))]
        [else
         (list* "\n" (make-string (string-length prefix) #\space) s
                (loop (+ (string-length prefix) slen)
                      (cdr vals)))])]))))
