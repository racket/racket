#lang racket/base
(provide crypto-random-unix-bytes)

(define (check-urandom-exists)
  (unless (file-exists? "/dev/urandom")
    (raise (make-exn:fail:filesystem
            "crypto-random-bytes: \"/dev/urandom\" does not exist"
            (current-continuation-marks)))))

; (: crypto-random-unix-bytes (-> Positive-Integer Bytes))
(define (crypto-random-unix-bytes n)
  (check-urandom-exists)
  (call-with-input-file* "/dev/urandom"
    (lambda (port)
      (read-bytes n port))))
