#lang racket/base
(require racket/system
         racket/format)

(provide get-date-stamp
         get-commit-stamp
         get-date+commit-stamp)

(define (get-commit-stamp)
  (define git (or (find-executable-path "git")
                  (find-executable-path "git.exe")))
  (define s (open-output-string))
  (parameterize ([current-output-port s]
                 [current-input-port (open-input-string "")])
    (system* git "log" "-1" "--pretty=format:%h"))
  (define commit-id (get-output-string s))
  commit-id)

(define (get-date-stamp)
  (define d (seconds->date (current-seconds)))
  (define (n n) (~a n #:align 'right #:pad-string "0" #:width 2))
  (~a (date-year d) (n (date-month d)) (n (date-day d))))

(define (get-date+commit-stamp)
  (~a (get-date-stamp)
      "-"
      (get-commit-stamp)))
