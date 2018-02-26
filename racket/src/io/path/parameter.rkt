#lang racket/base
(require "path.rkt"
         "check.rkt"
         "complete.rkt")

(provide current-directory
         current-directory-for-user
         current-load-relative-directory)

;; Note: the publicly available versions of these functions are
;; wrapped to include security-guard checks.

(define/who current-directory
  (make-parameter (case (system-path-convention-type)
		    [(unix) (path #"/" 'unix)]
		    [(windows) (path #"C:\\" 'windows)])
                  (lambda (v)
                    (check-directory-path who v))))

(define/who current-directory-for-user
  (make-parameter (current-directory)
                  (lambda (v)
                    (check-directory-path who v))))


(define/who current-load-relative-directory
  (make-parameter #f
                  (lambda (v)
                    (check who path-string? #:or-false v)
                    (and v
                         (path->complete-path v (current-directory))))))

(define (check-directory-path who v)
  (check who path-string? v)
  (path->complete-path v (current-directory)))
