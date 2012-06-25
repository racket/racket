#lang racket/base
(require (for-syntax racket/base))
(provide start-timing do-time)

;; some macros to do some timing, only when `timing?' is #t
(define-for-syntax timing? #f)

(define last-time #f) (define initial-time #f)
(define (set!-initial-time t) (set! initial-time t))
(define (set!-last-time t) (set! last-time t))
(define (pad str len pad-char)
  (define l (string-length str))
  (if (>= l len)
      str
      (string-append str (make-string (- len l) pad-char))))
(define-syntaxes (start-timing do-time)
  (if timing?
      (values
       (syntax-rules ()
         [(_ msg)
          (begin
            (when last-time
              (error 'start-timing "Timing already started"))
            (set!-last-time (current-process-milliseconds))
            (set!-initial-time last-time)
            (log-debug (format "TR Timing: ~a at ~a" (pad "Starting" 32 #\space) initial-time)))])
       (syntax-rules ()
         [(_ msg)
          (begin
            (unless last-time
              (start-timing msg))
            (let* ([t (current-process-milliseconds)]
                   [old last-time]
                   [diff (- t old)]
                   [new-msg (pad msg 32 #\space)])
              (set!-last-time t)
              (log-debug (format "TR Timing: ~a at ~a\tlast step: ~a\ttotal: ~a" new-msg t diff (- t initial-time)))))]))
      (values (lambda _ #'(void)) (lambda _ #'(void)))))
