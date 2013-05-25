#lang racket
(require "status.rkt")

(define (rewrite-status #:rewrite rewrite-string s)
  (match s
    [(struct exit (start end command-line output-log code))
     (make-exit start end 
                (rewrite-strings #:rewrite rewrite-string command-line)
                (rewrite-events #:rewrite rewrite-string output-log)
                code)]
    [(struct timeout (start end command-line output-log))
     (make-timeout start end 
                   (rewrite-strings #:rewrite rewrite-string command-line)
                   (rewrite-events #:rewrite rewrite-string output-log))]))

(define (rewrite-strings #:rewrite rewrite-string los)
  (map rewrite-string los))
(define (rewrite-events #:rewrite rewrite-string loe) 
  (map (rewrite-event #:rewrite rewrite-string) loe))
(define (rewrite-event #:rewrite rewrite-bytes)
  (match-lambda
    [(struct stdout (b)) (make-stdout (rewrite-bytes b))]
    [(struct stderr (b)) (make-stderr (rewrite-bytes b))]))


(define rewrite-string/c
  ((or/c string? bytes?) . -> . (or/c string? bytes?)))

(provide/contract
 [rewrite-string/c contract?]
 [rewrite-status (#:rewrite rewrite-string/c status? . -> . status?)])
