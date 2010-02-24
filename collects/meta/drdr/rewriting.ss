#lang scheme
(require "dirstruct.ss"
         "status.ss")

(define (rewrite-status s)
  (if (current-rev)
      (local [(define from (number->string (current-rev)))]
        (match s
          [(struct exit (start end command-line output-log code))
           (make-exit start end (rewrite-strings from command-line) (rewrite-events from output-log) code)]
          [(struct timeout (start end command-line output-log))
           (make-timeout start end (rewrite-strings from command-line) (rewrite-events from output-log))]))
      s))

(define (rewrite-strings from los)
  (map (curry rewrite-string from) los))
(define (rewrite-events from loe) 
  (map (rewrite-event from) loe))
(define (rewrite-event from)
  (match-lambda
    [(struct stdout (b)) (make-stdout (rewrite-bytes from b))]
    [(struct stderr (b)) (make-stderr (rewrite-bytes from b))]))

(define (rewrite-string from s)
  (regexp-replace* from s "<current-rev>"))

(define rewrite-bytes rewrite-string)

(provide/contract
 [rewrite-status (status? . -> . status?)])