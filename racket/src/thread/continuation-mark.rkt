#lang racket/base
(require "check.rkt"
         (submod "thread.rkt" scheduling)
         "host.rkt")

(provide continuation-marks)

(define/who (continuation-marks k [prompt-tag (default-continuation-prompt-tag)])
  (check who (lambda (k) (or (not k) (continuation? k) (thread? k)))
         #:contract "(or/c continuation? thread? #f)"
         k)
  (check who continuation-prompt-tag? prompt-tag)
  (cond
    [(thread? k)
     (define e (thread-engine k))
     (cond
       [(eq? e 'done) (host:continuation-marks #f prompt-tag)]
       [(eq? e 'running) (current-continuation-marks)]
       [else (host:continuation-marks e prompt-tag)])]
    [else
     (host:continuation-marks k prompt-tag)]))
