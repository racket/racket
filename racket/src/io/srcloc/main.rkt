#lang racket/base
(require "../common/check.rkt"
         "../format/main.rkt"
         "../path/parameter.rkt")

(provide srcloc->string)

(define/who (srcloc->string s)
  (check who srcloc? s)
  (and (srcloc-source s)
       (cond
         [(and (srcloc-line s)
               (srcloc-column s))
          (format "~a:~s:~s"
                  (adjust-path (srcloc-source s))
                  (srcloc-line s)
                  (srcloc-column s))]
         [else
          (format "~a::~s"
                  (adjust-path (srcloc-source s))
                  (srcloc-position s))])))

(define (adjust-path p)
  (define dir (current-directory-for-user))
  ;; FIXME
  p)
