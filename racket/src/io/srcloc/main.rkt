#lang racket/base
(require "../common/check.rkt"
         "../format/main.rkt"
         "../path/path.rkt"
         "../path/user-relative.rkt")

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
         [(srcloc-position s)
          (format "~a::~s"
                  (adjust-path (srcloc-source s))
                  (srcloc-position s))]
         [else
          (format "~a"
                  (adjust-path (srcloc-source s)))])))

(define (adjust-path p)
  (cond
    [(is-path? p) (relative-to-user-directory p)]
    [else p]))
