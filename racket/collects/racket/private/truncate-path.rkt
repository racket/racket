#lang racket/base
(provide truncate-path)

;; Drop information from the path `p` in the same way as marshaling a
;; path in a srcloc as part of compiled code
(define (truncate-path p)
  (define-values (base1 name1 dir?) (split-path p))
  (cond
    [(path? base1)
     (define-values (base2 name2 dir?) (split-path base1))
     (cond
       [(not base2)
        ;; Path at a root
        (path->string p)]
       [(symbol? name2)
        ;; "." or ".." before a name
        (string-append ".../" (path-elem->string name1))]
       [else
        (string-append ".../" (path->string name2) "/" (path-elem->string name1))])]
    [(eq? base1 'relative)
     (path-elem->string name1)]
    [else
     ;; Path is a root, ".", or ".."
     (path->string p)]))

(define (path-elem->string p)
  (cond
    [(eq? p 'same) "."]
    [(eq? p 'up) ".."]
    [else (path->string p)]))
