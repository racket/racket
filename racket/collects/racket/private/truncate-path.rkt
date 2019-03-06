#lang racket/base
(provide truncate-path)

;; Drop information from the path-for-some-system `p` in the same way
;; as marshaling a path in a srcloc as part of compiled code
(define (truncate-path p)
  (define-values (base1 name1 dir?) (split-path p))
  (cond
    [(path-for-some-system? base1)
     (define-values (base2 name2 dir?) (split-path base1))
     (cond
       [(not base2)
        ;; Path at a root
        (path-for-some-system->string p)]
       [(symbol? name2)
        ;; "." or ".." before a name
        (string-append ".../" (path-elem->string name1))]
       [else
        (string-append ".../" (path-for-some-system->string name2) "/" (path-elem->string name1))])]
    [(eq? base1 'relative)
     (path-elem->string name1)]
    [else
     ;; Path is a root, ".", or ".."
     (path-for-some-system->string p)]))

(define (path-elem->string p)
  (cond
    [(eq? p 'same) "."]
    [(eq? p 'up) ".."]
    [else (path-for-some-system->string p)]))

(define (path-for-some-system->string p)
  (cond
    [(path? p) (path->string p)]
    [else
     ;; There's no right answer here, but UTF-8 likely works out
     (bytes->string/utf-8 (path->bytes p) #\uFFFD)]))
