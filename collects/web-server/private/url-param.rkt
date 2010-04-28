#lang racket/base
(require racket/contract
         net/url
         racket/match
         racket/list
         "util.rkt")
(provide/contract
 [extract-param (url? string? . -> . (or/c string? false/c))]
 [insert-param (url? string? string? . -> . url?)])

;; extract-param : url string -> string
(define (extract-param url key)
  (define ps
    (apply append
           (map path/param-param (url-path url))))
  (let/ec esc
    (for-each (lambda (p)
                (with-handlers ([exn:fail? void])
                  (define l (read/string p))
                  (esc (cdr (assoc key l)))))
              ps)
    #f))

;; insert-param : url string string -> url
;; add a path/param to the path in a url
;; (assumes that there is only one path/param)
(define (insert-param in-url key val)
  (url-replace-path
   (match-lambda
     [(list)
      (list (make-path/param 
             ""
             (list (write/string (list (cons key val))))))]
     [old
      (match (reverse old)
        [(list-rest f r)
         (reverse (list* (make-path/param 
                          (path/param-path f)
                          (list (write/string
                                 (list* (cons key val)
                                        (with-handlers ([exn:fail? (lambda _ empty)])
                                          (filter (lambda (k*v) (not (equal? key (car k*v))))
                                                  (read/string (first (path/param-param f)))))))))
                         r))])])
   in-url))
