#lang racket/base

(require racket/class)
(provide load-one)

;; loaders used for snip-class% and editor-data-class%
(define (ok-string-element? m)
  (and (string? m)
       (regexp-match? #rx"^[-a-zA-Z0-9_. ]+$" m)
       (not (string=? m ".."))
       (not (string=? m "."))))

(define (ok-lib-path? m)
  (and (pair? m)
       (eq? 'lib (car m))
       (pair? (cdr m))
       (list? m)
       (andmap ok-string-element? (cdr m))))

(define load-one
  (lambda (str id %)
    (let ([m (with-handlers ([exn:fail:read? (lambda (x) #f)])
               (and (regexp-match #rx"^[(].*[)]$" str)
                    (let* ([p (open-input-string str)]
                           [m (read p)])
                      (and (eof-object? (read p))
                           m))))])
      (if (or (ok-lib-path? m)
              (and (list? m)
                   (= (length m) 2)
                   (ok-lib-path? (car m))
                   (ok-lib-path? (cadr m))))
          (let ([m (if (ok-lib-path? m)
                       m
                       (car m))])
            (let ([result (dynamic-require m id)])
              (if (is-a? result %)
                  result
                  (error 'load-class "not a ~a% instance" id))))
          #f))))
  
