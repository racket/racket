#lang racket/base

(provide extract-names)

(require racket/match)

(define (extract-names stx)
  (let ([dup-names
         (let loop ([sexp (syntax->datum stx)]
                    [names null])
           (match sexp
             [`(name ,(and sym (? symbol?)) ,pat)
              (loop pat (cons sym names))]
             [`(in-hole* ,(and sym (? symbol?)) ,pat1 ,pat2)
              (loop pat1
                    (loop pat2
                          (cons sym names)))]
             [`(in-hole ,pat1 ,pat2)
              (loop pat1
                    (loop pat2
                          (cons 'hole names)))]
             [(? list?)
              (let i-loop ([sexp sexp]
                           [names names])
                (cond
                  [(null? sexp) names]
                  [else (i-loop (cdr sexp) (loop (car sexp) names))]))]
             [else names]))]
        [ht (make-hasheq)])
    (for-each (lambda (name) (hash-set! ht name #f)) dup-names)
    (hash-map ht (lambda (x y) x))))
