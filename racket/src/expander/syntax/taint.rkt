#lang racket/base
(require "syntax.rkt"
         "taint-object.rkt")

(provide taint-content
         
         syntax-tainted?
         syntax-clean?
         syntax-taint

         struct-copy/t)

(define-syntax struct-copy/t
  (syntax-rules (syntax taint props)
    [(struct-copy/t syntax s [taint v])
     (let* ([stx s]
            [t v]
            [content* (syntax-content* stx)]
            [content (if (modified-content? content*)
                         (modified-content-content content*)
                         content*)]
            [p (and (modified-content? content*)
                    (modified-content-scope-propagations+taint content*))])
       (struct-copy syntax stx
                    [content*
                     (let ([new-p (if (taint? p)
                                      t
                                      ((propagation-set-taint-ref p) p t))])
                       (if new-p
                           (modified-content content new-p)
                           content))]))]))

(define (taint-content d)
  (non-syntax-map d
                  (lambda (tail? x) x)
                  (lambda (sub-s)
                    (cond
                     [(syntax-taintness sub-s) sub-s]
                     [else (struct-copy/t syntax sub-s
                                          [taint
                                           (tainted-for-content (syntax-content sub-s))])]))))

(define (syntax-tainted? s)
  (and (syntax-taintness s) #t))

(define (syntax-clean? s)
  (not (syntax-taintness s)))

(define (syntax-taint s)
  (if (syntax-taintness s)
      s
      (struct-copy/t syntax s
                     [taint (tainted-for-content (syntax-content s))])))
