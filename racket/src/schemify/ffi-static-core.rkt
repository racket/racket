#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "known.rkt"
         "find-known.rkt"
         "literal.rkt"
         "fold.rkt")

(provide make-ffi-static-core)

(define (make-ffi-static-core arg-types result-type
                              abi varargs-after blocking? async-apply
                              prim-knowns primitives knowns imports mutated)
  (define (lookup id)
    (define u (unwrap id))
    (cond
      [(symbol? u)
       (find-known u prim-knowns knowns imports mutated)]
      [(and (pair? u)
            (eq? 'assert-ctype-representation (unwrap (car u)))
            (pair? (unwrap (cdr u))))
       (lookup (car (unwrap (cdr u))))]
      [else #f]))
  (define (get-literal stx)
    (match stx
      [`(quote ,v) (known-literal (unwrap v))]
      [`,_
       (define u (unwrap stx))
       (cond
         [(and (not (symbol? u)) (not (pair? u)))
          (known-literal u)]
         [else (lookup stx)])]))
  (define (get-boolean-literal stx)
    (match stx
      [`(lambda . ,_) (known-literal #t)]
      [`(case-lambda . ,_) (known-literal #t)]
      [`,_
       (define k (get-literal stx))
       (cond
         [(known-procedure? k) (known-literal #t)]
         [else k])]))
  (define arg-ks (for/list ([arg-type (in-list arg-types)])
                   (lookup arg-type)))
  (define result-k (lookup result-type))
  (and (andmap known-ctype? arg-ks)
       (known-ctype? result-k)
       (let ([abi-k (get-literal abi)]
             [varargs-after-k (get-literal varargs-after)]
             [blocking?-k (get-boolean-literal blocking?)]
             [async-apply-k (get-boolean-literal async-apply)])
         (and (known-literal? abi-k)
              (known-literal? varargs-after-k)
              (known-literal? blocking?-k)
              (known-literal? async-apply-k)
              `(values (ffi-static-call-and-callback-core
                        ,(map known-ctype-rep arg-ks)
                        ,(known-ctype-rep result-k)
                        ',(known-literal-value abi-k)
                        ,(known-literal-value varargs-after-k)
                        ,(and (or (known-literal-value blocking?-k)
                                  (known-literal-value async-apply-k))
                              #t))
                       (list ,@arg-types) ,result-type
                       ,abi ,varargs-after ,blocking? ,async-apply)))))
