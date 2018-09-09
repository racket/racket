#lang racket/base
(require "match.rkt")

(provide add-args
         extract-rest-arg
         lambda-arity
         lambda-no-rest-args?
         args-length
         compatible-args?)

(define (add-args env ids)
  (cond
    [(null? ids) env]
    [(symbol? ids) (hash-set env ids #t)]
    [else (add-args (hash-set env (car ids) #t)
                    (cdr ids))]))

(define (extract-rest-arg ids)
  (if (pair? ids)
      (extract-rest-arg (cdr ids))
      ids))

(define (lambda-arity e #:precise-cases? [precise-cases? #f])
  (match e
    [`(lambda ,ids . ,_)
     (define min-a (args-length ids))
     (if (list? ids)
         (values min-a min-a)
         (values min-a -1))]
    [`(case-lambda)
     (if precise-cases?
         (values -1 "\"\"")
         (values 0 0))]
    [`(case-lambda [,unsorted-idss . ,_] ...)
     (cond
       [precise-cases?
        ;; Get full arity to record for arity reporting
        (define idss unsorted-idss)
        (define (bytes->string-constant s)
          ;; Drop the leading `#`:
          (substring (format "~s" s) 1))
        (define (encode big-endian?)
          (bytes->string-constant 
           (apply bytes-append
                  ;; Encode individual arities as pairs of `int`s:
                  (for/list ([ids (in-list idss)])
                    (define-values (min-a max-a) (lambda-arity `(lambda ,ids)))
                    (bytes-append (integer->integer-bytes min-a 4 #t big-endian?)
                                  (integer->integer-bytes max-a 4 #t big-endian?))))))
        (values (- (+ (length idss) 1))
                (format "c_SELECT_LITTLE_BIG_ENDIAN(~a, ~a)" (encode #f) (encode #t)))]
       [else
        ;; Get approximate arity for predictions about calls
        (define idss (sort unsorted-idss < #:key args-length))
        (define-values (min-a max-a) (lambda-arity `(lambda ,(car idss))))
        (let loop ([min-a min-a] [max-a max-a] [idss (cdr idss)])
          (cond
            [(null? idss) (values min-a max-a)]
            [else
             (define-values (new-min-a new-max-a) (lambda-arity `(lambda ,(car idss))))
             (loop (min min-a new-min-a)
                   (if (or (= max-a -1) (= new-max-a -1))
                       -1
                       (max max-a new-max-a))
                   (cdr idss))]))])]))

(define (lambda-no-rest-args? e)
  (match e
    [`(lambda ,ids . ,_) (list? ids)]
    [`(case-lambda [,idss . ,_] ...)
     (for/and ([ids (in-list idss)])
       (list? ids))]))

(define (args-length ids)
  (if (pair? ids) (add1 (args-length (cdr ids))) 0))

(define (compatible-args? n e)
  (match e
    [`(lambda ,ids . ,_) (= n (args-length ids))]
    [`(case-lambda [,idss . ,_] ...)
     (for/or ([ids (in-list idss)])
       (= n (args-length ids)))]))
