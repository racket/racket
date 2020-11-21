#lang racket/base
(require racket/fasl
         racket/unsafe/undefined
         racket/extflonum)

(provide fasl-literal?
         fasl-literals
         unfasl-literals/lazy
         force-unfasl-literals)

(define (fasl-literal? q need-exposed?)
  (cond
    [(impersonator? q) #t] ; i.e., strip impersonators when serializaing
    [(path? q) #t]
    [(regexp? q) #t]
    [(srcloc? q) #t]
    [(byte-regexp? q) #t]
    [(keyword? q) #t]
    [(hash? q) #t]
    [(string? q) #t] ; to intern
    [(bytes? q) #t] ; to intern
    [(prefab-struct-key q) #t] ; to intern
    [(need-exposed? q) #t] ; to expose to full linklet directory
    ;; No case for numbers, because they are historically not interned
    ;; on bytecode read, but extflonums are special
    [(extflonum? q) #t]
    ;; Assume that anything else can be handled; rejecting
    ;; non-serializable values is someone else's problem
    [else #f]))

(struct to-unfasl (bstr externals wrt))

(define (empty-literals? v)
  (and (vector? v)
       (eqv? 0 (vector-length v))))

(define (fasl-literals v need-exposed?)
  (cond
    [(empty-literals? v) v]
    [else
     (define exposed '())
     (define bstr (s-exp->fasl v
                               #:skip-prefix? #t
                               #:handle-fail cannot-fasl
                               ;; We have to keep uninterned symbols exposed, so they're
                               ;; fasled with the encloding linklet directory
                               #:external-lift? (lambda (v)
                                                  (and (need-exposed? v)
                                                       (begin
                                                         (set! exposed (cons v exposed))
                                                         #t)))))
     (if (null? exposed)
         bstr
         (cons bstr (list->vector (reverse exposed))))]))

(define (unfasl-literals/lazy v)
  (cond
    [(empty-literals? v) v]
    [else
     (box (to-unfasl (if (pair? v) (car v) v)
                     (if (pair? v) (cdr v) '#())
                     (current-load-relative-directory)))]))

(define (force-unfasl-literals b)
  (cond
    [(box? b)
     (define v (unbox b))
     (cond
       [(to-unfasl? v)
        (define new-v
          (parameterize ([current-load-relative-directory (to-unfasl-wrt v)])
            (fasl->s-exp (to-unfasl-bstr v)
                         #:datum-intern? #t
                         #:skip-prefix? #t
                         #:external-lifts (to-unfasl-externals v))))
        (let loop ()
          (cond
            [(box-cas! b v new-v)
             new-v]
            [else
             (let ([v (unbox b)])
               (cond
                 [(to-unfasl? v)
                  ;; must be a spurious CAS failure
                  (loop)]
                 [else
                  ;; other thread beat us to it
                  v]))]))]
       [else v])]
    [else b]))

(define (cannot-fasl v)
  (error 'write
         "cannot marshal value that is embedded in compiled code\n  value: ~v"
         v))
