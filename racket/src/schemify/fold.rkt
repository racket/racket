#lang racket/base
(require racket/fixnum
         "literal.rkt"
         "known.rkt")

(provide try-fold-primitive)

(define (try-fold-primitive orig-prim-sym orig-k exps prim-knowns primitives)
  (define prim-sym (if (known-procedure/then-pure/folding-unsafe? orig-k)
                       (known-procedure/then-pure/folding-unsafe-safe orig-k)
                       orig-prim-sym))
  (define k (if (known-procedure/then-pure/folding-unsafe? orig-k)
                (hash-ref prim-knowns prim-sym #f)
                orig-k))
  (define vals (for/list ([exp (in-list exps)])
                 (unwrap-literal exp)))
  (define check-result (limit-check k vals))
  (and check-result
       (let/ec esc
         (call-with-exception-handler
          (lambda (exn)
            (if (exn:fail? exn)
                (esc #f)
                exn))
          (lambda ()
            (define result 
              (apply (hash-ref primitives prim-sym (lambda args (error "missing")))
                     vals))
            (check-result result)
            (list (wrap-literal result)))))))

(define (limit-check k vals)
  (define kind
    (cond
      [(known-procedure/folding/limited? k)
       (known-procedure/folding/limited-kind k)]
      [(known-procedure/has-unsafe/folding/limited? k)
       (known-procedure/has-unsafe/folding/limited-kind k)]
      [else #f]))
  (case kind
    [(#f) void]
    [(expt)
     (and (not (and (= 2 (length vals))
                    (exact-integer? (car vals))
                    (exact-integer? (cadr vals))
                    ((* (integer-length (car vals))
                        (cadr vals))
                     . > . 1000)))
          void)]
    [(fixnum) (and (for/and ([v (in-list vals)])
                     (fixnum-for-every-system? v))
                   (lambda (v)
                     (unless (fixnum-for-every-system? v)
                       (error "result is not a fixnum for every system"))))]
    [else
     (error 'schemify:limited-ok? "unknown limit kind: ~a" k)]))
