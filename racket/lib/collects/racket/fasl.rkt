#lang racket/base

(provide s-exp->fasl
         fasl->s-exp)

(define (s-exp->fasl v [out #f])
  (when out
    (unless (output-port? out)
      (raise-argument-error 'fasl->s-exp "(or/c output-port? #f)" out)))
  (let ([p (or out
               (open-output-bytes))])
    (parameterize ([current-namespace (make-base-namespace)])
      (write (compile `(quote ,v)) p))
    (if out
        (void)
        (get-output-bytes p))))

(define (fasl->s-exp b)
  (unless (or (bytes? b)
              (input-port? b))
    (raise-arguments-error 'fasl->s-exp "(or/c bytes? input-port?)" b))
  (let ([p (if (bytes? b)
               (open-input-bytes b)
               b)])
    (let ([e (parameterize ([read-accept-compiled #t])
               (read p))])
      (if (compiled-expression? e)
          (parameterize ([current-namespace (make-base-namespace)])
            (eval e))
          e))))
