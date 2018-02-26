#lang racket/base
(require racket/pretty
         "schemify.rkt"
         "known.rkt")

(define prim-knowns
  ;; Register primitives
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/unsafe/ops)
      (namespace-require 'racket/flonum)
      (namespace-require 'racket/fixnum))
    (for/hasheq ([s (in-list (namespace-mapped-symbols ns))]
                 #:when (with-handlers ([exn:fail? (lambda (x) #f)])
                          (procedure? (eval s ns))))
      (values s a-known-procedure))))

(define (wrap-everywhere p)
  (cond
   [(pair? p)
    (datum->syntax #f (cons (wrap-everywhere (car p))
                            (wrap-everywhere (cdr p))))]
   [else
    (datum->syntax #f p)]))

(define-values (schemified importss-abi exports-info)
  (schemify-linklet `(linklet 
                      ()
                      (x ,#'y [,#'z ,#'ext-z])
                      .
                      ,(map
                        wrap-everywhere
                        '((define-values (struct:s make-s s? s-ref s-set!)
                            (make-struct-type 's #f 2 0 #f))
                          (define-values (y) (make-s (lambda () x) 5))
                          (define-values (x) (lambda () y))
                          (x)
                          (letrec-values ([(loop) (lambda () (loop))]) (loop))
                          (let-values ([(a) 1] [(b) 2]) (list a b))
                          (let-values ([(a b) (values 1 2)]) (list a b))
                          (define-values (done) (z)))))
                    #;
                    (call-with-input-file "regexp.rktl" read)
                    (lambda (old-v new-v)
                      (if (syntax? old-v)
                          (datum->syntax #f new-v old-v)
                          new-v))
                    (lambda (old-v) (syntax->datum (datum->syntax #f old-v)))
                    prim-knowns
                    (lambda args #hasheq())))

(pretty-print schemified)
(pretty-print exports-info)

