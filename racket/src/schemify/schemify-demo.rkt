#lang racket/base
(require racket/pretty
         (only-in racket/linklet
                  datum->correlated
                  correlated?
                  correlated-e)
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
      (values s (known-procedure (procedure-arity-mask (eval s ns)))))))

(define (wrap p)
  (cond
    [(and (pair? p)
          (eq? (car p) 'define-values))
     ;; expander doesn't use a correalted for id list, so avoid
     ;; adding one here
     (list (car p) (map wrap (cadr p)) (map wrap (cddr p)))]
    [(list? p)
     (datum->correlated (map wrap p))]
    [(pair? p)
     (cons (wrap (car p)) (wrap (cdr p)))]
    [else
     (datum->correlated p)]))

(define (unwrap p)
  (cond
    [(correlated? p) (unwrap (correlated-e p))]
    [(pair? p) (cons (unwrap (car p)) (unwrap (cdr p)))]
    [else p]))

(define-values (schemified importss exports import-keys imports-abis exports-info)
  (schemify-linklet `(linklet 
                      ()
                      (x y [z ext-z])
                       .
                      ,(map
                        wrap
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
                    #t ; serializable
                    #t ; datum-intern?
                    #f ; for-jitify?
                    #f ; allow-set!-undefined?
                    #f ; unsafe-mode?
                    #t ; enforce-constant?
                    #t ; allow-inline?
                    #f ; no-prompt?
                    prim-knowns
                    #f
                    #f))

(pretty-print (unwrap schemified))
(pretty-print exports-info)

