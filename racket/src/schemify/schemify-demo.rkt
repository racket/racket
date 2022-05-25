#lang racket/base
(require racket/pretty
         (only-in racket/linklet
                  datum->correlated
                  correlated?
                  correlated-e)
         "schemify.rkt"
         "known.rkt")

(define-values (prim-knowns primitives)
  ;; Register primitives
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/unsafe/ops)
      (namespace-require 'racket/flonum)
      (namespace-require 'racket/fixnum))
    (define base-primitives
      (for/hasheq ([s (in-list (namespace-mapped-symbols ns))]
                   #:when (with-handlers ([exn:fail? (lambda (x) #f)])
                            (procedure? (eval s ns))))
        (values s (eval s ns))))
    (define primitives (let* ([ht base-primitives]
                              [ht (hash-set ht 'eof eof)]
                              [ht (hash-set ht 'null null)])
                         ht))
    (values
     (for/hasheq ([(s v) (in-hash primitives)])
       (cond
         [(procedure? v)
          (define a (procedure-arity-mask v))
          (values s (case s
                      [(+ - * / integer->char char->integer void)
                       (known-procedure/folding a)]
                      [(fx+ fxlshift)
                       (known-procedure/folding/limited a 'fixnum)]
                      [(expt arithmetic-shift)
                       (known-procedure/folding/limited a 'expt)]
                      [(unsafe-fx+)
                       (known-procedure/then-pure/folding-unsafe a 'fx+)]
                      [else
                       (known-procedure a)]))]
         [else
          (values s (known-literal v))]))
     primitives)))

(define (wrap p)
  p
  #;
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
                      (x y [z ext-z] w c1 c2)
                       .
                      ,(map
                        wrap
                        '((define-values (struct:s make-s s? s-ref s-set!)
                            (make-struct-type 's #f 2 0 #f))
                          (define-values (y) (make-s (lambda () x) 5))
                          (define-values (x) (lambda () y))
                          (x)
                          (define-values (w) (case-lambda [() (+ 1 7)] [(a) x]))
                          (letrec-values ([(loop) (lambda () (loop))]) (loop))
                          (let-values ([(a) 1] [(b) 2]) (list a b))
                          (let-values ([(a b) (values 1 (+ 2 3))])
                            (list a
                                  b
                                  (arithmetic-shift 3 1000)
                                  (fx+ 4 5) (fx+ 4 (expt 2 40)) (fx* (fxlshift 1 20) (fxlshift 1 20))
                                  (unsafe-fx+ 4 5) (unsafe-fx+ 4 (expt 2 40))
                                  (integer->char 48)
                                  (char->integer '#\1)
                                  (void (void) eof-object null)))
                          (define-values (adds-unsafe) (lambda (x)
                                                         (list (unsafe-fx+ x 1)
                                                               (unsafe-fx+ x 2))))
                          (define-values (adds-safe) (lambda (x)
                                                       (list (fx+ x 1)
                                                             (unsafe-fx+ x 2))))
                          (define-values (adds-still-unsafe) (lambda (x)
                                                               (list (unsafe-fx+ x 1)
                                                                     (fx+ x 2))))
                          (define-values (done) (z))
                          (define-values (call) (lambda () (values 'c1 'c2)))
                          (define-values (c1 c2) (call)))))
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
                    primitives
                    #f
                    #f))

(pretty-print (unwrap schemified))
(pretty-print exports-info)

