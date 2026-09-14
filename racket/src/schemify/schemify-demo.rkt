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
                      (x y [z ext-z] w c1 c2 class2-struct-type-ref)
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
                                  (void (void) eof-object null)
                                  (#%foreign-inline (void) unique)))
                          (let-values ([(i) (#%foreign-inline (get i) copy)])
                            (list i i))
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
                          (define-values (c1 c2) (call))
                          (define-values (struct:class-struct-type make-class-struct-type class-struct-type?
                                                                   class-type-ref
                                                                   class-struct-type-ref)
                            (let-values ([(-struct:class-struct-type -make-class-struct-type -class-struct-type? -class-struct-type-ref)
                                          (make-struct-metatype 'class #f 1)])
                              (values -struct:class-struct-type -make-class-struct-type -class-struct-type?
                                      (make-struct-type-metaaccessor -class-struct-type-ref)
                                      (make-struct-field-accessor -class-struct-type-ref 0))))
                          (define-values (struct:c make-c c? c-ref1 c-ref2)
                            (let-values ([(-struct:c -make-c -c? -c-ref -c-set!)
                                          (make-class-struct-type 'c #f 2 0 #f null 'current #f '(0 1) #f #f 'vtable)])
                              (values -struct:c -make-c -c?
                                      (make-struct-field-accessor -c-ref 0)
                                      (make-struct-field-accessor -c-ref 1))))
                          (define-values (class-ref)
                            (lambda (o)
                              (list (class-struct-type? o)
                                    (class-struct-type-ref o))))
                          (define-values (struct:class2-struct-type make-class2-struct-type class2-struct-type?
                                                                    exposed-class2-struct-type-ref
                                                                    class2-type-ref
                                                                    class2-struct-type-ref
                                                                    class2-struct-type-metaref)
                            (let-values ([(-struct:class2-struct-type -make-class2-struct-type -class2-struct-type? -class2-struct-type-ref)
                                          (make-struct-metatype 'class2 struct:class-struct-type 2 'authentic)])
                              (values -struct:class2-struct-type -make-class2-struct-type -class2-struct-type?
                                      -class2-struct-type-ref
                                      (make-struct-type-metaaccessor -class2-struct-type-ref)
                                      (make-struct-field-accessor -class2-struct-type-ref 1)
                                      (make-struct-field-metaaccessor -class2-struct-type-ref 1))))
                          (define-values (class2-t-ref)
                            (lambda (o)
                              (class2-type-ref (list o o) 'ok)))
                          (define-values (class2-t-metaref)
                            (lambda (o)
                              (class2-struct-type-metaref (list o o) 'ok)))
                          (define-values (class2-ref)
                            (lambda (o)
                              (list (class2-struct-type? o)
                                    (class2-struct-type-ref o)))))))
                    #;
                    (call-with-input-file "regexp.rktl" read)
                    #t          ; serializable
                    #t          ; datum-intern?
                    #f          ; target 
                    #f          ; allow-set!-undefined?
                    #f          ; unsafe-mode?
                    #t          ; enforce-constant?
                    #t          ; allow-inline?
                    #f          ; no-prompt?
                    prim-knowns ; hasheq : symbol -> known-procedure (see "known.rkt") 
                    primitives  ; hasheq : symbol -> actual primitive
                    #f          ; compiler-query
                    #f          ; get-import-knowns
                    #f))

(pretty-print (unwrap schemified))
(pretty-print exports-info)

