#lang scheme/base

(require (for-syntax scheme/base
                     r6rs/private/inline-rules))

(provide define-inliner
         nocheck
         implementation-restriction)

(define-syntax-rule (nocheck v . _)
  v)

(define (implementation-restriction who what)
  (raise
   (make-exn:fail:unsupported
    (format "~a: result is out of range: ~e" who what)
    (current-continuation-marks))))

(define-syntax-rule (define-inliner define-fx numtype? numtype-str)
  (...
   (begin
     (define-syntax define-an-fx
       (syntax-rules ()
         [(_ orig fx check-result ([(arg ...) (tmp ...)] ...) . rest)
          (begin
            (provide fx)
            (define fx-proc
              (let ([fx (case-lambda 
                         [(arg ...)
                          (unless (numtype? arg)
                            (raise-type-error 'fx numtype-str arg))
                          ...
                          (let ([r (orig arg ...)])
                            (check-result r (implementation-restriction 'fx r)))]
                         ...
                         . rest)])
                fx))
            (define-syntax fx
              (inline-rules
               fx-proc
               [(_ arg ...)
                (let ([tmp arg] ...)
                  (if (and (numtype? tmp) ...)
                      (let ([v (orig tmp ...)])
                        (check-result v (fx-proc tmp ...)))
                      (fx-proc tmp ...)))]
               ...)))]))

     (define-syntax define-an-fx+rest
       (syntax-rules ()
         [(_ orig fx check clauses)
          (define-an-fx orig fx check clauses
            [args (for-each (lambda (arg)
                              (unless (numtype? arg)
                                (raise-type-error 'fx numtype-str arg)))
                            args)
                  (let ([r (apply orig args)])
                    (check r (implementation-restriction 'fx r))
                    r)])]))


     (define-syntax define-fx
       (syntax-rules (...)
         [(_ orig fx [(a) (b c)] check)
          (define-an-fx orig fx check
            ([(a) (t1)]
             [(b c) (t1 t2)]))]
         [(_ orig fx [(a) (b c (... ...))] check)
          (define-an-fx+rest orig fx check
            ([(a) (t1)]
             [(b c) (t1 t2)]))]
         [(_ orig fx (a b c (... ...)) check)
          (define-an-fx+rest orig fx check
            ([(a b) (t1 t2)]))]
         [(_ orig fx (a b (... ...)) check)
          (define-an-fx+rest orig fx check
            ([(a) (t1)]
             [(a b) (t1 t2)]
             [(a b c) (t1 t2 t3)]))]
         [(_ orig fx (a) check)
          (define-an-fx+rest orig fx check
            ([(a) (t1)]))]
         [(_ orig fx (a b) check)
          (define-an-fx orig fx check
            ([(a b) (t1 t2)]))]
         [(_ orig fx (a b c) check)
          (define-an-fx orig fx check
            ([(a b c) (t1 t2 t3)]))])))))
