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
     (define-syntax (define-an-fx stx)
       (syntax-case stx ()
         [(_ orig fx binary-op check-result base-body ([(arg ...) (tmp ...)] ...) . rest)
          (with-syntax ([(extra-clauses ...)
                         (if (syntax-e #'binary-op)
                             #'([(_ arg1 arg2) (binary-op arg1 arg2)])
                             #'())]
                        [(base-clause ...)
                         (if (syntax-e #'base-body)
                             #'([() . base-body])
                             #'())])
            #'(begin
                (provide fx)
                (define fx-proc
                  (let ([fx (case-lambda
                             base-clause ...
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
                   extra-clauses ...
                   [(_ arg ...)
                    (let ([tmp arg] ...)
                      (if (and (numtype? tmp) ...)
                          (let ([v (orig tmp ...)])
                            (check-result v (fx-proc tmp ...)))
                          (fx-proc tmp ...)))]
                   ...))))]))

     (define-syntax define-an-fx+rest
       (syntax-rules ()
         [(_ orig fx binary-op check base-body clauses)
          (define-an-fx orig fx binary-op check base-body clauses
            [args (for-each (lambda (arg)
                              (unless (numtype? arg)
                                (raise-type-error 'fx numtype-str arg)))
                            args)
                  (let ([r (apply orig args)])
                    (check r (implementation-restriction 'fx r))
                    r)])]))


     (define-syntax define-fx
       (syntax-rules (...)
         [(_ orig fx binary-op [(a) (b c)] check)
          (define-an-fx orig fx binary-op check #f
            ([(a) (t1)]
             [(b c) (t1 t2)]))]
         [(_ orig fx binary-op [(a) (b c (... ...))] check)
          (define-an-fx+rest orig fx binary-op check #f
            ([(a) (t1)]
             [(b c) (t1 t2)]))]
         [(_ orig fx binary-op (a b c (... ...)) check)
          (define-an-fx+rest orig fx binary-op check #f
            ([(a b) (t1 t2)]))]
         [(_ orig fx binary-op (a b (... ...)) check)
          (define-an-fx+rest orig fx binary-op check #f
            ([(a) (t1)]
             [(a b) (t1 t2)]
             [(a b c) (t1 t2 t3)]))]
         [(_ orig fx binary-op #:base base (a b (... ...)) check)
          (define-an-fx+rest orig fx binary-op check (base)
            ([(a) (t1)]
             [(a b) (t1 t2)]
             [(a b c) (t1 t2 t3)]))]
         [(_ orig fx binary-op (a) check)
          (define-an-fx+rest orig fx binary-op check #f
            ([(a) (t1)]))]
         [(_ orig fx binary-op (a b) check)
          (define-an-fx orig fx binary-op check #f
            ([(a b) (t1 t2)]))]
         [(_ orig fx binary-op (a b c) check)
          (define-an-fx orig fx binary-op check #f
            ([(a b c) (t1 t2 t3)]))])))))
