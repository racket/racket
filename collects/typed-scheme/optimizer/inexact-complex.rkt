#lang scheme/base

(require syntax/parse
         "../utils/utils.rkt"
         (for-template scheme/base scheme/flonum scheme/unsafe/ops)
         (types abbrev type-table utils subtype)
         (optimizer utils float))

(provide (all-defined-out))


(define-syntax-class inexact-complex-opt-expr
  (pattern e:expr
           #:when (isoftype? #'e -InexactComplex)
           #:with opt ((optimize) #'e)))

;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-inexact-complex-opt-expr
  (pattern (#%plain-app (~and (~var op (float-op binary-inexact-complex-ops)) (~or (~literal +) (~literal -)))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-part (unboxed-gensym)
           #:with imag-part (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(#,@(append (syntax->list #'(c1.bindings ... c2.bindings ... cs.bindings ... ...))
                                (list #`(real-part #,(for/fold ((o #'c1.real-part))
                                                         ((e (syntax->list #'(c2.real-part cs.real-part ...))))
                                                       #`(op.unsafe #,o #,e)))
                                      #`(imag-part #,(for/fold ((o #'c1.imag-part))
                                                         ((e (syntax->list #'(c2.imag-part cs.imag-part ...))))
                                                       #`(op.unsafe #,o #,e))))))))
  (pattern (#%plain-app (~and op (~literal *))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-part (unboxed-gensym)
           #:with imag-part (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-part and imag-part
                     #,@(let loop ([o1 #'c1.real-part]
                                   [o2 #'c1.imag-part]
                                   [e1 (syntax->list #'(c2.real-part cs.real-part ...))]
                                   [e2 (syntax->list #'(c2.imag-part cs.imag-part ...))]
                                   [rs (append (map (lambda (x) (unboxed-gensym))
                                                    (syntax->list #'(cs.real-part ...)))
                                               (list #'real-part))]
                                   [is (append (map (lambda (x) (unboxed-gensym))
                                                    (syntax->list #'(cs.imag-part ...)))
                                               (list #'imag-part))]
                                   [res '()])
                          (if (null? e1)
                              (reverse res)
                              (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr rs) (cdr is)
                                    ;; complex multiplication, imag part, then real part (reverse)
                                    (list* #`(#,(car is)
                                              (unsafe-fl+ (unsafe-fl* #,o2 #,(car e1))
                                                          (unsafe-fl* #,o1 #,(car e2))))
                                           #`(#,(car rs)
                                              (unsafe-fl- (unsafe-fl* #,o1 #,(car e1))
                                                          (unsafe-fl* #,o2 #,(car e2))))
                                           res)))))))
  (pattern (#%plain-app (~and op (~literal /))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-part (unboxed-gensym)
           #:with imag-part (unboxed-gensym)
           #:with (denominators ...)
           (for/list
            ([e1 (syntax->list #'(c2.real-part cs.real-part ...))]
             [e2 (syntax->list #'(c2.imag-part cs.imag-part ...))])
            #`(#,(unboxed-gensym) (unsafe-fl+ (unsafe-fl* #,e1 #,e1) (unsafe-fl* #,e2 #,e2))))
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ... denominators ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-part and imag-part
                     #,@(let loop ([o1 #'c1.real-part]
                                   [o2 #'c1.imag-part]
                                   [e1 (syntax->list #'(c2.real-part cs.real-part ...))]
                                   [e2 (syntax->list #'(c2.imag-part cs.imag-part ...))]
                                   [d  (map (lambda (x) (car (syntax-e x)))
                                            (syntax->list #'(denominators ...)))]
                                   [rs (append (map (lambda (x) (unboxed-gensym))
                                                    (syntax->list #'(cs.real-part ...)))
                                               (list #'real-part))]
                                   [is (append (map (lambda (x) (unboxed-gensym))
                                                    (syntax->list #'(cs.imag-part ...)))
                                               (list #'imag-part))]
                                   [res '()])
                          (if (null? e1)
                              (reverse res)
                              (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr d) (cdr rs) (cdr is)
                                    ;; complex division, imag part, then real part (reverse)
                                    (list* #`(#,(car is)
                                              (unsafe-fl/ (unsafe-fl- (unsafe-fl* #,o2 #,(car e1))
                                                                      (unsafe-fl* #,o1 #,(car e2)))
                                                          #,(car d)))
                                           #`(#,(car rs)
                                              (unsafe-fl/ (unsafe-fl+ (unsafe-fl* #,o1 #,(car e1))
                                                                      (unsafe-fl* #,o2 #,(car e2)))
                                                          #,(car d)))
                                           res)))))))
  (pattern e:expr
           ;; can't work on inexact reals, which are a subtype of inexact
           ;; complexes, so this has to be equality
           #:when (isoftype? #'e -InexactComplex)
           #:with e* (unboxed-gensym)
           #:with real-part (unboxed-gensym)
           #:with imag-part (unboxed-gensym)
           #:with (bindings ...)
           #`((e* #,((optimize) #'e))
              (real-part (unsafe-flreal-part e*))
              (imag-part (unsafe-flimag-part e*)))))

(define-syntax-class inexact-complex-unary-op
  (pattern (~or (~literal real-part) (~literal flreal-part)) #:with unsafe #'unsafe-flreal-part)
  (pattern (~or (~literal imag-part) (~literal flimag-part)) #:with unsafe #'unsafe-flimag-part))
(define binary-inexact-complex-ops
  (mk-float-tbl (list #'+ #'- #'* #'/)))


(define (optimize-inexact-complex-expr e)
  (syntax-parse e #:literal-sets (kernel-literals)
                [e:inexact-complex-opt-expr
                 (syntax/loc stx e.opt)]))
