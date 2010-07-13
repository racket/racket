#lang scheme/base

(require syntax/parse
         "../utils/utils.rkt"
         (for-template scheme/base scheme/math scheme/flonum scheme/unsafe/ops)
         (types abbrev type-table utils subtype)
         (optimizer utils))

(provide inexact-complex-opt-expr)


;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-inexact-complex-opt-expr
  (pattern (#%plain-app (~and op (~literal +))
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
                                                       #`(unsafe-fl+ #,o #,e)))
                                      ;; we can skip the imaginary parts of reals (#f)
                                      #`(imag-part
                                         #,(let ((l (filter syntax->datum
                                                            (syntax->list #'(c1.imag-part c2.imag-part cs.imag-part ...)))))
                                             (case (length l)
                                               ((0) #'0.0)
                                               ((1) (car l))
                                               (else
                                                (for/fold ((o (car l)))
                                                    ((e (cdr l)))
                                                  #`(unsafe-fl+ #,o #,e)))))))))))
  (pattern (#%plain-app (~and op (~literal -))
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
                                                       #`(unsafe-fl- #,o #,e)))
                                      ;; unlike addition, we simply can't skip imaginary parts of reals
                                      #`(imag-part
                                         #,(let* ((l1 (map (lambda (x) (if (syntax->datum x) x #'0.0))
                                                           (syntax->list #'(c1.imag-part c2.imag-part cs.imag-part ...))))
                                                  ;; but we can skip all but the first 0
                                                  (l2 (filter (lambda (x) (not (equal? (syntax->datum x) 0.0)))
                                                              (cdr l1))))
                                             (case (length l2)
                                               ((0) (car l1))
                                               (else
                                                (for/fold ((o (car l1)))
                                                    ((e l2))
                                                  #`(unsafe-fl- #,o #,e)))))))))))
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
                     #,@(let ((l (map (lambda (x) (if (syntax->datum x) x #'0.0))
                                      (syntax->list #'(c1.imag-part c2.imag-part cs.imag-part ...)))))
                          (let loop ([o1 #'c1.real-part]
                                     [o2 (car l)]
                                     [e1 (syntax->list #'(c2.real-part cs.real-part ...))]
                                     [e2 (cdr l)]
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
                                      ;; we eliminate operations on the imaginary parts of reals
                                      (let ((o-real? (equal? (syntax->datum o2) 0.0))
                                            (e-real? (equal? (syntax->datum (car e2)) 0.0)))
                                        (list* #`(#,(car is)
                                                  #,(cond ((and o-real? e-real?) #'0.0)
                                                          (o-real? #`(unsafe-fl* #,o1 #,(car e2)))
                                                          (e-real? #`(unsafe-fl* #,o2 #,(car e1)))
                                                          (else
                                                           #`(unsafe-fl+ (unsafe-fl* #,o2 #,(car e1))
                                                                         (unsafe-fl* #,o1 #,(car e2))))))
                                               #`(#,(car rs)
                                                  #,(cond ((or o-real? e-real?)
                                                           #`(unsafe-fl* #,o1 #,(car e1)))
                                                          (else
                                                           #`(unsafe-fl- (unsafe-fl* #,o1 #,(car e1))
                                                                         (unsafe-fl* #,o2 #,(car e2))))))
                                             res)))))))))
  (pattern (#%plain-app (~and op (~literal /))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-part (unboxed-gensym)
           #:with imag-part (unboxed-gensym)
           #:with reals (syntax->list #'(c1.real-part c2.real-part cs.real-part ...))
           ;; we currently don't skip imaginary parts of reals
           #:with imags (map (lambda (x) (if (syntax->datum x) x #'0.0))
                             (syntax->list #'(c1.imag-part c2.imag-part cs.imag-part ...)))
           #:with (denominators ...)
           (for/list
            ([e1 (cdr (syntax->list #'reals))]
             [e2 (cdr (syntax->list #'imags))])
            #`(#,(unboxed-gensym) (unsafe-fl+ (unsafe-fl* #,e1 #,e1) (unsafe-fl* #,e2 #,e2))))
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ... denominators ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-part and imag-part
                     #,@(let loop ([o1 (car (syntax->list #'reals))]
                                   [o2 (car (syntax->list #'imags))]
                                   [e1 (cdr (syntax->list #'reals))]
                                   [e2 (cdr (syntax->list #'imags))]
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
  (pattern (#%plain-app (~and op (~literal conjugate)) c:unboxed-inexact-complex-opt-expr)
           #:with real-part #'c.real-part
           #:with imag-part (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary inexact complex" #'op)
                  #`(#,@(append (syntax->list #'(c.bindings ...))
                                (list #'(imag-part (unsafe-fl- 0.0 c.imag-part)))))))
  (pattern e:expr
           ;; special handling of inexact reals
           #:when (subtypeof? #'e -Flonum)
           #:with real-part (unboxed-gensym)
           #:with imag-part #f
           #:with (bindings ...)
           #`((real-part #,((optimize) #'e))))
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

(define-syntax-class inexact-complex-binary-op
  (pattern (~or (~literal +) (~literal -) (~literal *) (~literal /) (~literal conjugate))))

(define-syntax-class inexact-complex-expr
  (pattern e:expr
           #:when (subtypeof? #'e -InexactComplex)
           #:with opt ((optimize) #'e)))

(define-syntax-class inexact-complex-opt-expr
  (pattern (#%plain-app op:inexact-complex-unary-op n:inexact-complex-expr)
           #:with opt
           (begin (log-optimization "unary inexact complex" #'op)
                  #'(op.unsafe n.opt)))
  (pattern (~and exp (#%plain-app op:inexact-complex-binary-op e:inexact-complex-expr ...))
           #:when (isoftype? #'exp -InexactComplex)
           #:with exp*:unboxed-inexact-complex-opt-expr #'exp
           #:with opt
           (begin (log-optimization "unboxed inexact complex" #'exp)
                  (reset-unboxed-gensym)
                  #'(let* (exp*.bindings ...)
                      (unsafe-make-flrectangular exp*.real-part exp*.imag-part)))))
