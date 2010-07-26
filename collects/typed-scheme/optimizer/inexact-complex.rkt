#lang scheme/base

(require syntax/parse syntax/id-table scheme/dict
         "../utils/utils.rkt"
         (for-template scheme/base scheme/math scheme/flonum scheme/unsafe/ops)
         (types abbrev type-table utils subtype)
         (optimizer utils float fixnum))

(provide inexact-complex-opt-expr inexact-complex-arith-opt-expr
         unboxed-inexact-complex-opt-expr unboxed-vars-table)


;; contains the bindings which actually exist as separate bindings for each component
;; associates identifiers to lists (real-binding imag-binding)
(define unboxed-vars-table (make-free-id-table))

;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-inexact-complex-opt-expr
  
  (pattern (#%plain-app (~and op (~literal +))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-binding (unboxed-gensym)
           #:with imag-binding (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(#,@(append (syntax->list #'(c1.bindings ... c2.bindings ... cs.bindings ... ...))
                                (let ()
                                   ;; we can skip the real parts of imaginaries (#f) and vice versa
                                   (define (skip-0s l)
                                     (let ((l (filter syntax->datum (syntax->list l))))
                                       (case (length l)
                                         ((0) #'0.0)
                                         ((1) (car l))
                                         (else
                                          (for/fold ((o (car l)))
                                              ((e (cdr l)))
                                            #`(unsafe-fl+ #,o #,e))))))
                                   (list
                                    #`(real-binding #,(skip-0s #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                                    #`(imag-binding #,(skip-0s #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)))))))))
  
  (pattern (#%plain-app (~and op (~literal -))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-binding (unboxed-gensym)
           #:with imag-binding (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(#,@(append (syntax->list #'(c1.bindings ... c2.bindings ... cs.bindings ... ...))
                                (let ()
                                  ;; unlike addition, we simply can't skip real parts of imaginaries
                                  (define (skip-0s l)
                                    (let* ((l1 (map (lambda (x) (if (syntax->datum x) x #'0.0))
                                                    (syntax->list l)))
                                           ;; but we can skip all but the first 0
                                           (l2 (filter (lambda (x) (not (equal? (syntax->datum x) 0.0)))
                                                       (cdr l1))))
                                      (case (length l2)
                                        ((0) (car l1))
                                        (else
                                         (for/fold ((o (car l1)))
                                             ((e l2))
                                           #`(unsafe-fl- #,o #,e))))))
                                  (list
                                   #`(real-binding #,(skip-0s #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                                   #`(imag-binding #,(skip-0s #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)))))))))
  
  (pattern (#%plain-app (~and op (~literal *))
                        c1:unboxed-inexact-complex-opt-expr
                        c2:unboxed-inexact-complex-opt-expr
                        cs:unboxed-inexact-complex-opt-expr ...)
           #:with real-binding (unboxed-gensym)
           #:with imag-binding (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-binding and imag-binding
                     #,@(let ((lr (map (lambda (x) (if (syntax->datum x) x #'0.0))
                                       (syntax->list #'(c1.real-binding c2.real-binding cs.real-binding ...))))
                              (li (map (lambda (x) (if (syntax->datum x) x #'0.0))
                                       (syntax->list #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)))))
                          (let loop ([o1 (car lr)]
                                     [o2 (car li)]
                                     [e1 (cdr lr)]
                                     [e2 (cdr li)]
                                     [rs (append (map (lambda (x) (unboxed-gensym))
                                                      (syntax->list #'(cs.real-binding ...)))
                                                 (list #'real-binding))]
                                     [is (append (map (lambda (x) (unboxed-gensym))
                                                      (syntax->list #'(cs.imag-binding ...)))
                                                 (list #'imag-binding))]
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
           #:with real-binding (unboxed-gensym)
           #:with imag-binding (unboxed-gensym)
           #:with reals (map (lambda (x) (if (syntax->datum x) x #'0.0))
                             (syntax->list #'(c1.real-binding c2.real-binding cs.real-binding ...)))
           #:with imags (map (lambda (x) (if (syntax->datum x) x #'0.0))
                             (syntax->list #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)))
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary inexact complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-binding and imag-binding
                     #,@(let loop ([o1 (car (syntax->list #'reals))]
                                   [o2 (car (syntax->list #'imags))]
                                   [e1 (cdr (syntax->list #'reals))]
                                   [e2 (cdr (syntax->list #'imags))]
                                   [rs (append (map (lambda (x) (unboxed-gensym))
                                                    (syntax->list #'(cs.real-binding ...)))
                                               (list #'real-binding))]
                                   [is (append (map (lambda (x) (unboxed-gensym))
                                                    (syntax->list #'(cs.imag-binding ...)))
                                               (list #'imag-binding))]
                                   [ds (map (lambda (x) (unboxed-gensym))
                                            (syntax->list #'(c2.real-binding cs.real-binding ...)))]
                                   [res '()])
                          (if (null? e1)
                              (reverse res)
                              (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr rs) (cdr is) (cdr ds)
                                    ;; complex division, imag part, real part, then denominator (reverse)
                                    (let ((o-real? (equal? (syntax->datum o2) 0.0))
                                          (e-real? (equal? (syntax->datum (car e2)) 0.0)))
                                      (cond [(and o-real? e-real?)
                                             (list*
                                              #`(#,(car is) 0.0) ; currently not propagated
                                              #`(#,(car rs) (unsafe-fl/ #,o1 #,(car e1)))
                                              res)]
                                            [o-real?
                                             (list*
                                              #`(#,(car is)
                                                 (unsafe-fl/ (unsafe-fl- 0.0
                                                                         (unsafe-fl* #,o1 #,(car e2)))
                                                             #,(car ds)))
                                              #`(#,(car rs) (unsafe-fl/ (unsafe-fl* #,o1 #,(car e1))
                                                                        #,(car ds)))
                                              #`(#,(car ds) (unsafe-fl+ (unsafe-fl* #,(car e1) #,(car e1))
                                                                        (unsafe-fl* #,(car e2) #,(car e2))))
                                              res)]
                                            [e-real?
                                             (list*
                                              #`(#,(car is) (unsafe-fl/ #,o2 #,(car e1)))
                                              #`(#,(car rs) (unsafe-fl/ #,o1 #,(car e1)))
                                              res)]
                                            [else
                                             (list*
                                              #`(#,(car is)
                                                 (unsafe-fl/ (unsafe-fl- (unsafe-fl* #,o2 #,(car e1))
                                                                         (unsafe-fl* #,o1 #,(car e2)))
                                                             #,(car ds)))
                                              #`(#,(car rs)
                                                 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* #,o1 #,(car e1))
                                                                         (unsafe-fl* #,o2 #,(car e2)))
                                                             #,(car ds)))
                                              #`(#,(car ds)
                                                 (unsafe-fl+ (unsafe-fl* #,(car e1) #,(car e1))
                                                             (unsafe-fl* #,(car e2) #,(car e2))))
                                              res)]))))))))

  (pattern (#%plain-app (~and op (~literal conjugate)) c:unboxed-inexact-complex-opt-expr)
           #:with real-binding #'c.real-binding
           #:with imag-binding (unboxed-gensym)
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary inexact complex" #'op)
                  #`(#,@(append (syntax->list #'(c.bindings ...))
                                (list #'(imag-binding (unsafe-fl- 0.0 c.imag-binding)))))))

  (pattern (#%plain-app (~and op (~or (~literal real-part) (~literal unsafe-flreal-part)))
                        c:unboxed-inexact-complex-opt-expr)
           #:with real-binding #'c.real-binding
           #:with imag-binding #f
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary inexact complex" #'op)
                  #'(c.bindings ...)))
  (pattern (#%plain-app (~and op (~or (~literal imag-part) (~literal unsafe-flimag-part)))
                        c:unboxed-inexact-complex-opt-expr)
           #:with real-binding #'c.imag-binding
           #:with imag-binding #f
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary inexact complex" #'op)
                  #'(c.bindings ...)))

  ;; if we see a variable that's already unboxed, use the unboxed bindings
  (pattern v:id
           #:with unboxed-info (dict-ref unboxed-vars-table #'v #f)
           #:when (syntax->datum #'unboxed-info)
           #:with real-binding (car  (syntax->list #'unboxed-info))
           #:with imag-binding (cadr (syntax->list #'unboxed-info))
           #:with (bindings ...) #'())
  
  ;; else, do the unboxing here  
  (pattern e:expr
           #:when (isoftype? #'e -InexactComplex)
           #:with e* (unboxed-gensym)
           #:with real-binding (unboxed-gensym)
           #:with imag-binding (unboxed-gensym)
           #:with (bindings ...)
           #`((e* #,((optimize) #'e))
              (real-binding (unsafe-flreal-part e*))
              (imag-binding (unsafe-flimag-part e*))))
  ;; special handling of reals
  (pattern e:float-expr
           #:with real-binding (unboxed-gensym)
           #:with imag-binding #f
           #:with (bindings ...)
           #`((real-binding #,((optimize) #'e))))
  (pattern e:fixnum-expr
           #:with real-binding (unboxed-gensym)
           #:with imag-binding #f
           #:with (bindings ...)
           #`((real-binding (unsafe-fx->fl #,((optimize) #'e)))))
  (pattern e:int-expr
           #:with real-binding (unboxed-gensym)
           #:with imag-binding #f
           #:with (bindings ...)
           #`((real-binding (->fl #,((optimize) #'e)))))
  (pattern e:expr
           #:when (isoftype? #'e -Real)
           #:with real-binding (unboxed-gensym)
           #:with imag-binding #f
           #:with (bindings ...)
           #`((real-binding (exact->inexact #,((optimize) #'e)))))
  (pattern e:expr
           #:when (isoftype? #'e -Number) ; complex, maybe exact, maybe not
           #:with e* (unboxed-gensym)
           #:with real-binding (unboxed-gensym)
           #:with imag-binding (unboxed-gensym)
           #:with (bindings ...)
           #`((e* #,((optimize) #'e))
              (real-binding (exact->inexact (real-part e*)))
              (imag-binding (exact->inexact (imag-part e*)))))
  (pattern e:expr
           #:with (bindings ...)
           (error "non exhaustive pattern match")
           #:with real-binding #f
           #:with imag-binding #f))

(define-syntax-class inexact-complex-unary-op
  (pattern (~or (~literal real-part) (~literal flreal-part)) #:with unsafe #'unsafe-flreal-part)
  (pattern (~or (~literal imag-part) (~literal flimag-part)) #:with unsafe #'unsafe-flimag-part))

(define-syntax-class inexact-complex-op
  (pattern (~or (~literal +) (~literal -) (~literal *) (~literal /) (~literal conjugate))))

(define-syntax-class inexact-complex-expr
  (pattern e:expr
           #:when (isoftype? #'e -InexactComplex)
           #:with opt ((optimize) #'e)))

(define-syntax-class inexact-complex-opt-expr

  ;; we can optimize taking the real of imag part of an unboxed complex
  ;; hopefully, the compiler can eliminate unused bindings for the other part if it's not used
  (pattern (#%plain-app (~and op (~or (~literal real-part) (~literal unsafe-flreal-part)
                                      (~literal imag-part) (~literal unsafe-flimag-part)))
                        c:inexact-complex-expr)
           #:with c*:inexact-complex-arith-opt-expr #'c
           #:with opt
           (begin (log-optimization "unboxed inexact complex" #'op)
                  (reset-unboxed-gensym)
                  #`(let* (c*.bindings ...)
                      #,(if (or (free-identifier=? #'op #'real-part)
                                (free-identifier=? #'op #'unsafe-flreal-part))
                            #'c*.real-binding
                            #'c*.imag-binding))))
  
  (pattern (#%plain-app op:inexact-complex-unary-op n:inexact-complex-expr)
           #:with opt
           (begin (log-optimization "unary inexact complex" #'op)
                  #'(op.unsafe n.opt)))
  (pattern e:inexact-complex-arith-opt-expr
           #:with opt
           #'e.opt))

(define-syntax-class inexact-complex-arith-opt-expr
  (pattern (~and exp (#%plain-app op:inexact-complex-op e:expr ...))
           #:when (isoftype? #'exp -InexactComplex)
           #:with exp*:unboxed-inexact-complex-opt-expr #'exp
           #:with real-binding #'exp*.real-binding
           #:with imag-binding #'exp*.imag-binding
           #:with (bindings ...) #'(exp*.bindings ...)
           #:with opt
           (begin (log-optimization "unboxed inexact complex" #'exp)
                  (reset-unboxed-gensym)
                  #'(let* (exp*.bindings ...)
                      (unsafe-make-flrectangular exp*.real-binding exp*.imag-binding)))))
