#lang racket/base

(require syntax/parse
         racket/match
         (for-template racket/base racket/unsafe/ops)
         "../utils/utils.rkt" "../utils/tc-utils.rkt"
         (rep type-rep)
         (types abbrev type-table utils)
         (optimizer utils logging string
                    float)) ; for int-expr

(provide sequence-opt-expr)


(define-syntax-class list-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (Listof: _)) #t]
                    [(tc-result1: (List: _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

;; unlike other vector optimizations, this works on unknown-length vectors
(define-syntax-class vector-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (Vector: _)) #t]
                    [(tc-result1: (HeterogeneousVector: _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define seq-opt-msg "Sequence type specialization.")

(define-syntax-class sequence-opt-expr
  #:commit
  ;; if we're iterating (with the for macros) over something we know is a list,
  ;; we can generate code that would be similar to if in-list had been used
  (pattern (#%plain-app op:id _ l)
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with l*:list-expr #'l
           #:with opt
           (begin (log-optimization "in-list" seq-opt-msg this-syntax)
                  #'(let ((i l*.opt))
                      (values unsafe-car unsafe-cdr i
                              (lambda (x) (not (null? x)))
                              (lambda (x) #t)
                              (lambda (x y) #t)))))
  ;; idem for vectors
  (pattern (#%plain-app op:id _ v)
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with v*:vector-expr #'v
           #:with opt
           (begin (log-optimization "in-vector" seq-opt-msg this-syntax)
                  #'(let* ((i   v*.opt)
                           (len (unsafe-vector-length i)))
                      (values (lambda (x) (unsafe-vector-ref i x))
                              (lambda (x) (unsafe-fx+ 1 x))
                              0
                              (lambda (x) (unsafe-fx< x len))
                              (lambda (x) #t)
                              (lambda (x y) #t)))))
  ;; and (byte) strings
  (pattern (#%plain-app op:id _ s)
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with s*:string-expr #'s
           #:with opt
           (begin (log-optimization "in-string" seq-opt-msg this-syntax)
                  #'(let* ((i   s*.opt)
                           (len (string-length i)))
                      (values (lambda (x) (string-ref i x))
                              (lambda (x) (unsafe-fx+ 1 x))
                              0
                              (lambda (x) (unsafe-fx< x len))
                              (lambda (x) #t)
                              (lambda (x y) #t)))))
  (pattern (#%plain-app op:id _ s)
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with s*:bytes-expr #'s
           #:with opt
           (begin (log-optimization "in-bytes" seq-opt-msg this-syntax)
                  #'(let* ((i   s*.opt)
                           (len (bytes-length i)))
                      (values (lambda (x) (bytes-ref i x))
                              (lambda (x) (unsafe-fx+ 1 x))
                              0
                              (lambda (x) (unsafe-fx< x len))
                              (lambda (x) #t)
                              (lambda (x y) #t)))))
  (pattern (#%plain-app op:id _ s) ; one-arg in-range
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with s*:int-expr #'s
           #:with opt
           (begin (log-optimization "in-range" seq-opt-msg this-syntax)
                  #'(let* ((end s*.opt))
                      (values (lambda (x) x)
                              (lambda (x) (unsafe-fx+ 1 x))
                              0
                              (lambda (x) (unsafe-fx< x end))
                              (lambda (x) #t)
                              (lambda (x y) #t))))))
