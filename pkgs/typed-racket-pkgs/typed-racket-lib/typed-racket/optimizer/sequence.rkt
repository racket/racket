#lang racket/base

(require syntax/parse
         racket/match
         unstable/function
         syntax/parse/experimental/specialize
         (for-template racket/base racket/unsafe/ops)
         "../utils/utils.rkt" "../utils/tc-utils.rkt"
         (rep type-rep)
         (types abbrev type-table utils)
         (optimizer utils logging float))

(provide sequence-opt-expr)


(define-syntax-class/specialize string-expr
  (typed-expr (λ (t) (type-equal? t -String))))
(define-syntax-class/specialize bytes-expr
  (typed-expr (λ (t) (type-equal? t -Bytes))))
(define-syntax-class/specialize list-expr
  (typed-expr (λ (t)
                 (match t
                   [(Listof: _) #t]
                   [(List: _) #t]
                   [_ #f]))))
(define-syntax-class/specialize vector-expr
  (typed-expr (disjoin Vector? HeterogeneousVector?)))

(define-syntax-rule (log-seq-opt opt-label)
  (log-opt opt-label "Sequence type specialization."))

(define-syntax-class make-sequence
  (pattern op:id
    #:when (id-from? #'op 'make-sequence 'racket/private/for)
    #:do [(add-disappeared-use (syntax-local-introduce #'op))]))

(define-syntax-class sequence-opt-expr
  #:commit
  ;; if we're iterating (with the for macros) over something we know is a list,
  ;; we can generate code that would be similar to if in-list had been used
  (pattern (#%plain-app op:make-sequence _ l:list-expr)
    #:do [(log-seq-opt "in-list")]
    #:with opt #'(let ((i l.opt))
                   (values unsafe-car unsafe-cdr i
                           (lambda (x) (not (null? x)))
                           (lambda (x) #t)
                           (lambda (x y) #t))))
  ;; idem for vectors
  (pattern (#%plain-app op:make-sequence _ v:vector-expr)
    #:do [(log-seq-opt "in-vector")]
    #:with opt #'(let* ((i   v.opt)
                        (len (unsafe-vector-length i)))
                   (values (lambda (x) (unsafe-vector-ref i x))
                           (lambda (x) (unsafe-fx+ 1 x))
                           0
                           (lambda (x) (unsafe-fx< x len))
                           (lambda (x) #t)
                           (lambda (x y) #t))))
  ;; and (byte) strings
  (pattern (#%plain-app op:make-sequence _ s:string-expr)
    #:do [(log-seq-opt "in-string")]
    #:with opt #'(let* ((i   s.opt)
                        (len (string-length i)))
                   (values (lambda (x) (string-ref i x))
                           (lambda (x) (unsafe-fx+ 1 x))
                           0
                           (lambda (x) (unsafe-fx< x len))
                           (lambda (x) #t)
                           (lambda (x y) #t))))
  (pattern (#%plain-app op:make-sequence _ s:bytes-expr)
    #:do [(log-seq-opt "in-bytes")]
    #:with opt #'(let* ((i   s.opt)
                        (len (bytes-length i)))
                   (values (lambda (x) (bytes-ref i x))
                           (lambda (x) (unsafe-fx+ 1 x))
                           0
                           (lambda (x) (unsafe-fx< x len))
                           (lambda (x) #t)
                           (lambda (x y) #t))))
  (pattern (#%plain-app op:make-sequence _ s:int-expr) ; one-arg in-range
    #:do [(log-seq-opt "in-range")]
    #:with opt #'(let* ((end s.opt))
                   (values (lambda (x) x)
                           (lambda (x) (unsafe-fx+ 1 x))
                           0
                           (lambda (x) (unsafe-fx< x end))
                           (lambda (x) #t)
                           (lambda (x y) #t)))))
