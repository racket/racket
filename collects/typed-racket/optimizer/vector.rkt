#lang racket/base

(require syntax/parse unstable/syntax
         racket/match racket/flonum
         (for-template racket/base racket/flonum racket/unsafe/ops)
         "../utils/utils.rkt"
         (utils tc-utils)
         (rep type-rep)
         (types type-table utils numeric-tower)
         (optimizer utils logging fixnum))

(provide vector-opt-expr)


(define-syntax-class vector-op
  #:commit
  ;; we need the non-* versions of these unsafe operations to be chaperone-safe
  (pattern (~literal vector-ref)  #:with unsafe #'unsafe-vector-ref  #:with unsafe-no-impersonator #'unsafe-vector*-ref)
  (pattern (~literal vector-set!) #:with unsafe #'unsafe-vector-set! #:with unsafe-no-impersonator #'unsafe-vector*-set!))
(define-syntax-class flvector-op
  #:commit
  (pattern (~literal flvector-ref)  #:with unsafe #'unsafe-flvector-ref)
  (pattern (~literal flvector-set!) #:with unsafe #'unsafe-flvector-set!))

(define-syntax-class known-length-vector-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (HeterogeneousVector: _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class vector-opt-expr
  #:commit
  ;; vector-length of a known-length vector
  (pattern (#%plain-app (~and op (~or (~literal vector-length)
                                      (~literal unsafe-vector-length)
                                      (~literal unsafe-vector*-length)))
                        v:known-length-vector-expr)
           #:with opt
           (begin (log-optimization "known-length vector-length"
                                    "Static vector length computation."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  (match (type-of #'v)
                    [(tc-result1: (HeterogeneousVector: es))
                     #`(begin v.opt #,(length es))]))) ; v may have side effects
  ;; we can optimize vector-length on all vectors.
  ;; since the program typechecked, we know the arg is a vector.
  ;; we can optimize no matter what.
  (pattern (#%plain-app (~and op (~literal vector-length)) v:expr)
           #:with opt
           (begin (log-optimization "vector-length" "Vector check elimination." this-syntax)
                  (add-disappeared-use #'op)
                  #`(unsafe-vector-length #,((optimize) #'v))))
  ;; same for flvector-length
  (pattern (#%plain-app (~and op (~literal flvector-length)) v:expr)
           #:with opt
           (begin (log-optimization "flvector-length" "Float vector check elimination." this-syntax)
                  (add-disappeared-use #'op)
                  #`(unsafe-flvector-length #,((optimize) #'v))))
  ;; we can optimize vector ref and set! on vectors of known length if we know
  ;; the index is within bounds (for now, literal or singleton type)
  (pattern (#%plain-app op:vector-op v:known-length-vector-expr i:expr new:expr ...)
           #:when (let ((len (match (type-of #'v)
                               [(tc-result1: (HeterogeneousVector: es)) (length es)]
                               [_ 0]))
                        (ival (or (syntax-parse #'i [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                                  (match (type-of #'i)
                                    [(tc-result1: (Value: (? number? i))) i]
                                    [_ #f]))))
                    (and (integer? ival) (exact? ival) (<= 0 ival (sub1 len))))
           #:with opt
           (begin (log-optimization "vector" "Vector bounds checking elimination." this-syntax)
                  (add-disappeared-use #'op)
                  #`(op.unsafe v.opt #,((optimize) #'i)
                               #,@(syntax-map (optimize) #'(new ...)))))

  ;; we can do the bounds checking separately, to eliminate some of the checks
  (pattern (#%plain-app op:vector-op v:expr i:fixnum-expr new:expr ...)
           #:with opt
           (begin (log-optimization "vector partial bounds checking elimination"
                                    "Partial bounds checking elimination."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  (let ([safe-fallback #`(op new-v new-i #,@(syntax-map (optimize) #'(new ...)))]
                        [i-known-nonneg? (subtypeof? #'i -NonNegFixnum)])
                    #`(let ([new-i #,((optimize) #'i)]
                            [new-v #,((optimize) #'v)])
                        ;; do the impersonator check up front, to avoid doing it twice (length and op)
                        (if (impersonator? new-v)
                            (if #,(let ([one-sided #'(unsafe-fx< new-i (unsafe-vector*-length new-v))])
                                    (if i-known-nonneg?
                                        ;; we know it's nonnegative, one-sided check
                                        one-sided
                                        #`(and (unsafe-fx>= new-i 0)
                                               #,one-sided)))
                                (op.unsafe new-v new-i #,@(syntax-map (optimize) #'(new ...)))
                                #,safe-fallback) ; will error. to give the right error message
                            ;; not an impersonator, can use unsafe-vector* ops
                            (if #,(let ([one-sided #'(unsafe-fx< new-i (unsafe-vector-length new-v))])
                                    (if i-known-nonneg?
                                        one-sided
                                        #`(and (unsafe-fx>= new-i 0)
                                               #,one-sided)))
                                (op.unsafe-no-impersonator new-v new-i #,@(syntax-map (optimize) #'(new ...)))
                                #,safe-fallback))))))
  ;; similarly for flvectors
  (pattern (#%plain-app op:flvector-op v:expr i:fixnum-expr new:expr ...)
           #:with opt
           (begin (log-optimization "flvector partial bounds checking elimination"
                                    "Partial bounds checking elimination."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  (let ([safe-fallback #`(op new-v new-i #,@(syntax-map (optimize) #'(new ...)))]
                        [i-known-nonneg? (subtypeof? #'i -NonNegFixnum)])
                    #`(let ([new-i #,((optimize) #'i)]
                            [new-v #,((optimize) #'v)])
                        (if #,(let ([one-sided #'(unsafe-fx< new-i (unsafe-flvector-length new-v))])
                                (if i-known-nonneg?
                                    one-sided
                                    #`(and (unsafe-fx>= new-i 0)
                                           #,one-sided)))
                            (op.unsafe new-v new-i #,@(syntax-map (optimize) #'(new ...)))
                            #,safe-fallback))))))
