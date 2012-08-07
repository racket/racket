#lang racket/base

(require syntax/parse racket/match
         "../utils/utils.rkt"
         (utils tc-utils)
         (rep type-rep)
         (types abbrev utils type-table)
         (optimizer utils logging)
         (for-template racket/base racket/unsafe/ops))

(provide list-opt-expr)

(define-syntax-class known-length-list-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (List: es)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class list-op
  #:commit
  (pattern (~literal list-ref)  #:with unsafe #'unsafe-list-ref)
  (pattern (~literal list-tail) #:with unsafe #'unsafe-list-tail))


(define-syntax-class list-opt-expr
  #:commit
  ;; Similar to known-length vectors opts.
  ;; If we use `list-ref' or `list-tail' on a known-length list with a
  ;; literal index, we can optimize if the index is within bounds.
  (pattern (#%plain-app op:list-op l:known-length-list-expr i:expr)
           #:when (let ((len (match (type-of #'l)
                               [(tc-result1: (List: es)) (length es)]
                               [_ 0])) ; can't happen
                        (ival (or (syntax-parse #'i
                                    [((~literal quote) i:number)
                                     (syntax-e #'i)]
                                    [_ #f])
                                  (match (type-of #'i)
                                    [(tc-result1: (Value: (? fixnum? i))) i]
                                    [_ -1])))) ; sure to fail the next check
                    (<= 0 ival (sub1 len)))
           #:with opt
           (begin (log-optimization "known-length list op"
                                    "List access specialization."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  #`(op.unsafe l.opt #,((optimize) #'i))))
  ;; We know the length of known-length lists statically.
  (pattern (#%plain-app (~and op (~literal length)) l:known-length-list-expr)
           #:with opt
           (begin (log-optimization "known-length list length"
                                    "Static list length computation."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  (match (type-of #'l)
                    [(tc-result1: (List: es))
                     #`(begin l.opt #,(length es))]))))
