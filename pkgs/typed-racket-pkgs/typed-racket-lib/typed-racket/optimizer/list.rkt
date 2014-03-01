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
  #:attributes (opt len)
  (pattern (~and e :opt-expr)
           #:attr tys (match (type-of #'e)
                        [(tc-result1: (List: es)) es]
                        [_ #f])
           #:when (attribute tys)
           #:attr len (length (attribute tys))))

(define-unsafe-syntax-class list-ref)
(define-unsafe-syntax-class list-tail)
(define-literal-syntax-class length)

(define-merged-syntax-class list-op (list-ref^ list-tail^))


(define-syntax-class list-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  ;; Similar to known-length vectors opts.
  ;; If we use `list-ref' or `list-tail' on a known-length list with a
  ;; literal index, we can optimize if the index is within bounds.
  (pattern (#%plain-app op:list-op l:known-length-list-expr i:value-expr)
    #:when (<= 0 (attribute i.val) (sub1 (attribute l.len)))
    #:do [(log-opt "known-length list op" "List access specialization.")]
    #:with opt #'(op.unsafe l.opt i.opt))
  ;; We know the length of known-length lists statically.
  (pattern (#%plain-app op:length^ l:known-length-list-expr)
    #:do [(log-opt "known-length list length" "Static list length computation.")]
    #:with opt #`(let () l.opt #,(attribute l.len))))
