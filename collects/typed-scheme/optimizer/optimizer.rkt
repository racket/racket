#lang scheme/base

(require syntax/parse 
         syntax/id-table racket/dict
         unstable/match scheme/match
         (for-template scheme/base scheme/flonum scheme/fixnum scheme/unsafe/ops racket/private/for)
         "../utils/utils.rkt" "../utils/tc-utils.rkt"
         (rep type-rep)
         (types abbrev type-table utils subtype)
         (optimizer utils fixnum float inexact-complex))

(provide optimize-top)


(define-syntax-class pair-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e) ; type of the operand
                    [(tc-result1: (Pair: _ _)) #t]
                    [_ #f])
           #:with opt #'e.opt))

(define-syntax-class pair-unary-op
  (pattern (~literal car) #:with unsafe #'unsafe-car)
  (pattern (~literal cdr) #:with unsafe #'unsafe-cdr))

(define-syntax-class vector-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (HeterogenousVector: _)) #t]
                    [_ #f])
           #:with opt #'e.opt))

(define-syntax-class vector-op
  ;; we need the * versions of these unsafe operations to be chaperone-safe
  (pattern (~literal vector-ref)  #:with unsafe #'unsafe-vector*-ref)
  (pattern (~literal vector-set!) #:with unsafe #'unsafe-vector*-set!))

(define-syntax-class list-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (Listof: _)) #t]
                    [(tc-result1: (List: _)) #t]
                    [_ #f])
           #:with opt #'e.opt))


(define-syntax-class opt-expr
  (pattern e:opt-expr*
           #:with opt (syntax-recertify #'e.opt this-syntax (current-code-inspector) #f)))


(define-syntax-class opt-expr*
  #:literal-sets (kernel-literals)

  ;; interesting cases, where something is optimized
  (pattern (~and res (#%plain-app (~var op (float-op unary-float-ops)) f:float-opt-expr))
           #:when (subtypeof? #'res -Flonum)
           #:with opt
           (begin (log-optimization "unary float" #'op)
                  #'(op.unsafe f.opt)))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-ops))
                                  f1:float-arg-expr
                                  f2:float-arg-expr
                                  fs:float-arg-expr ...))
           ;; if the result is a float, we can coerce integers to floats and optimize
           #:when (subtypeof? #'res -Flonum)
           #:with opt
           (begin (log-optimization "binary float" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-comps))
                                  f1:float-opt-expr
                                  f2:float-opt-expr
                                  fs:float-opt-expr ...))
           #:with opt
           (begin (log-optimization "binary float comp" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))

  (pattern (#%plain-app op:fixnum-unary-op n:fixnum-opt-expr)
           #:with opt
           (begin (log-optimization "unary fixnum" #'op)
                  #'(op.unsafe n.opt)))
  (pattern (#%plain-app (~var op (fixnum-op binary-fixnum-ops))
                        n1:fixnum-opt-expr
                        n2:fixnum-opt-expr
                        ns:fixnum-opt-expr ...)
           #:with opt
           (begin (log-optimization "binary fixnum" #'op)
                  (n-ary->binary #'op.unsafe #'n1.opt #'n2.opt #'(ns.opt ...))))
  (pattern (#%plain-app op:nonzero-fixnum-binary-op
                        n1:fixnum-opt-expr
                        n2:nonzero-fixnum-opt-expr)
           #:with opt
           (begin (log-optimization "binary nonzero fixnum" #'op)
                  #'(op.unsafe n1.opt n2.opt)))

  (pattern (#%plain-app op:inexact-complex-unary-op n:inexact-complex-opt-expr)
           #:with opt
           (begin (log-optimization "unary inexact complex" #'op)
                  #'(op.unsafe n.opt)))
  (pattern (~and exp (#%plain-app (~var op (float-op binary-inexact-complex-ops))
                                  e:inexact-complex-opt-expr ...))
           #:with exp*:unboxed-inexact-complex-opt-expr #'exp
           #:with opt
           (begin (log-optimization "unboxed inexact complex" #'exp)
                  (begin (reset-unboxed-gensym)
                         #'(let* (exp*.bindings ...)
                             (unsafe-make-flrectangular exp*.real-part exp*.imag-part)))))
  
  (pattern (#%plain-app (~and op (~literal exact->inexact)) n:fixnum-opt-expr)
           #:with opt
           (begin (log-optimization "fixnum to float" #'op)
                  #'(unsafe-fx->fl n.opt)))
  ;; we can optimize exact->inexact if we know we're giving it an Integer
  (pattern (#%plain-app (~and op (~literal exact->inexact)) n:int-opt-expr)
           #:with opt
           (begin (log-optimization "int to float" #'op)
                  #'(->fl n.opt)))
  ;; we can get rid of it altogether if we're giving it an inexact number
  (pattern (#%plain-app (~and op (~literal exact->inexact)) f:float-opt-expr)
           #:with opt
           (begin (log-optimization "float to float" #'op)
                  #'f.opt))

  (pattern (#%plain-app op:pair-unary-op p:pair-opt-expr)
           #:with opt
           (begin (log-optimization "unary pair" #'op)
                  #'(op.unsafe p.opt)))

  ;; vector-length of a known-length vector
  (pattern (#%plain-app (~and op (~or (~literal vector-length)
                                      (~literal unsafe-vector-length)
                                      (~literal unsafe-vector*-length)))
                        v:vector-opt-expr)
           #:with opt
           (begin (log-optimization "known-length vector" #'op)
                  (match (type-of #'v)
                    [(tc-result1: (HeterogenousVector: es))
                     #`(begin v.opt #,(length es))]))) ; v may have side effects
  ;; we can optimize vector-length on all vectors.
  ;; since the program typechecked, we know the arg is a vector.
  ;; we can optimize no matter what.
  (pattern (#%plain-app (~and op (~literal vector-length)) v:opt-expr)
           #:with opt
           (begin (log-optimization "vector" #'op)
                  #'(unsafe-vector*-length v.opt)))
  ;; same for flvector-length
  (pattern (#%plain-app (~and op (~literal flvector-length)) v:opt-expr)
           #:with opt
           (begin (log-optimization "flvector" #'op)
                  #'(unsafe-flvector-length v.opt)))
  ;; we can optimize vector ref and set! on vectors of known length if we know
  ;; the index is within bounds (for now, literal or singleton type)
  (pattern (#%plain-app op:vector-op v:vector-opt-expr i:opt-expr new:opt-expr ...)
           #:when (let ((len (match (type-of #'v)
                               [(tc-result1: (HeterogenousVector: es)) (length es)]
                               [_ 0]))
                        (ival (or (syntax-parse #'i [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                                  (match (type-of #'i)
                                    [(tc-result1: (Value: (? number? i))) i]
                                    [_ #f]))))
                    (and (integer? ival) (exact? ival) (<= 0 ival (sub1 len))))
           #:with opt
           (begin (log-optimization "vector" #'op)
                  #'(op.unsafe v.opt i.opt new.opt ...)))

  ;; if we're iterating (with the for macros) over something we know is a list,
  ;; we can generate code that would be similar to if in-list had been used
  (pattern (#%plain-app op:id _ l)
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with l*:list-opt-expr #'l
           #:with opt
           (begin (log-optimization "in-list" #'op)
                  #'(let ((i l*.opt))
                      (values unsafe-car unsafe-cdr i
                              (lambda (x) (not (null? x)))
                              (lambda (x) #t)
                              (lambda (x y) #t)))))

  ;; we can always optimize struct accessors and mutators
  ;; if they typecheck, they're safe
  (pattern (#%plain-app op:id s:opt-expr v:opt-expr ...)
           #:when (or (struct-accessor? #'op) (struct-mutator? #'op))
           #:with opt
           (let ([idx (struct-fn-idx #'op)])
             (if (struct-accessor? #'op)
                 (begin (log-optimization "struct ref" #'op)
                        #`(unsafe-struct-ref  s.opt #,idx))
                 (begin (log-optimization "struct set" #'op)
                        #`(unsafe-struct-set! s.opt #,idx v.opt ...)))))

  ;; boring cases, just recur down
  (pattern (#%plain-lambda formals e:opt-expr ...)
           #:with opt #'(#%plain-lambda formals e.opt ...))
  (pattern (define-values formals e:opt-expr ...)
           #:with opt #'(define-values formals e.opt ...))
  (pattern (case-lambda [formals e:opt-expr ...] ...)
           #:with opt #'(case-lambda [formals e.opt ...] ...))
  (pattern (let-values ([ids e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt #'(let-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (letrec-values ([ids e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt #'(letrec-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (letrec-syntaxes+values stx-bindings ([(ids ...) e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt #'(letrec-syntaxes+values stx-bindings ([(ids ...) e-rhs.opt] ...) e-body.opt ...))
  (pattern (kw:identifier expr ...)
           #:when (ormap (lambda (k) (free-identifier=? k #'kw))
                         (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app #'#%expression
                               #'#%variable-reference #'with-continuation-mark))
           #:with (expr*:opt-expr ...) #'(expr ...) ; we don't want to optimize in the cases that don't match the #:when clause
           #:with opt #'(kw expr*.opt ...))
  (pattern other:expr
           #:with opt #'other))

(define (optimize-top stx)
  (let ((port (if (and *log-optimizations?*
                       *log-optimizatons-to-log-file?*)
                  (open-output-file *optimization-log-file*
                                    #:exists 'append)
                  (current-output-port))))
    (begin0
      (parameterize ([current-output-port port]
                     [optimize (lambda (stx)
                                 (syntax-parse stx #:literal-sets (kernel-literals)
                                               [e:opt-expr
                                                (syntax/loc stx e.opt)]))])
        ((optimize) stx))
      (if (and *log-optimizations?*
               *log-optimizatons-to-log-file?*)
          (close-output-port port)
          #t))))
