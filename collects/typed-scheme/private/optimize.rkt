#lang scheme/base

(require syntax/parse (for-template scheme/base scheme/flonum scheme/fixnum scheme/unsafe/ops)
         "../utils/utils.rkt" unstable/match scheme/match unstable/syntax
         (rep type-rep) syntax/id-table racket/dict
         (types abbrev type-table utils subtype))
(provide optimize)

;; for use in match
(define (subtypeof x y)
  (subtype y x))

(define-syntax-class float-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -Flonum subtypeof)) #t] [_ #f])
           #:with opt #'e.opt))

(define-syntax-class int-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -Integer subtypeof)) #t] [_ #f])
           #:with opt #'e.opt))

(define-syntax-class fixnum-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -Fixnum subtypeof)) #t] [_ #f])
           #:with opt #'e.opt))
(define-syntax-class nonzero-fixnum-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -PositiveFixnum type-equal?)) #t]
                    [(tc-result1: (== -NegativeFixnum type-equal?)) #t]
                    [_ #f])
           #:with opt #'e.opt))


;; if the result of an operation is of type float, its non float arguments
;; can be promoted, and we can use unsafe float operations
;; note: none of the unary operations have types where non-float arguments
;;  can result in float (as opposed to real) results
(define-syntax-class float-arg-expr
  (pattern e:int-opt-expr
           #:with opt #'(->fl e.opt))
  (pattern e:float-opt-expr
           #:with opt #'e.opt))

(define (mk-unsafe-tbl generic safe-pattern unsafe-pattern)
  (for/fold ([h (make-immutable-free-id-table)]) ([g generic])
    (let ([f (format-id g safe-pattern g)] [u (format-id g unsafe-pattern g)])
      (dict-set (dict-set h g u) f u))))

(define (mk-float-tbl generic)
  (mk-unsafe-tbl generic "fl~a" "unsafe-fl~a"))

(define binary-float-ops 
  (mk-float-tbl (list #'+ #'- #'* #'/ #'min #'max)))
(define binary-float-comps
  (mk-float-tbl (list #'= #'<= #'< #'> #'>=)))
(define unary-float-ops
  (mk-float-tbl (list #'abs #'sin #'cos #'tan #'asin #'acos #'atan #'log #'exp
                      #'sqrt #'round #'floor #'ceiling #'truncate)))

(define-syntax-class (float-op tbl)
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (dict-ref tbl #'i)))


(define (mk-fixnum-tbl generic)
  (mk-unsafe-tbl generic "fx~a" "unsafe-fx~a"))

;; due to undefined behavior when results are out of the fixnum range, only some
;; fixnum operations can be optimized
;; the following must be closed on fixnums
(define binary-fixnum-ops
  (dict-set
   (dict-set
    (dict-set
     (dict-set
      (dict-set
       (dict-set
        (mk-fixnum-tbl (list #'= #'<= #'< #'> #'>= #'min #'max))
        #'bitwise-and #'unsafe-fxand)
       #'fxand #'unsafe-fxand)
      #'bitwise-ior #'unsafe-fxior)
     #'fxior #'unsafe-fxior)
    #'bitwise-xor #'unsafe-fxxor)
   #'fxxor #'unsafe-fxxor))
(define-syntax-class fixnum-unary-op
  (pattern (~or (~literal bitwise-not) (~literal fxnot)) #:with unsafe #'unsafe-fxnot)
  (pattern (~or (~literal abs)         (~literal fxabs)) #:with unsafe #'unsafe-fxabs))
;; closed on fixnums, but 2nd argument must not be 0
(define-syntax-class nonzero-fixnum-binary-op
  (pattern (~or (~literal quotient)  (~literal fxquotient))  #:with unsafe #'unsafe-fxquotient)
  (pattern (~or (~literal modulo)    (~literal fxmodulo))    #:with unsafe #'unsafe-fxmodulo)
  (pattern (~or (~literal remainder) (~literal fxremainder)) #:with unsafe #'unsafe-fxremainder))

(define-syntax-class (fixnum-op tbl)
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (dict-ref tbl #'i)))


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

(define-syntax-class opt-expr
  (pattern e:opt-expr*
           #:with opt (syntax-recertify #'e.opt this-syntax (current-code-inspector) #f)))

(define *log-optimizations?* #f)
(define *log-optimizatons-to-log-file?* #f)
(define *optimization-log-file* "opt-log")
(define (log-optimization kind stx)
  (if *log-optimizations?*
      (printf "~a line ~a col ~a - ~a - ~a\n"
              (syntax-source stx) (syntax-line stx) (syntax-column stx)
              (syntax->datum stx)
              kind)
      #t))

;; unlike their safe counterparts, unsafe binary operators can only take 2 arguments
(define (n-ary->binary op arg1 arg2 rest)
  (for/fold ([o arg1])
      ([e (syntax->list #`(#,arg2 #,@rest))])
    #`(#,op #,o #,e)))

(define-syntax-class opt-expr*
  #:literal-sets (kernel-literals)

  ;; interesting cases, where something is optimized
  (pattern (#%plain-app (~var op (float-op unary-float-ops)) f:float-opt-expr)
           #:with opt
           (begin (log-optimization "unary float" #'op)
                  #'(op.unsafe f.opt)))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-ops)) f1:float-arg-expr f2:float-arg-expr fs:float-arg-expr ...))
           #:when (match (type-of #'res)
                    ;; if the result is a float, we can coerce integers to floats and optimize
                    [(tc-result1: (== -Flonum subtypeof)) #t] [_ #f])
           #:with opt
           (begin (log-optimization "binary float" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-comps)) f1:float-opt-expr f2:float-opt-expr fs:float-opt-expr ...))
           #:when (match (type-of #'res)
                    [(tc-result1: (== -Boolean type-equal?)) #t] [_ #f])
           #:with opt
           (begin (log-optimization "binary float comp" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))

  (pattern (#%plain-app op:fixnum-unary-op n:fixnum-opt-expr)
           #:with opt
           (begin (log-optimization "unary fixnum" #'op)
                  #'(op.unsafe n.opt)))
  (pattern (#%plain-app (~var op (fixnum-op binary-fixnum-ops)) n1:fixnum-opt-expr n2:fixnum-opt-expr ns:fixnum-opt-expr ...)
           #:with opt
           (begin (log-optimization "binary fixnum" #'op)
                  (n-ary->binary #'op.unsafe #'n1.opt #'n2.opt #'(ns.opt ...))))
  (pattern (#%plain-app op:nonzero-fixnum-binary-op n1:fixnum-opt-expr n2:nonzero-fixnum-opt-expr)
           #:with opt
           (begin (log-optimization "binary nonzero fixnum" #'op)
                  #'(op.unsafe n1.opt n2.opt)))
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

(define (optimize stx)
  (let ((port (if (and *log-optimizations?*
                       *log-optimizatons-to-log-file?*)
                  (open-output-file *optimization-log-file*
                                    #:exists 'append)
                  (current-output-port))))
    (begin0
      (parameterize ([current-output-port port])
        (syntax-parse stx #:literal-sets (kernel-literals)
                      [e:opt-expr
                       (syntax/loc stx e.opt)]))
      (if (and *log-optimizations?*
               *log-optimizatons-to-log-file?*)
          (close-output-port port)
          #t))))
