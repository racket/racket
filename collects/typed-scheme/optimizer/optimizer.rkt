#lang scheme/base

(require syntax/parse (for-template scheme/base scheme/flonum scheme/fixnum scheme/unsafe/ops racket/private/for)
         "../utils/utils.rkt" "../utils/tc-utils.rkt" unstable/match scheme/match unstable/syntax unstable/values
         (rep type-rep) syntax/id-table racket/dict
         (types abbrev type-table utils subtype))
(provide optimize)

;; is the syntax object s's type a subtype of t?
(define (subtypeof s t)
  (match (type-of s)
    [(tc-result1: (== t (lambda (x y) (subtype y x)))) #t] [_ #f]))


(define-syntax-class float-opt-expr
  (pattern e:opt-expr
           #:when (subtypeof #'e -Flonum)
           #:with opt #'e.opt))
(define-syntax-class int-opt-expr
  (pattern e:opt-expr
           #:when (subtypeof #'e -Integer)
           #:with opt #'e.opt))

;; if the result of an operation is of type float, its non float arguments
;; can be promoted, and we can use unsafe float operations
;; note: none of the unary operations have types where non-float arguments
;;  can result in float (as opposed to real) results
(define-syntax-class float-arg-expr
  (pattern e:fixnum-opt-expr
           #:with opt #'(unsafe-fx->fl e.opt))
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
  (dict-set
   (dict-set
    (mk-float-tbl (list #'= #'<= #'< #'> #'>=))
    ;; not a comparison, but takes 2 floats and does not return a float,
    ;; unlike binary-float-ops
    #'make-rectangular #'unsafe-make-flrectangular)
   #'make-flrectangular #'unsafe-make-flrectangular))
(define unary-float-ops
  (mk-float-tbl (list #'abs #'sin #'cos #'tan #'asin #'acos #'atan #'log #'exp
                      #'sqrt #'round #'floor #'ceiling #'truncate)))

(define-syntax-class (float-op tbl)
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (dict-ref tbl #'i)))

;; to generate temporary symbols in a predictable manner
;; these identifiers are unique within a sequence of unboxed operations
;; necessary to have predictable symbols to add in the hand-optimized versions
;; of the optimizer tests (which check for equality of expanded code)
(define *unboxed-gensym-counter* 0)
(define (unboxed-gensym)
  (set! *unboxed-gensym-counter* (add1 *unboxed-gensym-counter*))
  (format-unique-id #'here "unboxed-gensym-~a" *unboxed-gensym-counter*))

(define-syntax-class inexact-complex-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -InexactComplex type-equal?)) #t] [_ #f])

           #:with opt #'e.opt))
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
  (pattern e:opt-expr
           ;; can't work on inexact reals, which are a subtype of inexact
           ;; complexes, so this has to be equality
           #:when (match (type-of #'e)
                    [(tc-result1: (== -InexactComplex type-equal?)) #t] [_ #f])
           #:with e* (unboxed-gensym)
           #:with real-part (unboxed-gensym)
           #:with imag-part (unboxed-gensym)
           #:with (bindings ...)
           #'((e* e.opt)
              (real-part (unsafe-flreal-part e*))
              (imag-part (unsafe-flimag-part e*)))))

(define-syntax-class inexact-complex-unary-op
  (pattern (~or (~literal real-part) (~literal flreal-part)) #:with unsafe #'unsafe-flreal-part)
  (pattern (~or (~literal imag-part) (~literal flimag-part)) #:with unsafe #'unsafe-flimag-part))
(define binary-inexact-complex-ops
  (mk-float-tbl (list #'+ #'- #'* #'/)))

(define-syntax-class fixnum-opt-expr
  (pattern e:opt-expr
           #:when (subtypeof #'e -Fixnum)
           #:with opt #'e.opt))
(define-syntax-class nonzero-fixnum-opt-expr
  (pattern e:opt-expr
           #:when (match (type-of #'e)
                    [(tc-result1: (== -PositiveFixnum type-equal?)) #t]
                    [(tc-result1: (== -NegativeFixnum type-equal?)) #t]
                    [_ #f])
           #:with opt #'e.opt))

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
  (pattern (~and res (#%plain-app (~var op (float-op unary-float-ops)) f:float-opt-expr))
           #:when (subtypeof #'res -Flonum)
           #:with opt
           (begin (log-optimization "unary float" #'op)
                  #'(op.unsafe f.opt)))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-ops)) f1:float-arg-expr f2:float-arg-expr fs:float-arg-expr ...))
           ;; if the result is a float, we can coerce integers to floats and optimize
           #:when (subtypeof #'res -Flonum)
           #:with opt
           (begin (log-optimization "binary float" #'op)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  (pattern (~and res (#%plain-app (~var op (float-op binary-float-comps)) f1:float-opt-expr f2:float-opt-expr fs:float-opt-expr ...))
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

  (pattern (#%plain-app op:inexact-complex-unary-op n:inexact-complex-opt-expr)
           #:with opt
           (begin (log-optimization "unary inexact complex" #'op)
                  #'(op.unsafe n.opt)))
  (pattern (~and exp (#%plain-app (~var op (float-op binary-inexact-complex-ops)) e:inexact-complex-opt-expr ...))
           #:with exp*:unboxed-inexact-complex-opt-expr #'exp
           #:with opt
           (begin (log-optimization "unboxed inexact complex" #'exp)
                  (begin (set! *unboxed-gensym-counter* 0)
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
