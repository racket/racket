#lang scheme/base

(require (for-syntax scheme/base
                     r6rs/private/identifier-syntax)
         r6rs/private/qq-gen
         r6rs/private/conds
         (prefix-in r5rs: r5rs)
         (only-in r6rs/private/readtable rx:number)
         scheme/bool)

(provide 

 ;; 11.2
 (rename-out [r6rs:define define]
             [r6rs:define-syntax define-syntax])

 ;; 11.4.1
 (rename-out [r5rs:quote quote])

 ;; 11.4.2
 (rename-out [r5rs:lambda lambda])

 ;; 11.4.3
 (rename-out [r5rs:if if])

 ;; 11.4.4
 set!

 ;; 11.4.5
 cond else => case
 and or
 
 ;; 11.4.6
 let let*
 (rename-out [r5rs:letrec letrec]
             [letrec letrec*])
 let-values let*-values
 
 ;; 11.4.7
 begin

 ;; 11.5
 eqv? eq? equal?

 ;; 11.6
 procedure?
 
 ;; 11.7.4
 number? complex?
 (rename-out [r6rs:real? real?]
             [r6rs:rational? rational?]
             [r6rs:integer? integer?]
             [real? real-valued?]
             [rational? rational-valued?]
             [integer? integer-valued?])
 exact? inexact?
 (rename-out [inexact->exact exact]
             [exact->inexact inexact])
 = < > <= >=
 zero? positive? negative? odd?
 even? finite? infinite? nan?
 min max
 + * - /
 abs 
 div-and-mod div mod
 div0-and-mod0 div0 mod0
 gcd lcm
 numerator denominator
 floor ceiling truncate round
 rationalize
 exp log sin cos tan asin acos atan
 sqrt (rename-out [integer-sqrt/remainder exact-integer-sqrt])
 expt
 make-rectangular make-polar real-part imag-part magnitude angle
 (rename-out [r6rs:number->string number->string]
             [r6rs:string->number string->number])

 ;; 11.8
 not boolean?

 ;; 11.9
 (rename-out [r5rs:pair? pair?]
             [r5rs:cons cons]
             [r5rs:car car]
             [r5rs:cdr cdr]
             [r5rs:caar caar]
             [r5rs:cadr cadr]
             [r5rs:cdar cdar]
             [r5rs:cddr cddr]
             [r5rs:caaar caaar]
             [r5rs:caadr caadr]
             [r5rs:cadar cadar]
             [r5rs:caddr caddr]
             [r5rs:cdaar cdaar]
             [r5rs:cdadr cdadr]
             [r5rs:cddar cddar]
             [r5rs:cdddr cdddr]
             [r5rs:caaaar caaaar]
             [r5rs:caaadr caaadr]
             [r5rs:caadar caadar]
             [r5rs:caaddr caaddr]
             [r5rs:cadaar cadaar]
             [r5rs:cadadr cadadr]
             [r5rs:caddar caddar]
             [r5rs:cadddr cadddr]
             [r5rs:cdaaar cdaaar]
             [r5rs:cdaadr cdaadr]
             [r5rs:cdadar cdadar]
             [r5rs:cdaddr cdaddr]
             [r5rs:cddaar cddaar]
             [r5rs:cddadr cddadr]
             [r5rs:cdddar cdddar]
             [r5rs:cddddr cddddr]
             [r5rs:null? null?]
             [r5rs:list? list?]
             [r5rs:list list]
             [r5rs:length length]
             [r5rs:append append]
             [r5rs:reverse reverse]
             [r5rs:list-tail list-tail]
             [r5rs:list-ref list-ref]
             [r5rs:map map]
             [r5rs:for-each for-each])

 ;; 11.10
 symbol? symbol=?
 string->symbol symbol->string
 
 ;; 11.11
 char? char=? char<? char>? char<=? char>=?
 integer->char char->integer

 ;; 11.12
 string?
 make-string string
 string-length string-ref
 string=? string<? string>? string<=? string>=?
 substring string-append
 (rename-out [r5rs:string->list string->list]
             [r5rs:list->string list->string])
 string-for-each string-copy

 ;; 11.13
 vector? make-vector vector
 vector-length vector-ref vector-set!
 (rename-out [r5rs:vector->list vector->list]
             [r5rs:list->vector list->vector])
 vector-fill! 
 vector-map
 vector-for-each

 ;; 11.14
 (rename-out [r6rs:error error])
 assertion-violation assert

 ;; 11.15
 (rename-out [r5rs:apply apply]
             [r6rs:call/cc call-with-current-continuation]
             [r6rs:call/cc call/cc])
 values call-with-values
 dynamic-wind

 ;; 11.17
 (rename-out [r6rs:quasiquote quasiquote])
 unquote unquote-splicing

 ;; 11.18
 let-syntax letrec-syntax

 ;; 11.19
 (for-syntax syntax-rules
             identifier-syntax
             ...
             _)

 )

;; ----------------------------------------

(define (r6rs:real? n)
  (and (real? n)
       (exact? (imag-part n))))

(define (r6rs:rational? n)
  (and (rational? n)
       (r6rs:real? n)
       (not (and (inexact? n)
                 (or (eqv? n +inf.0)
                     (eqv? n -inf.0)
                     (eqv? n +nan.0))))))

(define (r6rs:integer? n)
  (and (integer? n)
       (r6rs:rational? n)))

(define (finite? n)
  (r6rs:real? n))

(define (infinite? n)
  (or (eqv? n +inf.0)
      (eqv? n -inf.0)))

(define (nan? n)
  (eqv? n +nan.0))

;; Someone needs to look more closely at div and mod.
;; I started with the code from Enger04, and poked it
;; until the results matched the examples in R6RS.

(define (div x y)
  (let ([n (* (numerator x)
              (denominator y))]
        [d (* (denominator x)
              (numerator y))])
    (if (negative? n)
        (- (quotient (- (abs d) n 1) d))
        (quotient n d))))

(define (div0 x y)
  (cond
   [(zero? y) 0]
   [(positive? y)
    (if (negative? x)
        (- (div (- x) y))
        (div x y))]
   [(negative? y)
    (let ([n (* -2
                (numerator x)
                (denominator y))]
          [d (* (denominator x)
                (- (numerator y)))])
      (if (< n d)
          (- (quotient (- d n) (* 2 d)))
          (quotient (+ n d -1) (* 2 d))))]))

(define (mod x y)
  (- x (* (div x y) y)))

(define (div-and-mod x y)
  (let ([d (div x y)])
    (values d (- x (* d y)))))

(define (mod0 x y)
  (- x (* (div0 x y) y)))

(define (div0-and-mod0 x y)
  (let ([d (div0 x y)])
    (values d (- x (* d y)))))

(define (r6rs:number->string z [radix 10] [precision #f])
  (number->string z radix))

(define (r6rs:string->number s [radix 10])
  (and (regexp-match? rx:number s)
       (string->number (regexp-replace* #rx"[|][0-9]+" s "") radix)))

(define-syntax-rule (make-mapper what for for-each in-val val-length val->list)
  (case-lambda
   [(proc val) (for ([c (in-val val)])
                 (proc c))]
   [(proc val1 val2) 
    (if (= (val-length val1)
           (val-length val2))
        (for ([c1 (in-val val1)]
              [c2 (in-val val2)])
          (proc c1 c2))
        (error 'val-for-each "~as have different lengths: ~e and: ~e"
               what
               val1 val2))]
   [(proc val1 . vals)
    (let ([len (val-length val1)])
      (for-each (lambda (s)
                  (unless (= (val-length s) len)
                    (error 'val-for-each "~a have different lengths: ~e and: ~e"
                           what
                           val1 s)))
                vals)
      (apply for-each 
             proc 
             (val->list val1)
             (map val->list vals)))]))

(define string-for-each
  (make-mapper "string" for for-each in-string string-length string->list))

(define vector-for-each
  (make-mapper "vector" for for-each in-vector vector-length vector->list))

(define vector-map
  (make-mapper "vector" for/list map in-vector vector-length vector->list))


(define (r6rs:error who msg . irritants)
  (raise
   (make-exn:fail:r6rs
    (format "~a: ~a" who msg)
    (current-continuation-marks)
    who
    irritants)))

(define (assertion-violation who msg . irritants)
  (raise
   (make-exn:fail:r6rs
    (format "~a: ~a" who msg)
    (current-continuation-marks)
    who
    irritants)))

(define-syntax-rule (assert expr)
  (unless expr
    (assertion-violation #f "assertion failed")))

;; ----------------------------------------
;; quasiquote generalization

(define-generalized-qq r6rs:quasiquote 
  quasiquote unquote unquote-splicing)

;; ----------------------------------------
;; define

(define-syntax (r6rs:define stx)
  (syntax-case stx ()
    [(_ id) 
     (identifier? #'id)
     #'(define id (void))]
    [(_ . rest) #'(r5rs:define . rest)]))

;; ----------------------------------------
;; define-syntax: wrap a transformer to
;;  ensure that the result of an expansion is
;;  a wrapped syntax object.

(define-syntax (r6rs:define-syntax stx)
  (syntax-case stx ()
    [(_ id expr)
     (identifier? #'id)
     (syntax/loc stx
       (define-syntax id (wrap-as-needed expr)))]))

(define-for-syntax (wrap r stx)
  (cond
   [(syntax? r) r]
   [(symbol? r) (error 'macro
                       "transformer result included a raw symbol: ~e"
                       r)]
   [(mpair? r) (datum->syntax
                stx
                (cons (wrap (mcar r) stx)
                      (wrap (mcdr r) stx))
                stx)]
   [(vector? r) (datum->syntax
                 stx
                 (list->vector
                  (map (lambda (r) (wrap r stx))
                       (vector->list r)))
                 stx)]
   [else (datum->syntax stx r stx)]))

(define-for-syntax (wrap-as-needed v)
  (if (and (procedure? v)
           (procedure-arity-includes? v 1))
      (procedure-reduce-arity
       (case-lambda 
        [(stx) (if (syntax? stx)
                   (let ([r (v stx)])
                     (wrap r stx))
                   (v stx))]
        [args (apply v args)])
       (procedure-arity v))
      v))

;; ----------------------------------------

(define detect-tail-key (gensym))

(define (mk-k full-k tag)
  (lambda args 
    (if (continuation-prompt-available? tag)
        (abort-current-continuation
         tag
         (lambda () (apply values args)))
        (apply full-k args))))

(define (r6rs:call/cc f)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 1))
    ;; let call/cc report the error:
    (call/cc f))
  ;; To support call/cc-based jumps in exception
  ;;  handlers, we both grab a continuation and set a prompt
  (let/cc k
    (let ([v (make-continuation-prompt-tag 'r6rs:call/cc)]
          [orig-key (continuation-mark-set-first #f detect-tail-key)])
      (with-continuation-mark detect-tail-key v
        (let ([new-key (continuation-mark-set-first #f detect-tail-key)])
          (if (not (eq? new-key orig-key))
              ;; Old mark surived => not tail wrt old call.
              ;; Create an escape continuation to use for
              ;; error escapes. Of course, we rely on the fact
              ;; that continuation marks are not visible to EoPL
              ;; programs.
              (call-with-continuation-prompt
               (lambda ()
                 (f (mk-k k new-key)))
               new-key)
              ;; Old mark replaced => tail wrt old call.
              ;; To preserve tail semantics for all but the first call
              ;; reuse `mark' instead of creating a new escape continuation:
              (with-continuation-mark detect-tail-key orig-key
                (f (mk-k k orig-key)))))))))
