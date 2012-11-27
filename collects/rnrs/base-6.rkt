#lang scheme/base

(require (for-syntax (rename-in r6rs/private/base-for-syntax
                                [syntax-rules r6rs:syntax-rules])
                     scheme/base)
         scheme/promise
         scheme/splicing
         r6rs/private/qq-gen
         r6rs/private/exns
         r6rs/private/no-set
         (for-syntax r6rs/private/reconstruct)
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
 (rename-out [r6rs:lambda lambda])

 ;; 11.4.3
 (rename-out [r5rs:if if])

 ;; 11.4.4
 (rename-out [r6rs:set! set!])

 ;; 11.4.5
 (rename-out [r5rs:cond cond]
             [r5rs:case case])
 else => 
 and or
 
 ;; 11.4.6
 let let*
 (rename-out [r6rs:letrec letrec]
             [r6rs:letrec* letrec*]
             [r6rs:let-values let-values]
             [r6rs:let*-values let*-values])
 
 ;; 11.4.7
 begin

 ;; 11.5
 eqv? eq? equal?

 ;; 11.6
 procedure?
 
 ;; 11.7.4
 number? complex? real? rational? integer?
 real-valued? rational-valued? integer-valued?
 exact? inexact?
 (rename-out [inexact->exact exact]
             [exact->inexact inexact])
 = < > <= >=
 zero? positive? negative? odd?
 even? finite? infinite? nan?
 min max
 + * - 
 (rename-out [r6rs:/ /])
 abs 
 div-and-mod div mod
 div0-and-mod0 div0 mod0
 gcd lcm
 numerator denominator
 floor ceiling truncate round
 rationalize
 exp (rename-out [r6rs:log log]) sin cos tan asin acos atan
 sqrt (rename-out [integer-sqrt/remainder exact-integer-sqrt])
 (rename-out [r6rs:expt expt])
 make-rectangular make-polar real-part imag-part magnitude 
 (rename-out [r6rs:angle angle]
             [r6rs:number->string number->string]
             [r6rs:string->number string->number])

 ;; 11.8
 not boolean? (rename-out [r6rs:boolean=? boolean=?])

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
 symbol? (rename-out [r6rs:symbol=? symbol=?])
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
             [call-with-current-continuation call/cc])
 call-with-current-continuation
 values call-with-values
 dynamic-wind

 ;; 11.17
 (rename-out [r6rs:quasiquote quasiquote])
 unquote unquote-splicing

 ;; 11.18
 (rename-out [r6rs:let-syntax let-syntax]
             [r6rs:letrec-syntax letrec-syntax])

 ;; 11.19
 (for-syntax (rename-out [r6rs:syntax-rules syntax-rules])
             identifier-syntax
             ...
             _)

 )

;; ----------------------------------------

(define (real-valued? o)
  (or (real? o)
      (and (complex? o)
           (zero? (imag-part o)))))

(define (rational-valued? o)
  (or (rational? o)
      (and (complex? o)
           (zero? (imag-part o))
           (rational? (real-part o)))))

(define (integer-valued? o)
  (or (integer? o)
      (and (complex? o)
           (zero? (imag-part o))
           (integer? (real-part o)))))

(define (finite? n)
  (if (real? n)
      (not (or (eqv? n +inf.0)
               (eqv? n -inf.0)
               (eqv? n +nan.0)))
      (raise-type-error 'infinite? "real" n)))
  
(define (infinite? n)
  (if (real? n)
      (or (eqv? n +inf.0)
          (eqv? n -inf.0))
      (raise-type-error 'infinite? "real" n)))

(define (nan? n)
  (if (real? n)
      (eqv? n +nan.0)
      (raise-type-error 'nan? "real" n)))

;; Someone needs to look more closely at div and mod.
;; I started with the code from Enger04, and poked it
;; until the results matched the examples in R6RS.

(define (div x y)
  (cond
   [(rational? y)
    (let ([n (* (numerator x)
                (denominator y))]
          [d (* (denominator x)
                (numerator y))])
      (if (negative? n)
          (- (quotient (- (abs d) n 1) d))
          (quotient n d)))]
   [(real? y)
    ;; infinity or nan
    (if (equal? y +nan.0)
        +nan.0
        1.0)]
   [else
    (raise-type-error 'div "real number" y)]))

(define (mod x y)
  (- x (* (div x y) y)))

(define (div-and-mod x y)
  (let ([d (div x y)])
    (values d (- x (* d y)))))

(define (div0-and-mod0 x y)
  (let-values ([(d m) (div-and-mod x y)])
    (if (>= m (/ (abs y) 2))
        (if (negative? y)
            (values (sub1 d) (+ m y))
            (values (add1 d) (- m y)))
        (values d m))))

(define (div0 x y)
  (let-values ([(d m) (div0-and-mod0 x y)])
    d))

(define (mod0 x y)
  (let-values ([(d m) (div0-and-mod0 x y)])
    m))

(define-syntax r6rs:/
  ;; R6RS says that division with exact zero is treated like 
  ;; division by inexact zero if any of the other arguments are inexact.
  ;; We use a macro to inline tests in binary mode, since the JIT
  ;; can inline for flonum arithmetic.
  (make-set!-transformer
   (lambda (stx)
     (if (identifier? stx)
         (syntax/loc stx r6rs-/)
         (syntax-case stx (r6rs:set!)
           [(r6rs:set! . _)
            (raise-syntax-error #f
                                "cannot mutate imported identifier"
                                stx)]
           [(_ expr) #'(/ expr)]
           [(_ expr1 expr2)
            #'(let ([a expr1]
                    [b expr2])
                (cond
                 [(and (eq? b 0) (number? a) (inexact? a))
                  (/ a 0.0)]
                 [(and (eq? a 0) (number? b) (inexact? b))
                  (/ 0.0 b)]
                 [else (/ a b)]))]
           [(_ . args) 
            #'(r6rs-/ . args)])))))

(define r6rs-/
  (case-lambda
   [(n) (/ n)]
   [(a b) (r6rs:/ a b)]
   [args (if (ormap (lambda (x) (and (number? x) (inexact? x))) args)
             (apply /
                    (map (lambda (v) (if (eq? v 0)
                                         0.0
                                         v))
                         args))
             (apply / args))]))

(define r6rs:log
  (case-lambda
   [(n) (log n)]
   [(n m) (/ (log n) (log m))]))

(define (r6rs:expt base power)
  (cond
   [(and (number? base)
         (zero? base)
         (number? power))
    (if (zero? power)
        (if (and (eq? base 0)
                 (exact? power))
            1
            1.0)
        (if (positive? (real-part power))
            (if (and (eq? base 0)
                     (exact? power))
                0
                0.0)
            (expt base power)))]
   [(and (eq? base 1)
         (number? power) 
         (inexact? power))
    (expt (exact->inexact base) power)]
   [else (expt base power)]))

(define (r6rs:angle n)
  ; because `angle' produces exact 0 for reals:
  (if (and (inexact-real? n) (positive? n))
      0.0 
      (angle n)))

(define (r6rs:number->string z [radix 10] [precision #f])
  (number->string z radix))

(define (r6rs:string->number s [radix 10])
  (let* ([prefix (case radix
                   [(10) "#d"]
                   [(16) "#x"]
                   [(8) "#o"]
                   [(2) "#b"]
                   [else (raise-type-error
                          'string->number
                          "2, 8, 10, or 16"
                          radix)])]
         [s (if (regexp-match? #rx"#[dDxXoObB]" s)
                s
                (string-append prefix s))])
    (and (regexp-match? (force rx:number) s)
         (string->number (regexp-replace* #rx"[|][0-9]+" s "")))))

(define r6rs:symbol=?
  (case-lambda
   [(a b) (symbol=? a b)]
   [(a b . rest) (and (symbol=? a b)
                      (andmap (lambda (s)
                                (symbol=? a s))
                              rest))]))

(define r6rs:boolean=?
  (case-lambda
   [(a b) (boolean=? a b)]
   [(a b . rest) (and (boolean=? a b)
                      (andmap (lambda (s)
                                (boolean=? a s))
                              rest))]))

(define-syntax-rule (make-mapper what for for-each in-val val-length val->list list->result)
  (case-lambda
   [(proc val) (list->result
                (for ([c (in-val val)])
                  (proc c)))]
   [(proc val1 val2) 
    (if (= (val-length val1)
           (val-length val2))
        (list->result
         (for ([c1 (in-val val1)]
               [c2 (in-val val2)])
           (proc c1 c2)))
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
      (list->result
       (apply for-each
              proc 
              (val->list val1)
              (map val->list vals))))]))

(define string-for-each
  (make-mapper "string" for for-each in-string string-length string->list void))

(define vector-for-each
  (make-mapper "vector" for for-each in-vector vector-length vector->list void))

(define vector-map
  (make-mapper "vector" for/list map in-vector vector-length vector->list list->vector))

(define (add-irritants msg irritants)
  (if (null? irritants)
      msg
      (apply
       string-append
       msg
       "\n irritants:"
       (map (lambda (s)
              (format "\n  ~e" s))
            irritants))))

(define (r6rs:error who msg . irritants)
  (raise
   (make-exn:fail:r6rs
    (add-irritants
     (if who
         (format "~a: ~a" who msg)
         msg)
     irritants)
    (current-continuation-marks)
    msg
    who
    irritants)))

(define (assertion-violation who msg . irritants)
  (raise
   (make-exn:fail:contract:r6rs
    (add-irritants
     (if who
         (format "~a: ~a" who msg)
         msg)
     irritants)
    (current-continuation-marks)
    msg
    who
    irritants)))

(define-syntax-rule (assert expr)
  (unless expr
    (assertion-violation #f "assertion failed")))

;; ----------------------------------------
;; quasiquote generalization

(define-generalized-qq r6rs:quasiquote 
  r5rs:quasiquote unquote unquote-splicing values)

;; ----------------------------------------
;; letrec
;;   Need bindings like R5RS, but int-def body like Racket

(define-syntax (r6rs:letrec stx)
  (syntax-case stx (r6rs:lambda)
    [(_ ([id (r6rs:lambda . rest)] ...) . body)
     #'(letrec ([id (r6rs:lambda . rest)] ...) (let () (#%stratified-body . body)))]
    [(_ bindings . body)
     #'(r5rs:letrec bindings (let () (#%stratified-body . body)))]))

(define-syntax-rule (r6rs:letrec* bindings . body)
  (letrec bindings (#%stratified-body . body)))

;; ----------------------------------------
;; let[*]-values

(define-syntax (r6rs:let-values stx)
  #`(letX-values let-values #,stx))

(define-syntax (r6rs:let*-values stx)
  #`(letX-values let*-values #,stx))

(define-syntax (letX-values stx)
  (syntax-case stx ()
    [(_ dest:let-values orig)
     (let ([orig #'orig])
       (syntax-case orig ()
         [(_ ([formals expr] ...) body0 body ...)
          (with-syntax ([bindings
                         (map (lambda (formals expr)
                                (if (syntax->list formals)
                                    (list formals expr)
                                    (let ([ids (let loop ([formals formals])
                                                 (cond
                                                  [(identifier? formals)
                                                   (list formals)]
                                                  [(and (syntax? formals)
                                                        (pair? (syntax-e formals)))
                                                   (loop (syntax-e formals))]
                                                  [(pair? formals)
                                                   (unless (identifier? (car formals))
                                                     (raise-syntax-error
                                                      #f
                                                      "not an identifier for binding"
                                                      orig
                                                      (car formals)))
                                                   (cons (car formals) (loop (cdr formals)))]
                                                  [else
                                                   (unless (identifier? (car formals))
                                                     (raise-syntax-error
                                                      #f
                                                      "not an identifier for binding"
                                                      orig
                                                      formals))]))])
                                      #`[#,ids
                                         (call-with-values
                                             (lambda () #,expr)
                                           (r6rs:lambda #,formals
                                             (values . #,ids)))])))
                              (syntax->list #'(formals ...))
                              (syntax->list #'(expr ...)))])
            #'(dest:let-values bindings (#%stratified-body body0 body ...)))]))]))

;; ----------------------------------------
;; lambda & define
;;   Need rest-arg conversion like R5RS, but int-def handlign like Racket

(define-syntax (r6rs:lambda stx)
  (syntax-case stx ()
    [(_ (id ...) . body)
     (andmap identifier? (syntax->list #'(id ...)))
     (syntax/loc stx (lambda (id ...) (#%stratified-body . body)))]
    [(_ args . body)
     (syntax/loc stx (r5rs:lambda args (let () (#%stratified-body . body))))]))

(define-for-syntax (check-label id orig-stx def)
  ;; This test shouldn't be needed, and it interferes
  ;;  with macro-introduced bindings:
  #;
  (when (eq? 'module (syntax-local-context))
    (when (identifier-binding id #f)
      (raise-syntax-error
       #f
       "cannot define imported identifier"
       orig-stx
       id)))
  def)

(define-syntax (r6rs:define stx)
  (syntax-case stx ()
    [(_ id) 
     (identifier? #'id)
     (check-label #'id stx (syntax/loc stx (define id (void))))]
    [(_ (name . args) . body)
     (check-label #'name
                  stx
                  (syntax/loc stx (r5rs:define (name . args) (let () (#%stratified-body . body)))))]
    [(_ . rest) #'(define . rest)]))

;; ----------------------------------------
;; define-syntax: wrap a transformer to
;;  ensure that the result of an expansion is
;;  a wrapped syntax object.

(define-syntax (r6rs:define-syntax stx)
  (syntax-case stx ()
    [(_ id expr)
     (identifier? #'id)
     (check-label #'id
                  stx
                  (syntax/loc stx
                    (define-syntax id (wrap-as-needed expr))))]))

(define-for-syntax (wrap-as-needed v)
  (cond
   [(and (procedure? v)
         (procedure-arity-includes? v 1))
    (procedure-reduce-arity
     (case-lambda 
      [(stx) (if (syntax? stx)
                 (let ([r (v stx)])
                   (wrap r stx stx #t))
                 (v stx))]
      [args (apply v args)])
     (procedure-arity v))]
   [(set!-transformer? v)
    (make-set!-transformer (wrap-as-needed (set!-transformer-procedure v)))]
   [else v]))

;; ----------------------------------------

;; let[rec]-syntax needs to be splicing, and it needs the
;; same transformer wrapper as in `define-syntax'

(define-syntax (r6rs:let-syntax stx)
  (syntax-case stx ()
    [(_ ([id expr] ...) body ...)
     (syntax/loc stx
       (splicing-let-syntax ([id (wrap-as-needed expr)] ...) body ...))]))

(define-syntax (r6rs:letrec-syntax stx)
  (syntax-case stx ()
    [(_ ([id expr] ...) body ...)
     (syntax/loc stx
       (splicing-letrec-syntax ([id (wrap-as-needed expr)] ...) body ...))]))
