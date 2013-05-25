(module lang-utils "lang-core.rkt"
  (require (only-in racket let define-syntax define apply procedure-arity syntax->datum with-input-from-file for-syntax make-empty-namespace cleanse-path collection-path begin syntax-rules)
           (except-in racket
                       else
                       module
                       begin
                       syntax-rules
                       #%app
                       #%top
                       #%datum
                       #%plain-module-begin
                       #%module-begin
                       #%top-interaction
                       λ
                       let
                       define
                       define-syntax
                       define-for-syntax
                       case
                       apply
                       if
                       lambda
                       case-lambda
                       chaperone-procedure
                       free-identifier=?
                       reverse
                       collection-path
                       collection-file-path
                       list-ref
                       require 
                       raise-arity-error
                       procedure-rename
                       impersonate-procedure
                       procedure-reduce-arity
                       procedure-arity
                       procedure->method
                       prop:procedure
                       regexp-replace*
                       provide
                       letrec
                       match
                       cons car cdr pair? null?
                       caar caadr cdar cadar cadr cddr caddr cdddr cadddr cddddr
                       make-struct-type
                       make-struct-field-accessor
                       make-struct-field-mutator
                       vector
                       vector-ref
                       define-struct
                       list
                       list*
                       list?
                       append
                       and
                       or
                       cond when unless
                       map ormap andmap assoc member open-input-file open-output-file open-input-output-file call-with-output-file call-with-input-file with-output-to-file with-input-from-file)
           (rename-in (only-in mzscheme if) [if mzscheme:if])
           (rename-in (only-in "lang-ext.rkt" lift) [lift lift])
           (only-in frtime/core/frp super-lift behavior? value-now)
           (rename-in "lang-ext.rkt"  [undefined undefined])
           (rename-in "lang-ext.rkt" [undefined? undefined?])
           racket/class
           (for-syntax racket/base))
  (require (only-in racket/list empty))
  
  (define-syntax (lifted-send stx)
    (syntax-case stx ()
      [(_ obj meth arg ...)
       (with-syntax ([(obj-tmp) (generate-temporaries '(obj))]
                     [(arg-tmp ...) (generate-temporaries (syntax->list #'(arg ...)))])
         #'(lift #t 
                 (lambda (obj-tmp arg-tmp ...)
                   (send obj-tmp meth arg-tmp ...))
                 obj arg ...))]))
  
  
  (define (list-ref lst idx)
    (if (lift #t positive? idx)
        (list-ref (cdr lst) (lift #t sub1 idx))
        (car lst)))
  
  (define-syntax cond
    (syntax-rules (else =>)
      [(_ [else result1 result2 ...])
       (begin result1 result2 ...)]
      [(_ [test => result])
       (let ([temp test])
         (if temp (result temp)))]
      [(_ [test => result] clause1 clause2 ...)
       (let ([temp test])
         (if temp
             (result temp)
             (cond clause1 clause2 ...)
             (cond clause1 clause2 ...)))]
      [(_ [test]) test]
      [(_ [test] clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...)
             (cond clause1 clause2 ...)))]
      [(_ [test result1 result2 ...])
       (if test (begin result1 result2 ...))]
      [(_ [test result1 result2 ...]
          clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)
           (cond clause1 clause2 ...))]))
  
  (define-syntax and
    (syntax-rules ()
      [(_) #t]
      [(_ exp) exp]
      [(_ exp exps ...) (if exp
                            (and exps ...)
                            #f)]))
  
  (define-syntax or
    (syntax-rules ()
      [(_) #f]
      [(_ exp) exp]
      [(_ exp exps ...) (let ([v exp])
                          (if v
                              v
                              (or exps ...)
                              (or-undef exps ...)))]))
  
  
  (define-syntax or-undef
    (syntax-rules ()
      [(_) undefined]
      [(_ exp) (let ([v exp]) (if v v undefined))]
      [(_ exp exps ...) (let ([v exp])
                          (if v
                              v
                              (or-undef exps ...)
                              (or-undef exps ...)))]))
  
  
  
  (define-syntax when
    (syntax-rules ()
      [(_ test body ...) (if test (begin body ...))]))
  
  (define-syntax unless
    (syntax-rules ()
      [(_ test body ...) (if (not test) (begin body ...))]))
  
  (define ormap
    (case-lambda
      [(pred lst) (list-match
                   lst
                   (lambda (a d) (or (pred a) (ormap pred d)))
                   (lambda () #f))]
      [(pred l1 l2) (list-match
                     l1
                     (lambda (a1 d1)
                       (list-match
                        l2
                        (lambda (a2 d2)
                          (or (pred a1 a2) (ormap pred d1 d2)))
                        (lambda ()
                          (error "expected lists of same length, but got" l1 l2))))
                     (lambda ()
                       (list-match
                        l2
                        (lambda (a d)
                          (error "expected lists of same length, but got" l1 l2))
                        (lambda () #f))))]))
  
  (define (andmap proc lst)
    (list-match
     lst
     (lambda (a d) (and (proc a) (andmap proc d)))
     (lambda () #t)))
  
  (define (caar v)
    (car (car v)))
  
  (define (cdar v)
    (cdr (car v)))
  
  (define (cadr v)
    (car (cdr v)))
  
  (define (cadar v)
    (car (cdar v)))
  
  (define (caadr v)
    (car (cadr v)))
  
  (define (cddr v)
    (cdr (cdr v)))
  
  (define (caddr v)
    (car (cddr v)))
  
  (define (cdddr v)
    (cdr (cddr v)))
  
  (define (cadddr v)
    (car (cdddr v)))
  
  (define (cddddr v)
    (cdr (cdddr v)))
  
  (define (split-list acc lst)
    (if (null? (cdr lst))
        (values acc (car lst))
        (split-list (append acc (list (car lst))) (cdr lst))))
  
  (define (all-but-last lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (all-but-last (cdr lst)))))
  
  (define frp:apply
    (lambda (fn . args)
      (let* ([first-args (all-but-last args)]
             [last-args (raise-list-for-apply (first (last-pair args)))])
        (super-lift
         (lambda (last-args)
           (apply apply fn (append first-args (cons last-args empty))))
         last-args))))
  
  (define-syntax frp:case
    (syntax-rules ()
      [(_ exp clause ...)
       (let ([v exp])
         (vcase v clause ...))]))
  
  (define-syntax vcase
    (syntax-rules (else)
      [(_ v [else exp ...])
       (begin exp ...)]
      [(_ v [dl exp ...])
       (if (lift #t memv v (quote dl))
           (begin exp ...))]
      [(_ v [dl exp ...] clause ...)
       (if (lift #t memv v (quote dl))
           (begin exp ...)
           (vcase v clause ...))]))
  
  (define map
    (case-lambda
      [(f l) (list-match
              l
              (lambda (a d) (cons (f a) (map f d)))
              (lambda () null))]
      [(f l1 l2) (list-match
                  l1
                  (lambda (a1 d1)
                    (list-match
                     l2
                     (lambda (a2 d2) (cons (f a1 a2) (map f d1 d2)))
                     (lambda () (error "map expected lists of same length but got" l1 l2))))
                  (lambda ()
                    (list-match
                     l2
                     (lambda (a2 d2) (error "map expected lists of same length but got" l1 l2))
                     (lambda () null))))]
      [(f l . ls) (if (and (pair? l) (andmap pair? ls))
                      (cons (apply f (car l) (map car ls)) (apply map f (cdr l) (map cdr ls)))
                      null)]))
  
  
  (define (frp:length lst)
    (cond
      [(pair? lst) (lift #t add1 (frp:length (cdr lst)))]
      [(null? lst) 0]
      [else (error 'length (format "expects list, given ~a" lst))]))
  
  (define (frp:list->string lst)
    (lift #t list->string (raise-reactivity lst)))
  
  (define (reverse lst)
    (let loop ([lst lst] [acc '()])
      (if (pair? lst)
          (loop (cdr lst) (cons (car lst) acc))
          acc)))
  
  ;; This do-nothing function is only here so that frtime programs can
  ;; mark segments of code that shouldn't be optimized in the frtime-opt
  ;; language.  Ironically, frtime-opt has its *own* definition of this
  ;; function; this one is just for source compatibility.
  (define (dont-optimize x) x)
  
  (provide cond 
           and 
           or 
           or-undef 
           when 
           unless
           map
           ormap 
           andmap
           caar
           caadr
           cdar
           cadar
           cadr
           cddr
           caddr
           cdddr
           cadddr
           cddddr
           build-path
           collection-path
           lifted-send
           dont-optimize
           
           list-ref
           (rename-out [frp:case case])
           (rename-out [frp:apply apply])
           (rename-out [frp:length length])
           (rename-out [frp:list->string list->string])
           (rename-out [eq? mzscheme:eq?])
           reverse
           
           (lifted + - * / = 
                   eq? 
                   equal? eqv? < > <= >= 
                   add1 cos sin tan symbol->string symbol?
                   number->string string->symbol eof-object? exp expt even? odd? string-append eval
                   sub1 sqrt not number? string string? zero? min max modulo
                   string->number void? rational? char? char-upcase char-ci>=? char-ci<=?
                   string>=? char-upper-case? char-alphabetic?
                   string<? string-ci=? string-locale-ci>?
                   string-locale-ci<? string-locale-ci=? atan asin acos exact? magnitude imag-part
                   real-part numerator abs log lcm gcd arithmetic-shift integer-sqrt make-rectangular
                   complex? char>? char<? char=?
                   char-numeric? date-time-zone-offset substring string->list
                   string-ci<? string-ci>=? string<=? string-ci<=? string>? string-locale<? string=?
                   string-length string-ref
                   floor angle round
                   ceiling real? date-hour procedure? procedure-arity
                   rationalize date-year-day date-week-day date? date-dst? date-year date-month date-day
                   date-minute date-second make-date char-downcase char>=? char<=? char->integer integer->char boolean?
                   integer? quotient remainder positive? negative? inexact->exact exact->inexact
                   make-polar denominator truncate bitwise-not bitwise-xor bitwise-and bitwise-ior inexact?
                   char-whitespace? assq assv memq memv list-tail
                   seconds->date
                   expand syntax->datum exn-message continuation-mark-set->list exn-continuation-marks
                   exn:fail? regexp-match
                   vector->list list->vector make-vector)
           
           
           make-exn:fail  current-inspector make-inspector
           make-empty-namespace namespace? namespace-symbol->identifier namespace-variable-value
           namespace-set-variable-value! namespace-undefine-variable! namespace-mapped-symbols
           parameterize current-seconds current-milliseconds current-inexact-milliseconds
           call-with-values make-parameter
           null
           gensym collect-garbage
           error set! printf fprintf current-error-port for-each void
           procedure-arity-includes? raise-type-error raise thread
           current-continuation-marks
           raise-mismatch-error for-syntax define-syntax define-syntaxes syntax-rules syntax-case
           (lifted:nonstrict format)
           print-struct
           define
           let
           let*
           values
           let*-values           
           let-values
           define-values
           begin
           begin0
           quote
           quasiquote
           unquote
           unquote-splicing
           
           syntax
           let/ec
           with-handlers
           unsyntax
           current-security-guard
           make-security-guard
           dynamic-require
           path? complete-path? absolute-path? relative-path? path-string?
           path->complete-path
           string->path path->string
           bytes->path path->bytes
           split-path simplify-path normal-case-path cleanse-path resolve-path
           path-replace-suffix
           current-directory
           exit
           system-type 
           unsyntax-splicing 
           
           delay
           force
           random
           sleep
           read-case-sensitive
           file-exists?
           with-input-from-file
           read)
  
  ; from core
  (provide (all-from-out "lang-core.rkt"))
  
  )
