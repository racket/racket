(module frtime (lib "frp.ss" "frtime")

  (require (all-except mzscheme
                       module
                       #%app
                       #%top
                       #%datum
                       #%plain-module-begin
                       #%module-begin
                       if
                       require
                       provide
                       letrec
                       match
                       cons car cdr pair? null? null
			caar cdar cadr cddr caddr cdddr cadddr cddddr
                       ;undefined?
                       and
                       or
                       cond when unless
                       map ormap andmap assoc member)
           (rename mzscheme mz:cons cons)
           ;(rename mzscheme mz:and and)
           ;(rename mzscheme mz:or or)
           ;(lib "list.ss")
           (lib "contract.ss")
           (only "erl.ss" tid?))
  
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
  
  (define (ormap proc lst)
    (and (pair? lst)
         (or (proc (car lst)) (ormap proc (cdr lst)))))
  
  (define (andmap proc lst)
    (or (null? lst)
        (and (proc (car lst)) (andmap proc (cdr lst)))))
  
  (define (caar v)
    (car (car v)))
  
  (define (cdar v)
    (cdr (car v)))
  
  (define (cadr v)
    (car (cdr v)))
  
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
  
;  (define list
;    (case-lambda
;      [() null]
;      [(a . d) (cons a (apply list d))]))
  
  (define-syntax frtime:case
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
      [(f l) (if (pair? l)
                 (cons (f (car l)) (map f (cdr l)))
                 null)]
      [(f l1 l2) (if (and (pair? l1) (pair? l2))
                     (cons (f (car l1) (car l2)) (map f (cdr l1) (cdr l2)))
                     null)]
      [(f l . ls) (if (and (pair? l) (andmap pair? ls))
                      (cons (lift #f apply f (car l) (map car ls)) (lift #f apply map f (cdr l) (map cdr ls)))
                      null)]))

  ; TO DO: assoc member [vectors] structs
  ; first cut: could be made more efficient by creating
  ; a dedicated signal to update each element of the vector
  (define (frtime:vector2 . args)
    (if (ormap behavior? args)
        (let* ([n (length args)]
               [v1 (make-vector n)]
               [v2 (make-vector n)])
          (apply
           proc->signal
           (lambda ()
             (let ([tmp v2])
               (set! v2 v1)
               (set! v1 tmp))
             (let loop ([i 0] [args args])
               (when (< i n)
                 (vector-set! v1 i (value-now (car args)))
                 (loop (add1 i) (cdr args))))
             v1)
           args))
        (apply vector args)))

  (define (frtime:vector . args)
    (if (ormap behavior? args)
        (let* ([n (length args)]
               [vec (make-vector n)]
               [arg-behs 
                ; initialize the vector
                (let loop ([i 0] [args args] [ret null])
                  (if (< i n)
                      (loop (add1 i)
                            (cdr args)
                            (mz:cons
                             (let ([arg (car args)])
                               (proc->signal
                                (lambda ()
                                  (let ([v (value-now arg)])
                                    (vector-set! vec i v)
                                    v))
                                arg))
                             ret))
                      ret))])
          (apply proc->signal (lambda () arg-behs vec) arg-behs))
        (apply vector args)))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))

  (define (lift-strict . args)
    (apply lift #t args))
  
  ;; Imported from mzscheme:
  (provide (lifted + - * / = eq? equal? eqv? < > <= >= list? add1 cos sin tan symbol->string symbol?
                   number->string exp expt even? odd? list-ref string-append eval
                   sub1 sqrt not number? string? zero? min max modulo
                   string->number void? rational? char? char-upcase char-ci>=? char-ci<=?
                   string>=? char-upper-case? char-alphabetic?
                   string<? string-ci=? string-locale-ci>?
                   string-locale-ci<? string-locale-ci=? atan asin acos exact? magnitude imag-part
                   real-part numerator abs log lcm gcd arithmetic-shift integer-sqrt make-rectangular
                   complex? char>? char<? char=?
                   char-numeric? date-time-zone-offset list->string substring string->list
                   string-ci<? string-ci>=? string<=? string-ci<=? string>? string-locale<? string=?
                   string-length string-ref
                   floor angle round
                   ceiling real? date-hour vector-ref procedure? procedure-arity
                   rationalize date-year-day date-week-day date? date-dst? date-year date-month date-day
                   date-minute date-second make-date char-downcase char>=? char<=? char->integer integer->char boolean?
                   integer? quotient remainder positive? negative? inexact->exact exact->inexact
                   make-polar denominator truncate bitwise-not bitwise-xor bitwise-and bitwise-ior inexact?
                   char-whitespace? assq assv memq memv list-tail reverse append length seconds->date
                   expand syntax-object->datum exn-message continuation-mark-set->list exn-continuation-marks
                   exn:fail?
                   )
           (rename frtime:case case)
           (rename frtime:vector vector)
           (rename frtime:vector2 vector2)
           (rename eq? mzscheme:eq?)
           make-exn:fail
           make-namespace namespace? namespace-symbol->identifier namespace-variable-value
           namespace-set-variable-value! namespace-undefine-variable! namespace-mapped-symbols
           parameterize current-seconds current-milliseconds current-inexact-milliseconds
           call-with-values make-parameter
           null gensym collect-garbage
           error define-struct set! printf fprintf current-error-port for-each void
           procedure-arity-includes? raise-type-error raise thread
           current-continuation-marks
           raise-mismatch-error require-for-syntax define-syntax syntax-rules syntax-case
           set-eventspace
	   install-errortrace-key
           (lifted:nonstrict apply format list list*)
           general-event-processor
           lambda
           case-lambda
           define-values
           define
           let
           let-values
           let*
	   let*-values
           begin
           begin0
           quote
           quasiquote
           unquote
           values
           syntax
           let/ec
           with-handlers
           delay
           force
           random
           sleep
           )

  ;; Defined in frp.ss:
  (provide module
           #%app
           #%top
           #%datum
           #%plain-module-begin
           #%module-begin
           render
           require
           provide
           letrec
           undefined
           undefined?
           if
           lift
           match
           time-b
           seconds
           milliseconds
           exceptions
           cons
           pair?
           null?
           car
           cdr
           signal-value
           signal?
           behavior?
           event?
           event-receiver?
           frtime-version
           raise-exceptions
           synchronize
           frp:send
           snapshot
           snapshot-all
           snapshot/sync
           snapshot/apply
           )
           

 ; (define (behavior? v) (not (event? v)))

  ;; Defined in this module:
  (provide when unless behaviorof -=> nothing nothing?
           cond and or andmap ormap map lift-strict never-e
           caar cadr cdar cddr caddr cdddr cadddr cddddr
           magic value-nowable?)

  ; returns true on values that can be passed to value-now
  ; (e.g. behaviors or constants)
  ; note difference from behavior?, which returns true only
  ; on values that may actually change and should be monitored
  ; for change
  (define (value-nowable? v)
    #t)
    ;(not (and (signal? v) (event-cons? (signal-value v)))))

  (provide/contract
   [proc->signal (((-> any/c))
                  any/c
                  . ->* . (signal?))]

   [value-now (value-nowable? . -> . any)]

   [until (value-nowable? value-nowable? . -> . behavior?)]

   [switch ((event?) (value-nowable?) . opt-> . signal?)]

   [merge-e (() (listof event?) . ->* . (event?))]

   [once-e (event? . -> . event?)]

   [changes (value-nowable? . -> . event?)]

   [event-receiver (-> event?)]

   [when-e (value-nowable? . -> . event?)]

   [while-e (value-nowable? value-nowable? . -> . event?)]

   [==> (event? (any/c . -> . any) . -> . event?)]

   [=#> (event? (any/c . -> . any) . -> . event?)]

   [=#=> (event? (any/c . -> . (union any/c nothing?)) . -> . event?)]

   [map-e ((any/c . -> . any) event? . -> . event?)]
   
   [filter-e ((any/c . -> . any) event? . -> . event?)]

   [filter-map-e ((any/c . -> . (union any/c nothing?)) event? . -> . event?)]

   [collect-e (event? any/c (any/c any/c . -> . any) . -> . event?)]

   [collect-b (event? any/c (any/c any/c . -> . any) . -> . behavior?)]

   [accum-e (event? any/c . -> . event?)]
   
   [accum-b (event? any/c . -> . behavior?)]

   [send-event (event-receiver? any/c . -> . void?)]

   [send-synchronous-event (event-receiver? any/c . -> . void?)]

   [send-synchronous-events (list? . -> . void?)]

   [hold ((event?) (value-nowable?) . opt-> . behavior?)]

   [new-cell (() (any/c) . opt-> . (union behavior? event?))]

   [set-cell! ((union behavior? event?) any/c . -> . void?)]

   [snapshot-e ((event?) any/c . ->* . (event?))]

   [snapshot-map-e ((procedure? event?)
                    any/c     ;; the behaviors
                    . ->* . 
                    (event?))]

   [derivative (value-nowable? . -> . behavior?)]

   [integral ((value-nowable?) (value-nowable?) . opt-> . behavior?)]

   [delay-by (value-nowable? value-nowable? . -> . signal?)]
   
   [inf-delay (value-nowable? . -> . behavior?)]
   
   [bind (symbol? event? . -> . event?)]
   
   [remote-reg (tid? symbol? . -> . event?)]

   ))
