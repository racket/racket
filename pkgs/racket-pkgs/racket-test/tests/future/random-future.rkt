#lang scheme
(require redex/reduction-semantics
         scheme/system
         scheme/flonum
         scheme/future)

#|
The JIT-inlined primitives are listed in `compiler/decompile' in
`annotate-inline'. Plus the unboxed ones in `annoted-unboxed'. Those
are probably interesting.

Other interesting primitives would be ones that consume procedures to
tail-call, such as `apply' or `hash-ref'.

Multiple return values are also interesting.

Errors/exceptions and other kinds of control?

(define (annotate-inline a)
  (if (and (symbol? (car a))
           (case (length a)
             [(2) (memq (car a) '(not null? pair? mpair? symbol?
                                      syntax? char? boolean?
                                      number? real? exact-integer?
                                      fixnum? inexact-real?
                                      procedure? vector? box? string? bytes? eof-object?
                                      zero? negative? exact-nonnegative-integer?  
                                      exact-positive-integer?
                                      car cdr caar cadr cdar cddr
                                      mcar mcdr unbox vector-length syntax-e
                                      add1 sub1 - abs bitwise-not
                                      list list* vector vector-immutable box))]
             [(3) (memq (car a) '(eq? = <= < >= >
                                      bitwise-bit-set? char=?
                                      + - * / quotient remainder min max bitwise-and bitwise-ior bitwise-xor
                                      arithmetic-shift vector-ref string-ref bytes-ref
                                      set-mcar! set-mcdr! cons mcons
                                      list list* vector vector-immutable))]
             [(4) (memq (car a) '(vector-set! string-set! bytes-set!
                                              list list* vector vector-immutable
                                              + - * / min max bitwise-and bitwise-ior bitwise-xor))]
             [else (memq (car a) '(list list* vector vector-immutable
                                        + - * / min max bitwise-and bitwise-ior bitwise-xor))]))
      (cons '#%in a)
      a))

(define (annotate-unboxed args a)
  (define (unboxable? e s)
    (cond
     [(localref? e) #t]
     [(toplevel? e) #t]
     [(eq? '#%flonum (car s)) #t]
     [(not (expr? e)) #t]
     [else #f]))
  (if (and (symbol? (car a))
           (case (length a)
             [(2) (memq (car a) '(flabs flsqrt ->fl
                                        unsafe-flabs
                                        unsafe-flsqrt
                                        unsafe-fx->fl
                                        flsin flcos fltan
                                        flasin flacos flatan
                                        flexp fllog
                                        flfloor flceiling flround fltruncate
                                        flmin flmax
                                        unsafe-flmin unsafe-flmax))]
             [(3) (memq (car a) '(fl+ fl- fl* fl/
                                      fl< fl> fl<= fl>= fl=
                                      flvector-ref
                                      unsafe-fl+ unsafe-fl- unsafe-fl* unsafe-fl/
                                      unsafe-fl< unsafe-fl>
                                      unsafe-fl=
                                      unsafe-fl<= unsafe-fl>=
                                      unsafe-flvector-ref
                                      unsafe-f64vector-ref))]
             
             [(4) (memq (car a) '(flvector-set!
                                  unsafe-flvector-set!
                                  unsafe-f64vector-set!))]
             [else #f])
           (andmap unboxable? args (cdr a)))
      (cons '#%flonum a)
      a))

|#

(define-language fut
  ;; single value, non-error expressions
  (exp (any->any/prim exp)
       fl-exp
       (begin exp exp ...)
       (if (fl-fl->bool/prim fl-exp fl-exp)
           exp
           exp)
       (let-values ([(a b) 2v-exp])
         a)
       (hash-ref (make-hash) 'not-there (λ () exp))
       (with-handlers ((exn:fail? (λ (x) 'failed))) (begin bad-exp 'passed)))
  
  ;; expressions that probably signal a runtime error
  (bad-exp exp
           2v-exp
           (prim bad-exp ...)
           (if bad-exp bad-exp bad-exp)
           (begin bad-exp bad-exp ...)
           (values bad-exp ...))
  
  ;; expressions that produce two multiple values
  (2v-exp (hash-ref (make-hash) 'not-there (λ () 2v-exp))
          (values exp exp)
          (if exp 2v-exp 2v-exp))
  
  (fl-exp fl-val 
          (fl-fl->fl/prim fl-exp fl-exp)
          (fl->fl/prim fl-exp)
          (apply fl-fl->fl/prim (list fl-exp fl-exp)))
  
  (base-val stx sym num fl-val)
  (stx #'exp)
  (sym 'x 'y 'z)
  (num 0 -1 1 1/2 5/3 (sqrt 2))
  (fl-val 1.0 2.0 -1.0 (sqrt 2))
  
  (prim exact-int->fl
        fl->fl/prim
        fl-fl->fl/prim
        fl-fl->bool/prim)
  
  (exact-int->fl ->fl) ;; unused
  (fl->fl/prim flabs flsqrt 
               flsin flcos fltan
               flasin flacos flatan
               flexp fllog
               flfloor flceiling flround fltruncate)
  
  (fl-fl->fl/prim fl+ fl- fl* fl/ flmin flmax)
  (fl-fl->bool/prim fl< fl> fl<= fl>= fl=)
  
  (any->any/prim not pair? mpair? symbol? syntax?
                 syntax? char? boolean?
                 number? real? exact-integer?
                 fixnum? inexact-real?
                 procedure? vector? box? string? bytes? eof-object?))

(define iterations-to-try 10000)

(define gen-exp (let ([f (generate-term fut exp)])
                  (λ () (f 10))))

(define (write-and-try-prog prog)
  (call-with-output-file "tmp.rkt"
    (λ (port)
      (display "#lang scheme\n" port)
      (pretty-print '(require scheme/future scheme/flonum) port)
      (pretty-print prog port))
    #:exists 'truncate)
  (unless (system "./mzscheme3m tmp.rkt")
    (error 'system-failed)))

(define (gen-prog)
  (let* ([expressions (gen-expressions)]
         [vars (build-list (length expressions)
                           (λ (x) (string->symbol (format "f~a" x))))])
    `(let (,@(map (λ (x y) `[,x (future (λ () (let loop ([i ,iterations-to-try])
                                                (unless (zero?  i)
                                                  ,y
                                                  (loop (- i 1))))))])
                  vars
                  expressions))
       ,@(map (λ (x) `(touch ,x)) vars))))

;; gen-expressions : -> (non-empty-listof expressions)
;; currently tailored to the two processor case.
(define (gen-expressions)
  (case (random 5)
    [(0 1 2) (let ([e (gen-exp)])
               (list e e))]
    [(3) (list (gen-exp)
               (gen-exp))]
    [(4) (list (gen-exp)
               (gen-exp)
               (gen-exp))]))

(define-namespace-anchor ns-here)
(let ([seed (+ 1 (random (expt 2 30)))])
  (printf "DrDr Ignore! random-seed ~s\n" seed)
  (random-seed seed))

(define start-time (current-seconds))
(let loop ([n 32])
  (unless (zero? n)
    (when (< (- (current-seconds) start-time) 120)
      (printf ".") (flush-output)
      (let ([p (gen-prog)])
        ;(pretty-print p)
        (eval p (namespace-anchor->namespace ns-here)))
      (loop (- n 1)))))
(newline)

(module+ test
  (module config info
    (define random? #t)))
