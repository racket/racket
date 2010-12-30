#lang racket

(require "grammar.ss" 
         "reduce.rkt"
         (only-in "model-impl.rkt" runtime-error runtime-error?)
         (except-in redex/reduction-semantics plug)
         racket/runtime-path)

(provide (all-defined-out))

(define (main [seed-arg #f])
  (define seed
    (if seed-arg 
        (string->number seed-arg)
        (add1 (random (sub1 (expt 2 31))))))
  (printf "Test seed: ~s\n" seed)
  (parameterize ([current-pseudo-random-generator test-prg])
    (random-seed seed))
  (parameterize ([redex-pseudo-random-generator test-prg])
    (time (test #:attempts 3000))
    (time (test #:source :-> #:attempts 3000))))

(define-syntax-rule (test . kw-args)
  (redex-check grammar p (same-behavior? (term p)) 
               #:prepare fix-prog . kw-args))

(define fix-prog
  (match-lambda
    [`(<> ,s ,_ ,e)
     (match-let ([`([,xs ,vs] ...) (remove-duplicates s #:key first)])
       `(<> ,(map list xs (map (fix-expr xs) vs)) [] ,((fix-expr xs) e)))]))

(define (fix-expr top-vars) 
  (compose drop-duplicate-binders
           proper-wcms 
           consistent-dws
           (curry close top-vars '())))

(struct error (cause) #:transparent)
(struct answer (output result) #:transparent)
(struct bad-test (reason) #:transparent)
(struct timeout ())

(define (same-behavior? prog)
  (let ([impl-behavior (timeout-kill 15 (impl-eval (impl-program (transform-intermediate prog))))])
    (or (bad-test? impl-behavior)
        (timeout? impl-behavior)
        (let ([model-behavior (timeout-warn 30 (model-eval prog) (pretty-write prog))])
          (or (timeout? model-behavior)
              (if (error? impl-behavior)
                  (error? model-behavior)
                  (and (answer? model-behavior)
                       (equal? impl-behavior model-behavior))))))))

(define impl-program
  (match-lambda
    [`(<> ,s [] ,e)
     `(parameterize ([first-error (box #f)])
        (define result
          (with-handlers ([runtime-error? void])
            (letrec ,s ,e)))
        (match (first-error)
          [(box #f) result]
          [(box err) (raise err)]))]
    [e e]))

(define-runtime-module-path model-impl "model-impl.rkt")

(define impl-eval
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket)
      (namespace-require (resolved-module-path-name model-impl)))
    (define show
      (match-lambda
        [(? procedure?) 'procedure]
        [(? list? vs) (map show vs)]
        [v v]))
    (λ (test)
      (define output (open-output-string))
      (define result
        (with-handlers ([exn:fail? (compose error exn-message)]
                        [runtime-error?
                         (match-lambda
                           [(runtime-error '% "non-procedure" _)
                            (bad-test "procedure as tag")]
                           [e (error e)])])
          (parameterize ([current-output-port output])
            (eval test ns))))
      (if (or (error? result) (bad-test? result))
          result
          (answer (get-output-string output) 
                  (show result))))))

(define model-eval-steps (make-parameter +inf.0))

(define (model-eval prog)
  (let/ec return
    (define show
      (match-lambda
        [(? number? n) n]
        [(? boolean? b) b]
        [`(list . ,vs) (map show vs)]
        [v 'procedure]))
    (define (eval prog steps)
      (define ns (set))
      (let recur ([p prog] [d steps] [s (set)])
        (define qs (apply-reduction-relation :-> p))
        (if (empty? qs)
            (set! ns (set-add ns p))
            (if (< d 0)
                (return (timeout))
                (for ([q qs])
                     (if (set-member? s q)
                         (return (timeout))
                         (recur q (sub1 d) (set-add s p)))))))
      (set-map ns values))
    (match (eval prog (model-eval-steps))
      [(list (and p `(<> ,_ ,output ,result)))
       (if (v? result)
           (answer
            (apply string-append (map (curry format "~v") output))
            (show result))
           (error p))])))

(define (with-timeout thunk timeout on-timeout)
  (let ([c (make-channel)])
    (struct raised (value))
    (let ([t (thread
              (λ () 
                (channel-put 
                 c (with-handlers ([exn:fail? raised])
                     (thunk)))))])
      (match (sync/timeout timeout c)
        [#f (on-timeout t c)]
        [(raised v) (raise v)]
        [x x]))))

(define-syntax-rule (timeout-kill time expr)
  (with-timeout (λ () expr) time 
                (λ (t _) 
                  (kill-thread t)
                  (timeout))))
(define-syntax-rule (timeout-warn time expr warn)
  (with-timeout (λ () expr) time
                (λ (_ c)
                  warn
                  (sync c))))

(define (close top-vars loc-vars expr)
  (match expr
    [(? x? x) 
     (let ([bound (append top-vars loc-vars)])
       (cond [(memq x bound) x]
             [(not (empty? bound)) 
              (random-member bound)]
             [else (random-literal)]))]
    [`(set! ,x ,e)
     (if (empty? top-vars)
         (close top-vars loc-vars e)
         `(set! ,(random-member top-vars)
                ,(close top-vars loc-vars e)))]
    [`(λ ,xs ,e) 
     `(λ ,xs 
        ,(close (filter (negate (curryr member xs)) top-vars) 
                (append xs loc-vars)
                e))]
    [`(dw ,x ,e_1 ,e_2 ,e_3)
     `(dw ,x 
          ,(close top-vars loc-vars e_1) 
          ,(close top-vars loc-vars e_2) 
          ,(close top-vars loc-vars e_3))]
    ; substitution does not recur inside continuation values
    ; (not sure why it bothers to recur within dw expression)
    [`(cont ,v ,E)
     `(cont ,(close top-vars '() v)
            ,(close top-vars '() E))]
    [`(cont ,E)
     `(comp ,(close top-vars '() E))]
    [(? list?)
     (map (curry close top-vars loc-vars) expr)]
    [_ expr]))

(define drop-duplicate-binders
  (match-lambda
    [`(λ ,xs ,e)
     `(λ ,(remove-duplicates xs) ,(drop-duplicate-binders e))]
    [(? list? es)
     (map drop-duplicate-binders es)]
    [e e]))

(define (consistent-dws p)
  (define pre-post
    (let ([h (make-hash)])
      (λ (id pre post)
        (match (hash-ref h id #f)
          [#f
           (hash-set! h id (list pre post))
           (list pre post)]
          [x x]))))
  (let recur ([x p] [excluded '()])
    (match x
      [`(dw ,x ,e1 ,e2 ,e3)
       (if (member x excluded)
           (recur e2 excluded)
           (match-let ([(list e1’ e3’) (pre-post x e1 e3)])
             `(dw ,x 
                  ,(recur e1’ (cons x excluded))
                  ,(recur e2 excluded)
                  ,(recur e3’ (cons x excluded)))))]
      [(? list?) (map (curryr recur excluded) x)]
      [_ x])))

(define (proper-wcms e)
  (let fix ([ok? #t] [e e])
    (match e
      [`(wcm ,w ,e)
       (if ok? 
           `(wcm ,(remove-duplicates (fix #t w) #:key first) 
                 ,(fix #f e))
           (fix #f e))]
      [`(list . ,vs)
       `(list . ,(map (curry fix #t) vs))]
      [`(λ ,xs ,e)
       ; #f in case applied with a continuation that's already marked
       `(λ ,xs ,(fix #f e))]
      [`(cont ,v ,E)
       `(cont ,(fix #t v) ,(fix #t E))]
      [`(comp ,E)
       `(comp ,(fix #t E))]
      [`(begin ,e1 ,e2)
       `(begin ,(fix #t e1)
               ,(fix ok? e2))]
      [`(% ,e1 ,e2 ,e3)
       `(% ,(fix #t e1) ,(fix ok? e2) ,(fix #t e3))]
      [`(dw ,x ,e1 ,e2 ,e3)
       `(dw ,x ,(fix #t e1) ,(fix ok? e2) ,(fix #t e3))]
      [`(if ,e1 ,e2 ,e3)
       `(if ,(fix #t e1) 
            ,(fix ok? e2)
            ,(fix ok? e3))]
      [`(set! ,x ,e)
       `(set! ,x ,(fix #t e))]
      [(? list?) 
       (map (curry fix #t) e)]
      [_ e])))

(define transform-intermediate
  (match-lambda
    [(and p `(<> ,s ,o ,e))
     (define fresh (make-fresh p))
     (define allocated (map first s))
     (define (alloc-cell prefix)
       (define cell (fresh prefix))
       (set! allocated (cons cell allocated))
       cell)
     (define no-dw? (alloc-cell "handlers-disabled?"))
     (define dw-frame-locs
       (let ([locs (make-hash)])
         (λ (x)
           (hash-ref 
            locs x
            (λ () (let ([ys (list (alloc-cell (format "~s-allocated?" x))
                                  (alloc-cell (format "~s-skip-pre?" x))
                                  (alloc-cell (format "~s-comp-cont" x)))])
                    (hash-set! locs x ys)
                    ys))))))
     (define transform
       (match-lambda
         [`(wcm () ,m)
          (transform m)]
         [`(wcm ([,k ,v] . ,w) ,m)
          `(call/cm ,(transform k) ,(transform v)
                    (λ () ,(transform `(wcm ,w ,m))))]
         [(and e `(dw ,x ,e1 ,e2 ,e3))
          (match-let ([(list a? s? c) (dw-frame-locs x)]
                      [t (fresh "t")])
            `((λ (,t)
                (if ,a?
                    (begin (if ,no-dw? #f (set! ,s? #t)) (,c ,t))
                    (% 1
                       (dynamic-wind
                        (λ () 
                          (if ,no-dw?
                              #f
                              (if ,a? 
                                  (if ,s? (set! ,s? #f) ,(transform e1))
                                  #f)))
                        (λ ()
                          ((call/comp 
                            (λ (k)
                              (begin
                                (set! ,c k)
                                (abort 1 k)))
                            1)))
                        (λ () 
                          (if ,no-dw?
                              (set! ,a? #t)
                              (if ,a?
                                  ,(transform e3)
                                  (set! ,a? #t)))))
                       (λ (k) (begin (if ,no-dw? #f (set! ,s? #t)) (k ,t))))))
              (λ () ,(transform e2))))]
         [`(cont ,v ,E)
          (let ([x (fresh "v")])
            `(begin
               (set! ,no-dw? #t)
               ((λ (,x)
                  (% ,x 
                     ,(transform 
                       (term (plug ,E (call/cc (λ (k) (abort ,x k)) ,x))))
                     (λ (x) (begin (set! ,no-dw? #f) x))))
                ,(transform v))))]
         [`(comp ,E)
          (define numbers
            (match-lambda
              [(? integer? n) (list n)]
              [(? list? l) (append-map numbers l)]
              [_ (list)]))
          (define t (add1 (apply max 0 (numbers E))))
          `(begin
             (set! ,no-dw? #t)
             (% ,t 
                ,(transform
                  (term (plug ,E (call/comp (λ (k) (abort ,t k)) ,t))))
                (λ (x) (begin (set! ,no-dw? #f) x))))]
         [`(list ,vs ...)
          `(list ,@(map transform-value vs))]
         [(? list? xs) 
          (map transform xs)]
         [e e]))
     (define transform-value
       (match-lambda
         [(and e (or `(cont ,_ ,_) `(comp ,_)))
          `(λ (x) (,(transform e) x))]
         [e (transform e)]))
     (define e’ (transform e))
     (define s’ (map (match-lambda [(list x v) (list x (transform-value v))]) s))
     `(<> ,(map (λ (x) (match (assoc x s’)
                         [#f (list x #f)]
                         [(list _ v’) (list x v’)]))
                allocated)
          ,o
          ,e’)]))

;; The built-in `plug' sometimes chooses the wrong hole.
(define-metafunction grammar
  [(plug hole any) any]
  [(plug (in-hole W (dw x e_1 E e_2)) any)
   (in-hole W (dw x e_1 (plug E any) e_2))]
  [(plug (wcm w M) any)
   (wcm w (plug M any))]
  [(plug (v ... W e ...) any)
   (v ... (plug W any) e ...)] 
  [(plug (begin W e) any)
   (begin (plug W any) e)]
  [(plug (% W e_1 e_2) any)
   (% (plug W any) e_1 e_2)]
  [(plug (% v e W) any)
   (% v e (plug W any))]
  [(plug (% v_1 W v_2) any)
   (% v_1 (plug W any) v_2)]
  [(plug (set! x W) any)
   (set! x (plug W any))]
  [(plug (if W e_1 e_2) any)
   (if (plug W any) e_1 e_2)])

(define (make-fresh p)
  (define suffix
    (let recur ([x p] [s 0])
      (cond [(symbol? x)
             (match (regexp-match #rx"_(.+)$" (symbol->string x))
               [(list _ n) (max s (add1 (string->number n)))]
               [#f s])]
            [(pair? x) (recur (cdr x) (recur (car x) s))]
            [else s])))
  (λ (prefix)
    (begin0 (string->symbol (format "~a_~a" prefix suffix))
            (set! suffix (add1 suffix)))))

(define (random-literal)
  (random-member
   '(dynamic-wind abort current-marks cons
                  -inf.0 +inf.0 -1 0 1 1/3 -1/4 .33 -.25 4-3i 3+4i
                  call/cc call/comp call/cm
                  #f #t zero? print + first rest)))

(define (random-member xs)
  (parameterize ([current-pseudo-random-generator test-prg])
    (list-ref xs (random (length xs)))))

(define test-prg (make-pseudo-random-generator))