#lang racket

(require "grammar.rkt"
         "reduce.rkt"
         (except-in redex/reduction-semantics plug)
         racket/runtime-path)

(provide (all-defined-out))

(module+ main (apply main (vector->list (current-command-line-arguments))))
(define (main . args)
  (define from-grammar-tests #f)
  (define from-rules-tests #f)
  
  (define seed (add1 (random (sub1 (expt 2 31)))))
  
  (define size #f)
  (define attempt->size default-attempt-size)

  (define repetitions 1)
  
  (command-line
   #:argv args
   #:once-each
   ["--grammar"
    n
    "Perform n tests generated from the grammar for programs"
    (set! from-grammar-tests (string->number n))]
   ["--rules"
    n
    "Perform n tests generated from the reduction relation"
    (set! from-rules-tests (string->number n))]
   ["--seed"
    n
    "Generate tests using the PRG seed n"
    (set! seed (string->number n))]
   ["--size"
    n
    "Generate tests of size at most n"
    (set! size (string->number n))
    (set! attempt->size (const size))]
   ["--log"
    p
    "Log generated tests to path p"
    (log-test (curryr pretty-write (open-output-file p #:exists 'truncate)))]
   ["--repetitions"
    n
    "Repeats the command n times"
    (set! repetitions (string->number n))])
  
  
  (for ([_ (in-range 0 repetitions)])
    
    (printf "Test seed: ~a (size: ~a)\n" seed (or size "variable"))
    (parameterize ([current-pseudo-random-generator test-prg])
      (random-seed seed))
    
    (parameterize ([redex-pseudo-random-generator test-prg])
      (when from-grammar-tests
        (time (test #:attempts from-grammar-tests #:attempt-size attempt->size)))
      (when from-rules-tests
        (time (test #:source :-> #:attempts from-rules-tests #:attempt-size attempt->size))))))

(define log-test (make-parameter void))

(define-syntax-rule (test . kw-args)
  (redex-check grammar p (begin ((log-test) (term p)) (same-behavior? (term p))) 
               #:prepare fix-prog . kw-args))

(define fix-prog
  (match-lambda
    [`(<> ,s ,_ ,e)
     (match-let ([`([,xs ,vs] ...) (remove-duplicates s #:key first)])
       `(<> ,(map list xs (map (fix-expr xs) vs)) [] ,((fix-expr xs) e)))]))

(define (fix-expr top-vars) 
  (define rewrite
    (compose drop-duplicate-binders
             proper-wcms
             proper-conts
             consistent-dws
             (curry close top-vars '())))
  ; Must call proper-wcm after proper-conts because the latter
  ; exposes opportunities to the former.
  ;
  ; (% 1 
  ;    (cont 1
  ;          (wcm ([2 3])
  ;               (% 1
  ;                  (wcm ([2 4])
  ;                       hole)
  ;                  (λ (x) x))))
  ;   (λ (x) x))
  ;
  ; But proper-conts sometimes cannot do its job until proper-wcms
  ; turns an arbitrary context into an evaluation context.
  ;
  ; (% 1 
  ;  (cont 1
  ;        (wcm ([2 3])
  ;             (wcm ([2 4])
  ;                  (% 1 hole (λ (x) x)))))
  ;  (λ (x) x))
  ;
  ; It might work to make proper-conts work in more contexts,
  ; but it's easier to iterate the rules to a fixed point (and
  ; there may be more dependencies that require iteration anyway).
  (λ (e)
    (let loop ([e e])
      (define e’ (rewrite e))
      (if (equal? e e’) e (loop e’)))))

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
     `(let* ([previous-error #f]
             [result 
              (with-handlers ([exn:fail? void])
                (call-with-exception-handler
                 (λ (exn)
                   (when (and (exn:fail? exn) (not previous-error))
                     (set! previous-error exn))
                   exn)
                 (λ () (letrec ,s ,e))))])
            (if (exn:fail? previous-error)
                (raise previous-error)
                result))]
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
        (with-handlers ([exn:fail?
                         (match-lambda
                           [(exn:fail (regexp "%: expected argument of type <non-procedure>") _)
                            (bad-test "procedure as tag")]
                           [(exn:fail m _)
                            (error m)])])
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
     (define e’ (close top-vars loc-vars e))
     (cond [(memq x top-vars)
            `(set! ,x ,e’)]
           [(empty? top-vars) e’]
           [else `(set! ,(random-member top-vars) ,e’)])]
    [`(λ ,xs ,e) 
     `(λ ,xs 
        ,(close (filter (negate (curryr member xs)) top-vars) 
                (append xs loc-vars)
                e))]
    [`(dw ,x ,e_1 ,e_2 ,e_3)
     ; Local variables are substituted away in realistic pre-
     ; and post-thunks. This invariant is important to 
     ; `consistent-dws', which copies such thunks into different
     ; scopes.
     `(dw ,x 
          ,(close top-vars '() e_1) 
          ,(close top-vars loc-vars e_2) 
          ,(close top-vars '() e_3))]
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
  ; Performs two tasks:
  ; 1. drops duplicate cm keys, and
  ; 2. drops `wcm' frames when the reduction relation
  ; would not otherwise merge the marks (replacing them
  ; with `call/cm' requires more care, since the `wcm'
  ; body may contain a hole).
  (let fix ([ctxt 'wont-have-wcm] [e e])
    (define tail
      (match-lambda
        [(or 'comp-top 'may-have-wcm) 'may-have-wcm]
        ['wont-have-wcm 'wont-have-wcm]))
    (match e
      [`(wcm ,w ,e)
       (match ctxt
         [(or 'comp-top 'wont-have-wcm)
          `(wcm ,(remove-duplicates (fix 'dont-care w) #:key first) 
                ,(fix 'may-have-wcm e))]
         ['may-have-wcm
          (fix 'may-have-wcm e)])]
      [`(list . ,vs)
       ; context doesn't matter for values
       `(list . ,(map (curry fix 'dont-care) vs))]
      [`(λ ,xs ,e)
       ; caller's continuation may be marked
       `(λ ,xs ,(fix 'may-have-wcm e))]
      [`(cont ,v ,E)
       ; body will be wrapped in a prompt
       `(cont ,(fix 'dont-care v) ,(fix 'wont-have-wcm E))]
      [`(comp ,E)
       ; comp application merges only top-level marks
       `(comp ,(fix 'comp-top E))]
      [`(begin ,e1 ,e2)
       `(begin ,(fix 'wont-have-wcm e1)
               ; "begin-v" does not merge marks
               ,(fix (tail ctxt) e2))]
      [`(% ,e1 ,e2 ,e3)
       `(% ,(fix 'wont-have-wcm e1)
           ; prompt persists until e2 is a value
           ,(fix 'wont-have-wcm e2)
           ,(fix 'wont-have-wcm e3))]
      [`(dw ,x ,e1 ,e2 ,e3)
       `(dw ,x 
            ,(fix 'wont-have-wcm e1)
            ; dw persists until e2 is a value
            ,(fix 'wont-have-wcm e2)
            ,(fix 'wont-have-wcm e3))]
      [`(if ,e1 ,e2 ,e3)
       `(if ,(fix 'wont-have-wcm e1) 
            ; "ift" and "iff" do not merge marks
            ,(fix (tail ctxt) e2)
            ,(fix (tail ctxt) e3))]
      [`(set! ,x ,e)
       `(set! ,x ,(fix 'wont-have-wcm e))]
      [(? list?) 
       (map (curry fix 'wont-have-wcm) e)]
      [_ e])))

(define proper-conts
  ; Turns (cont v_1 (in-hole E_1 (% v_1 E_2 v_2)))
  ; into  (cont v_1 (in-hole E_1        E_2     ))
  ; since no real program can construct the former.
  ;
  ; It would be nice to perform this transformation
  ; by iteratively applying this rewrite rule
  ;
  ; (--> (in-hole (name C (cross e)) (cont v_1 (in-hole E_1 (% v_1 E_2 v_2))))
  ;      (in-hole C (cont v_1 (in-hole E_1 E_2))))
  ;
  ; but a Redex bug (PR 11579) prevents that from working.
  (let ([none (gensym)])
    (define-metafunction grammar
      [(fix (cont v E) any)
       (cont (fix v ,none) (fix E v))]
      
      [(fix (dw x e_1 E e_2) any)
       (dw x (fix e_1 ,none) (fix E any) (fix e_2 ,none))]
      [(fix (wcm w E) any)
       (wcm (fix w ,none) (fix E any))]
      [(fix (v ... E e ...) any)
       ((fix v ,none) ... (fix E any) (fix e ,none) ...)]
      [(fix (begin E e) any)
       (begin (fix E any) (fix e ,none))]
      [(fix (% E e_1 e_2) any)
       (% (fix E any) (fix e_1 ,none) (fix e_2 ,none))]
      [(fix (% v e E) any)
       (% (fix v ,none) (fix e ,none) (fix E any))]
      [(fix (% any E v) any)
       (fix E any)]
      [(fix (% v_1 E v_2) any)
       (% (fix v_1 ,none) (fix E any) (fix v_2 ,none))]
      [(fix (set! x E) any)
       (set! x (fix E any))]
      [(fix (if E e_1 e_2) any)
       (if (fix E any) (fix e_1 ,none) (fix e_2 ,none))]
      
      [(fix (any_1 ...) any_2)
       ((fix any_1 ,none) ...)]
      [(fix any_1 any_2)
       any_1])
    (λ (expr)
      (term (fix ,expr ,none)))))

(define transform-intermediate
  (match-lambda
    [(and p `(<> ,s ,o ,e))
     (define fresh (make-fresh p))
     (define allocated (map first s))
     (define (alloc-cell prefix)
       (define cell (fresh prefix))
       (set! allocated (cons cell allocated))
       cell)
     (define capts (alloc-cell "active-cont-capts"))
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
                    (begin (if (zero? ,capts) (set! ,s? #t) #f) (,c ,t))
                    (% 1
                       (dynamic-wind
                        (λ () 
                          (if (zero? ,capts)
                              (if ,a? 
                                  (if ,s? (set! ,s? #f) ,(transform e1))
                                  #f)
                              #f))
                        (λ ()
                          ((call/comp 
                            (λ (k)
                              (begin
                                (set! ,c k)
                                (abort 1 k)))
                            1)))
                        (λ () 
                          (if (zero? ,capts)
                              (if ,a?
                                  ,(transform e3)
                                  (set! ,a? #t))
                              (set! ,a? #t))))
                       (λ (k) (begin (if (zero? ,capts) (set! ,s? #t) #f) (k ,t))))))
              (λ () ,(transform e2))))]
         [`(cont ,v ,E)
          (let ([x (fresh "v")])
            `(begin
               (set! ,capts (+ ,capts 1))
               ((λ (,x)
                  (% ,x 
                     ,(transform 
                       (term (plug ,E (call/cc (λ (k) (abort ,x k)) ,x))))
                     (λ (x) (begin (set! ,capts (+ ,capts -1)) x))))
                ,(transform v))))]
         [`(comp ,E)
          (define numbers
            (match-lambda
              [(? integer? n) (list n)]
              [(? list? l) (append-map numbers l)]
              [_ (list)]))
          (define t (add1 (apply max 0 (numbers E))))
          `(begin
             (set! ,capts (+ ,capts 1))
             (% ,t 
                ,(transform
                  (term (plug ,E (call/comp (λ (k) (abort ,t k)) ,t))))
                (λ (x) (begin (set! ,capts (+ ,capts -1)) x))))]
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
          (begin
            (set! ,capts 0)
            ,e’))]))

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
