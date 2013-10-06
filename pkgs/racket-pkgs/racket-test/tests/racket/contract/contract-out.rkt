#lang racket/base
(require "test-util.rkt"
         (for-syntax racket/base))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  
  (define exn:fail:contract:blame-object 
    (contract-eval 'exn:fail:contract:blame-object))
  (define exn:fail:contract:blame?
    (contract-eval 'exn:fail:contract:blame?))
  
  (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 scheme/base
               (require scheme/contract)
               (define x 1)
               (provide/contract (x integer?))))
      (eval '(require 'contract-test-suite1))
      (eval 'x)))
  
  (test/spec-passed
   'provide/contract2
   '(let ()
      (eval '(module contract-test-suite2 scheme/base
               (require scheme/contract)
               (provide/contract)))
      (eval '(require 'contract-test-suite2))))
  
  (test/spec-failed
   'provide/contract3
   '(let ()
      (eval '(module contract-test-suite3 scheme/base
               (require scheme/contract)
               (define x #f)
               (provide/contract (x integer?))))
      (eval '(require 'contract-test-suite3))
      (eval 'x))
   "contract-test-suite3")
  
  (test/spec-passed
   'provide/contract4
   '(begin
      (eval '(module contract-test-suite4 scheme/base
               (require scheme/contract)
               (define-struct s (a) #:mutable)
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite4))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)))))
  
  (test/spec-passed
   'provide/contract4-b
   '(begin
      (eval '(module contract-test-suite4-b scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite4-b))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))))))
  
  (test/spec-passed/result
   'provide/contract4-c
   '(begin
      (eval '(module contract-test-suite4-c scheme/base
               (require scheme/contract)
               (define-struct s (a b) #:mutable)
               (provide/contract (struct s ((a any/c) (b any/c))))))
      (eval '(require 'contract-test-suite4-c))
      (eval '(let ([an-s (make-s 1 2)])
               (list (s-a an-s)
                     (s-b an-s)
                     (begin (set-s-a! an-s 3)
                            (s-a an-s))
                     (begin (set-s-b! an-s 4)
                            (s-b an-s))))))
   
   (list 1 2 3 4))
  
  (test/spec-passed
   'provide/contract5
   '(begin
      (eval '(module contract-test-suite5 scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (define-struct t (a))
               (provide/contract (struct s ((a any/c)))
                                 (struct t ((a any/c))))))
      (eval '(require 'contract-test-suite5))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (make-t 1)
                   (t-a (make-t 1))
                   (t? (make-t 1))))))
  
  (test/spec-passed
   'provide/contract6
   '(begin
      (eval '(module contract-test-suite6 scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite6))
      (eval '(define-struct (t s) ()))))
  
  (test/spec-passed
   'provide/contract6b
   '(begin
      (eval '(module contract-test-suite6b scheme/base
               (require scheme/contract)
               (define-struct s_ (a))
               (provide/contract (struct s_ ((a any/c))))))
      (eval '(require 'contract-test-suite6b))
      (eval '(module contract-test-suite6b2 scheme/base
               (require 'contract-test-suite6b)
               (require scheme/contract)
               (define-struct (t_ s_) (b))
               (provide s_-a)
               (provide/contract (struct (t_ s_) ((a any/c) (b any/c))))))
      (eval '(require 'contract-test-suite6b2))
      (eval '(define-struct (u_ t_) ()))
      (eval '(s_-a (make-u_ 1 2)))))
  
  (test/spec-passed
   'provide/contract7
   '(begin
      (eval '(module contract-test-suite7 scheme/base
               (require scheme/contract)
               (define-struct s (a b))
               (define-struct (t s) (c d))
               (provide/contract
                (struct s ((a any/c) (b any/c)))
                (struct (t s) ((a any/c) (b any/c) (c any/c) (d any/c))))))
      (eval '(require 'contract-test-suite7))
      (eval '(let ([x (make-t 1 2 3 4)])
               (s-a x)
               (s-b x)
               (t-c x)
               (t-d x)
               (void)))))
  
  (test/spec-passed
   'provide/contract8
   '(begin
      (eval '(module contract-test-suite8 scheme/base
               (require scheme/contract)
               (define-struct i-s (contents))
               (define (w-f-s? x) #t)
               (provide/contract
                (struct i-s ((contents (flat-named-contract "integer-set-list" w-f-s?)))))))
      (eval '(require 'contract-test-suite8))
      (eval '(i-s-contents (make-i-s 1)))))
  
  (test/spec-passed
   'provide/contract9
   '(begin
      (eval '(module contract-test-suite9 scheme/base
               (require scheme/contract)
               (define the-internal-name 1)
               (provide/contract (rename the-internal-name the-external-name integer?))
               (void (+ the-internal-name 1))))
      (eval '(require 'contract-test-suite9))
      (eval '(+ the-external-name 1))))
  
  (test/spec-passed
   'provide/contract10
   '(begin
      (eval '(module pc10-m scheme/base
               (require scheme/contract)
               (define-struct s (a b) #:inspector (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module pc10-n scheme/base
               (require mzlib/struct
                        'pc10-m)
               (print-struct #t)
               (void 
                (copy-struct s
                             (make-s 1 2)
                             [s-a 3]))))
      (eval '(require 'pc10-n))))
  
  (test/spec-passed
   'provide/contract11
   '(begin
      (eval '(module pc11-m scheme/base
               (require scheme/contract)
               (define x 1)
               (provide/contract [rename x y integer?]
                                 [rename x z integer?])))
      (eval '(module pc11-n scheme/base
               (require 'pc11-m)
               (void (+ y z))))
      (eval '(require 'pc11-n))))
  
  ;; this test is broken, not sure why
  #|
  (test/spec-failed
   'provide/contract11b
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module pc11b-m scheme/base
               (require scheme/contract)
               (define-struct s (a b) #:inspector (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module pc11b-n scheme/base
               (require mzlib/struct
                        m)
               (print-struct #t)
               (copy-struct s
                            (make-s 1 2)
                            [s-a #f])))
      (eval '(require 'pc11b-n)))
   "n")
|#
  
  (test/spec-passed
   'provide/contract12
   '(begin
      (eval '(module pc12-m scheme/base
               (require scheme/contract)
               (define-struct (exn2 exn) ())
               (provide/contract (struct (exn2 exn) ((message any/c) (continuation-marks any/c))))))
      (eval '(require 'pc12-m))))
  
  (test/spec-passed/result
   'provide/contract13
   '(begin
      (eval '(module pc13-common-msg-structs scheme/base
               (require scheme/contract)
               (define-struct register (name type) #:inspector (make-inspector))
               (provide/contract (struct register ([name any/c] [type any/c])))))
      
      (eval '(require 'pc13-common-msg-structs))
      (eval '(require (lib "plt-match.rkt")))
      (eval '(match (make-register 1 2)
               [(struct register (name type))
                (list name type)])))
   (list 1 2))
  
  (test/spec-passed
   'provide/contract14
   '(begin
      (eval '(module pc14-test1 scheme/base
               (require scheme/contract)
               
               (define-struct type (flags))
               (define-struct (type:ptr type) (type))
               
               (provide/contract
                (struct type
                  ([flags (listof string?)]))
                
                (struct (type:ptr type)
                  ([flags (listof string?)] [type type?])))))
      
      (eval '(module pc14-test2 scheme/base
               (require mzlib/plt-match)
               (require 'pc14-test1)
               (void
                (match (make-type:ptr '() (make-type '()))
                  [(struct type:ptr (flags type)) #f]))))
      (eval '(require 'pc14-test2))))
  
  ;; make sure unbound identifier exception is raised.
  (contract-error-test
   'contract-error-test7
   #'(begin
       (eval '(module pos scheme/base
                (require scheme/contract)
                (provide/contract [i any/c]))))
   exn:fail:syntax?)
  
  ;; provide/contract should signal errors without requiring a reference to the variable
  ;; this test is bogus, because provide/contract'd variables can be set!'d.
  (test/spec-failed
   'provide/contract15
   '(begin
      (eval '(module pos scheme/base
               (require scheme/contract)
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require 'pos)))
   "pos")
  
  ;; this is really a positive violation, but name the module `neg' just for an addl test
  (test/spec-failed
   'provide/contract16
   '(begin
      (eval '(module neg scheme/base
               (require scheme/contract)
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require 'neg)))
   "neg")
  
  ;; this test doesn't pass yet ... waiting for support from define-struct
  
  #;
  (test/neg-blame
   'provide/contract17
   '(begin
      (eval '(module pos scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (provide/contract [struct s ((a integer?))])))
      (eval '(module neg scheme/base
               (require 'pos)
               (define-struct (t s) ())
               (make-t #f)))
      (eval '(require 'neg))))
  
  (test/spec-passed
   'provide/contract18
   '(begin
      (eval '(module pc18-pos scheme/base
               (require scheme/contract)
               (define-struct s ())
               (provide/contract [struct s ()])))
      (eval '(require 'pc18-pos))
      (eval '(make-s))))
  
  (test/spec-passed/result
   'provide/contract19
   '(begin
      (eval '(module pc19-a scheme/base
               (require scheme/contract)
               (define-struct a (x))
               (provide/contract [struct a ([x number?])])))
      
      (eval '(module pc19-b scheme/base
               (require 'pc19-a
                        scheme/contract)
               (define-struct (b a) (y))
               (provide/contract [struct (b a) ([x number?] [y number?])])))
      
      (eval '(module pc19-c scheme/base
               (require 'pc19-b
                        scheme/contract)
               
               (define-struct (c b) (z))
               (provide/contract [struct (c b) ([x number?] [y number?] [z number?])])))
      
      (eval' (module pc19-d scheme/base
               (require 'pc19-a 'pc19-c)
               (define pc19-ans (a-x (make-c 1 2 3)))
               (provide pc19-ans)))
      
      (eval '(require 'pc19-d))
      (eval 'pc19-ans))
   1)
  
  ;; test that unit & contract don't collide over the name `struct'
  (test/spec-passed
   'provide/contract20
   '(eval '(module tmp scheme/base
             (require scheme/contract
                      mzlib/unit)
             
             (define-struct s (a b))
             
             (provide/contract
              [struct s ([a number?]
                         [b symbol?])]))))
  
  (test/spec-passed
   'provide/contract21
   '(begin
      (eval '(module provide/contract21a scheme/base
               (require scheme/contract)
               (provide/contract [f integer?])
               (define f 1)))
      (eval '(module provide/contract21b scheme/base
               (require (for-syntax 'provide/contract21a)
                        (for-syntax scheme/base))
               (define-syntax (unit-body stx)
                 f f
                 #'1)))))
  
  (test/spec-passed
   'provide/contract22
   '(begin
      (eval '(module provide/contract22a scheme/base
               (require scheme/contract)
               (provide/contract [make-bound-identifier-mapping integer?])
               (define make-bound-identifier-mapping 1)))
      (eval '(module provide/contract22b scheme/base
               (require (for-syntax 'provide/contract22a)
                        (for-syntax scheme/base))
               
               (define-syntax (unit-body stx)
                 make-bound-identifier-mapping)
               
               (define-syntax (f stx)
                 make-bound-identifier-mapping)))))
  
  (test/spec-passed
   'provide/contract23
   '(begin
      (eval '(module provide/contract23a scheme/base
               (require scheme/contract)
               (provide/contract [f void?])
               (define f (void))))
      
      (eval '(module provide/contract23b scheme/base
               (require 'provide/contract23a)
               (#%expression f)
               f))
      
      (eval '(require 'provide/contract23b))))
  
  #|
  (test/spec-passed
   'provide/contract24
   '(begin
      (eval '(module provide/contract24 scheme/base
               (require (prefix-in c: scheme/contract))
               (c:case-> (c:-> integer? integer?)
                         (c:-> integer? integer? integer?))))))
  |#
  
  ;; tests that contracts pick up the #%app from the context
  ;; instead of always using the scheme/base #%app.
  (test/spec-passed
   'provide/contract25
   '(begin
      (eval '(module provide/contract25a scheme/base
               (require scheme/contract)
               (provide/contract [seventeen integer?])
               (define seventeen 17)))
      (eval '(module provide/contract25b scheme/base
               (require 'provide/contract25a)
               (void (let-syntax ([#%app (syntax-rules ()
                                           [(#%app e ...) (list e ...)])])
                       (seventeen 18)))))
      (eval '(require 'provide/contract25b))))
  
  (test/spec-passed/result
   'provide/contract26
   '(begin
      (eval '(module provide/contract26 scheme/base
               (require scheme/contract)
               (define-struct pc26-s (a))
               (provide/contract (struct pc26-s ((a integer?))))))
      (eval '(require 'provide/contract26))
      (eval '(pc26-s-a (make-pc26-s 1))))
   1)
  
  (test/spec-passed/result
   'provide/contract27
   '(begin
      (eval '(module provide/contract27a scheme/base
               (require scheme/contract)
               (define-struct person (name) #:transparent)
               (provide/contract (struct person ([name string?])))))
      (eval '(module provide/contract27b scheme/base
               (require 'provide/contract27a)
               (provide (struct-out person))))
      (eval '(module provide/contract27c scheme/base
               (require 'provide/contract27b)
               (define provide/contract27ans (person-name (make-person "me")))
               (provide provide/contract27ans)))
      (eval '(require 'provide/contract27c))
      (eval 'provide/contract27ans))
   "me")
  
  #;
  (test/spec-passed/result
   'provide/contract28
   '(begin
      (eval '(module provide/contract28-m1 scheme/base
               (require scheme/contract)
               (define-struct repair () #:transparent)
               (provide/contract [struct repair ()])))
      (eval '(module provide/contract28-m2 scheme/base
               (require 'provide/contract28-m1 scheme/contract)
               (provide/contract [struct repair ()])))
      (eval '(module provide/contract28-m3 scheme/base
               (require 'provide/contract28-m2)
               (provide provide/contract28-res)
               (define provide/contract28-res (repair? (make-repair)))))
      (eval '(require 'provide/contract28-m3))
      (eval 'provide/contract28-res))
   #t)
  
  #;
  (test/spec-passed/result
   'provide/contract29
   '(begin
      (eval '(module provide/contract29-m1 scheme/base
               (require scheme/contract)
               (define-struct q (a b))
               (define-struct (repair q) (c d) #:transparent)
               (provide/contract 
                [struct repair ([a integer?] [b integer?] [c integer?] [d integer?])])))
      (eval '(module provide/contract29-m2 scheme/base
               (require 'provide/contract29-m1 scheme/contract)
               (provide/contract 
                [struct repair ([a integer?] [b integer?] [c integer?] [d integer?])])))
      (eval '(module provide/contract29-m3 scheme/base
               (require 'provide/contract29-m2)
               (provide provide/contract29-res)
               (define provide/contract29-res (list (repair? (make-repair 1 2 3 4))
                                                    (repair-c (make-repair 1 2 3 4))))))
      (eval '(require 'provide/contract29-m3))
      (eval 'provide/contract29-res))
   (list #t 3))
  
  ;; for this test I have to be able to track back thru the requirees to find the right
  ;; name for the negative blame (currently it blames m3, but it should  blame m2).
  #;
  (test/spec-failed
   'provide/contract30
   '(begin
      (eval '(module provide/contract30-m1 scheme/base
               (require scheme/contract)
               (provide/contract [f (-> integer? integer?)])
               (define (f x) x)))
      (eval '(module provide/contract30-m2 scheme/base
               (require 'provide/contract30-m1)
               (provide f)))
      (eval '(module provide/contract30-m3 scheme/base
               (require 'provide/contract30-m2)
               (f #f)))
      (eval '(require 'provide/contract30-m3)))
   "provide/contract30-m2")
  
  (test/spec-passed/result
   'provide/contract31
   '(begin
      (eval '(module provide/contract31-m1 scheme/base
               (require scheme/contract)
               (provide/contract
                #:∃ x
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))
      
      (eval '(module provide/contract31-m2 scheme/base
               (require scheme/contract 'provide/contract31-m1)
               (provide provide/contract31-x)
               (define provide/contract31-x (f (λ (x) x)))))
      
      (eval '(require 'provide/contract31-m2))
      (eval 'provide/contract31-x))
   10)
  
  (test/spec-passed/result
   'provide/contract32
   '(begin
      (eval '(module provide/contract32-m1 scheme/base
               (require scheme/contract)
               (provide/contract
                #:exists x
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))
      
      (eval '(module provide/contract32-m2 scheme/base
               (require scheme/contract 'provide/contract32-m1)
               (provide provide/contract32-x)
               (define provide/contract32-x (f (λ (x) x)))))
      
      (eval '(require 'provide/contract32-m2))
      (eval 'provide/contract32-x))
   10)
  
  (test/spec-passed/result
   'provide/contract33
   '(begin
      (eval '(module provide/contract33-m1 scheme/base
               (require scheme/contract)
               (provide/contract
                #:exists (x)
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))
      
      (eval '(module provide/contract33-m2 scheme/base
               (require scheme/contract 'provide/contract33-m1)
               (provide provide/contract33-x)
               (define provide/contract33-x (f (λ (x) x)))))
      
      (eval '(require 'provide/contract33-m2))
      (eval 'provide/contract33-x))
   10)
  
  (test/spec-passed/result
   'provide/contract34
   '(begin
      (eval '(module provide/contract34-m1 scheme/base
               (require scheme/contract)
               (define x integer?)
               (define g 11)
               (provide/contract
                [g x]
                #:exists (x)
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))
      
      (eval '(module provide/contract34-m2 scheme/base
               (require scheme/contract 'provide/contract34-m1)
               (provide provide/contract34-x)
               (define provide/contract34-x (f (λ (x) x)))))
      
      (eval '(require 'provide/contract34-m2))
      (eval 'provide/contract34-x))
   10)
  
  
  ;; The following test is designed to test that source locations for contracts
  ;; survive compilation and being saved to disk (and thus aren't recorded by
  ;; quoted syntax object constant embedded in the expansion).
  (let ()
    ;; compile/wash : like compile, but reads and writes the data
    ;; so that source locations (and other things presumably) get dumped.
    (define (compile/wash x)
      (let-values ([(in out) (make-pipe)])
        (thread
         (λ () (write (contract-compile x) out)))
        (parameterize ([read-accept-compiled #t])
          (read in))))
    
    ;; drop-var-info : syntax -> syntax
    ;; strips the lexical content from the syntax object, but preserves the source locations
    (define-syntax (drop-var-info stx)
      (syntax-case stx ()
        [(_ arg)
         (let loop ([stx #'arg])
           (cond
             [(syntax? stx)
              #`(datum->syntax #f 
                               #,(loop (syntax-e stx)) 
                               (vector #,(syntax-source stx)
                                       #,(syntax-line stx)
                                       #,(syntax-column stx)
                                       #,(syntax-position stx)
                                       #,(syntax-span stx)))]
             [(pair? stx)
              #`(cons #,(loop (car stx))
                      #,(loop (cdr stx)))]
             [else #`'#,stx]))]))
    
    ;; WARNING: do not add or remove lines between here-line and the two modules
    ;; below it, unless you also revise the expected result of the test case.
    (define here-line (let-syntax ([m (λ (stx) #`'#,(syntax-line stx))]) (m)))
    
    (contract-eval
     (compile/wash
      (drop-var-info
       (module provide/contract-35/m racket/base
         (require racket/contract)
         (define (f x) x)
         (provide/contract [f (-> integer? integer?)])))))
    
    (contract-eval
     (compile/wash
      (drop-var-info
       (module provide/contract-35/n racket/base
         (require 'provide/contract-35/m)
         (f #f)))))
    
    (test (format "contract-out.rkt:~a.28"
                  (+ here-line 8))
          'provide/contract-compiled-source-locs
          (with-handlers ((exn:fail?
                           (λ (x)
                             (define m 
                               (regexp-match #rx"contract-out[.]rkt[^ ]*.28" (exn-message x)))
                             (if m 
                                 (car m) 
                                 (list 'regexp-match-failed (exn-message x))))))
            (contract-eval '(require 'provide/contract-35/n)))))
  
  ;; test that provide/contract by itself in a module doesn't signal an error
  (test/spec-passed/result
   'provide/contract35
   '(begin
      (eval '(module provide/contract35-m1 racket
               (provide/contract [add1 (-> number? number?)])))
      
      (eval '(module provide/contract35-m2 racket/base
               (require 'provide/contract35-m1)
               (provide provide/contract35-three)
               (define provide/contract35-three (add1 2))))
      
      (eval '(require 'provide/contract35-m2))
      (eval 'provide/contract35-three))
   3)
  
  (test/spec-passed/result
   'provide/contract36
   '(begin
      
      (eval '(module provide/contract36-m racket/base
               (require racket/contract)
               (struct a (x))
               (struct b a ())
               (provide/contract
                [struct a ((x symbol?))]
                [struct (b a) ((x symbol?))])))
      
      (eval '(module provide/contract36-n racket/base
               (require 'provide/contract36-m)
               (provide new-b-x)
               (define new-b-x
                 (a-x
                  (struct-copy b (b 'x)
                               [x #:parent a 'y])))))
      
      (eval '(require 'provide/contract36-n))
      (eval 'new-b-x))
   'y)
  
  (test/spec-failed
   'provide/contract37
   '(begin
      
      (eval '(module provide/contract37-m racket/base
               (require racket/contract)
               (struct a (x))
               (struct b a ())
               (provide/contract
                [struct a ((x symbol?))]
                [struct (b a) ((x symbol?))])))
      
      (eval '(module provide/contract37-n racket/base
               (require 'provide/contract37-m)
               (struct-copy b (b 'x)
                            [x #:parent a 5])))
      
      (eval '(require 'provide/contract37-n)))
   "provide/contract37-n")
  
  (test/spec-passed/result
   'provide/contract38
   '(begin
      (eval
       '(module provide/contract38-a racket
          (define-struct s () #:transparent)
          (provide/contract [struct s ()])))
      
      (eval
       '(module provide/contract38-b racket
          (require 'provide/contract38-a)
          (define a-struct (make-s))
          (define-values (type _) (struct-info a-struct))
          (provide the-answer)
          (define the-answer (eq? type struct:s))))
      
      (dynamic-require ''provide/contract38-b 'the-answer))
   #t)
  
  ;; #:forall contract-out clauses
  (test/spec-passed/result
   'provide/contract39
   '(begin
      (eval '(module provide/contract39-m1 racket/base
               (require racket/contract)
               (provide/contract
                #:∀ x
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))
      
      (eval '(module provide/contract39-m2 racket/base
               (require racket/contract 'provide/contract39-m1)
               (provide provide/contract39-x)
               (define provide/contract39-x (f 10 (λ (x) x)))))
      
      (eval '(require 'provide/contract39-m2))
      (eval 'provide/contract39-x))
   10)
  
  (test/spec-passed/result
   'provide/contract40
   '(begin
      (eval '(module provide/contract40-m1 racket/base
               (require racket/contract)
               (provide/contract
                #:forall x
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))
      
      (eval '(module provide/contract40-m2 racket/base
               (require racket/contract 'provide/contract40-m1)
               (provide provide/contract40-x)
               (define provide/contract40-x (f 10 (λ (x) x)))))
      
      (eval '(require 'provide/contract40-m2))
      (eval 'provide/contract40-x))
   10)
  
  (test/spec-passed/result
   'provide/contract41
   '(begin
      (eval '(module provide/contract41-m1 racket/base
               (require racket/contract)
               (provide/contract
                #:forall (x)
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))
      
      (eval '(module provide/contract41-m2 racket/base
               (require racket/contract 'provide/contract41-m1)
               (provide provide/contract41-x)
               (define provide/contract41-x (f 10 (λ (x) x)))))
      
      (eval '(require 'provide/contract41-m2))
      (eval 'provide/contract41-x))
   10)
  
  (test/spec-passed/result
   'provide/contract42
   '(begin
      (eval '(module provide/contract42-m1 racket/base
               (require racket/contract)
               (define x integer?)
               (define g 11)
               (provide/contract
                [g x]
                #:forall (x)
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))
      
      (eval '(module provide/contract42-m2 racket/base
               (require racket/contract 'provide/contract42-m1)
               (provide provide/contract42-x)
               (define provide/contract42-x (f 10 (λ (x) x)))))
      
      (eval '(require 'provide/contract42-m2))
      (eval 'provide/contract42-x))
   10)
  
  (test/spec-passed/result
   'provide/contract43
   '(begin
      (eval '(module provide/contract43-m1 racket/base
               (require racket/contract)
               (struct spider (legs))
               (provide (contract-out (struct spider ([legs number?]))))))
      
      (eval '(module provide/contract43-m2 racket/base
               (require racket/contract 'provide/contract43-m1)
               (provide provide/contract43-x)
               (define provide/contract43-x
                 (spider-legs
                  (contract (struct/c spider integer?)
                            (spider 121)
                            'pos
                            'neg)))))
      
      (eval '(require 'provide/contract43-m2))
      (eval 'provide/contract43-x))
   121)
  
  (test/spec-passed
   'provide/contract44
   '(begin
      (eval '(module provide/contract44-m1 racket/base
               (require racket/contract)
               (struct heap (v) #:transparent)
               (provide (rename-out (heap new-heap)))))
      
      (eval '(module provide/contract44-m2 racket/base
               (require racket/contract 'provide/contract44-m1)
               (void (contract (struct/c new-heap any/c)
                               (new-heap 121)
                               'pos
                               'neg))))
      
      (eval '(require 'provide/contract44-m2))))
  
  (test/spec-passed/result
   'provide/contract45
   '(begin
      (eval '(module provide/contract45-m1 racket/base
               (require racket/contract)
               (struct heap (v) #:transparent)
               (provide 
                (contract-out 
                 (struct heap ([v integer?]) #:omit-constructor)))
               (define a-heap (heap 11))
               (provide a-heap)))
      
      (eval '(module provide/contract45-m2 racket/base
               (require racket/contract 'provide/contract45-m1)
               (define provide/contract45-x (heap-v a-heap))
               (provide provide/contract45-x)))
      
      (eval '(require 'provide/contract45-m2))
      (eval 'provide/contract45-x))
   11)
  
  (test/spec-passed/result
   'provide/contract46
   '(begin
      (eval '(module provide/contract46-m1 racket/base
               (require racket/contract/base)
               (provide
                (contract-out
                 (struct s ([x any/c]) #:omit-constructor)))
               (struct s (x))
               (define an-s (s 123))
               (provide an-s)))
      (eval '(module provide/contract46-m2 racket/base
               (require 'provide/contract46-m1
                        racket/match)
               (define provide/contract46-x
                 (match an-s
                   [(s x) x]))
               (provide provide/contract46-x)))
      (eval '(require 'provide/contract46-m2))
      (eval 'provide/contract46-x))
   123)
  
  (test/spec-passed/result
   'provide/contract47
   '(begin
      (eval '(module provide/contract47-m1 racket
               (struct the-name-of-my-struct ())
               (provide (contract-out (struct the-name-of-my-struct ())))))
      (eval '(module provide/contract47-m2 racket
               (require 'provide/contract47-m1)
               (define provide/contract47-x (object-name the-name-of-my-struct))
               (provide provide/contract47-x)))
      (eval '(require 'provide/contract47-m2))
      (eval 'provide/contract47-x))
   'the-name-of-my-struct)
  
  (contract-error-test
   'contract-error-test8
   #'(begin
       (eval '(module pce1-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable1 'five)
                (provide/contract [the-defined-variable1 number?])))
       (eval '(require 'pce1-bug)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"the-defined-variable1: broke its contract" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test9
   #'(begin
       (eval '(module pce2-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable2 values)
                (provide/contract [the-defined-variable2 (-> number? any)])))
       (eval '(require 'pce2-bug))
       (eval '(the-defined-variable2 #f)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"the-defined-variable2: contract violation" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test10
   #'(begin
       (eval '(module pce3-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable3 (λ (x) #f))
                (provide/contract [the-defined-variable3 (-> any/c number?)])))
       (eval '(require 'pce3-bug))
       (eval '(the-defined-variable3 #f)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"the-defined-variable3" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test11
   #'(begin
       (eval '(module pce4-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable4 (λ (x) #f))
                (provide/contract [the-defined-variable4 (-> any/c number?)])))
       (eval '(require 'pce4-bug))
       (eval '((if #t the-defined-variable4 the-defined-variable4) #f)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"^the-defined-variable4" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test12
   #'(begin
       (eval '(module pce5-bug scheme/base
                (require scheme/contract)
                
                (define-struct bad (a b))
                
                (provide/contract
                 [struct bad ((string? a) (string? b))])))
       (eval '(require 'pce5-bug)))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"expected field name to be b, but found string?" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test13
   #'(begin
       (eval '(module pce6-bug scheme/base
                (require scheme/contract)
                
                (define-struct bad-parent (a))
                (define-struct (bad bad-parent) (b))
                
                (provide/contract
                 [struct bad ((a string?) (string? b))])))
       (eval '(require 'pce6-bug)))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"expected field name to be b, but found string?" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test14
   #'(begin
       (eval '(module pce7-bug scheme/base
                (require scheme/contract)
                (define x 1)
                (provide/contract [x integer?])))
       (eval '(module pce7-bug2 scheme/base
                (require 'pce7-bug)
                (set! x 5))))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"cannot set!" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test15
   #'(begin
       (eval '(module pce8-bug1 scheme/base
                (require scheme/contract)
                (define (f x) x)
                (provide/contract [f (-> integer? integer? integer?)])))
       (eval '(require 'pce8-bug1)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"pce8-bug" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test16
   #'(begin
       (eval '(module pce9-bug scheme
                (define (f x) "wrong")
                (provide/contract
                 [rename f g
                         (-> number? number?)])))
       (eval '(require 'pce9-bug))
       (eval '(g 12)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"^g.*contract from: pce9-bug" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test17
   #'(begin
       (eval '(module pce10-bug scheme
                (define (f x) "wrong")
                (provide/contract
                 [rename f g
                         (-> number? number?)])))
       (eval '(require 'pce10-bug))
       (eval '(g 'a)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"^g.*contract from: pce10-bug" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test18
   #'(begin
       (eval '(module pce18-bug racket
                (struct point (x y))
                (provide (contract-out
                          (struct point ([x integer?])))))))
   (λ (x)
     (and (exn:fail:syntax? x)
          ;; testing that the error says "contract-out" and not "provide/contract"
          (regexp-match #rx"contract-out: found 2 fields" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test19
   #'(begin
       (eval '(module pce19-bug racket
                (struct point (x y))
                (provide (contract-out
                          (struct point ([x integer?] [y integer?])
                            #:omit-constructor)))))
       (eval '(module pce19-b racket
                (require 'pce19-bug)
                make-point)))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"unbound identifier .* make-point" (exn-message x)))))
  
  (contract-error-test
   'contract-error-test20
   #'(begin
       (eval '(module pce20-bug racket
                (struct point (x y))
                (provide (contract-out
                          (struct point ([x integer?] [y integer?])
                            #:omit-constructor)))))
       (eval '(module pce20-b racket
                (require 'pce20-bug)
                point)))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"point: .* cannot be used as an expression" (exn-message x)))))
  
  (contract-eval
   `(,test
     'pos
     (compose blame-positive exn:fail:contract:blame-object)
     (with-handlers ((void values)) (contract not #t 'pos 'neg))))
  
  
  ;; check that `contract-out' contracts can use contracts
  ;; defined later in the module
  (test/spec-passed/result
   'contract-out1
   '(begin
      (eval '(module contract-out1-m racket/base
               (require racket/contract)
               (provide (contract-out [f (-> ok? ok?)]))
               (define (f x) (+ x 1))
               (define (ok? v) (exact-integer? v))))
      (eval '(require 'contract-out1-m))
      (eval '(f 10)))
   11)
  
  (test/spec-passed/result
   'contract-out2
   '(begin
      (eval '(module contract-out2-m racket/base
               (require racket/contract)
               (provide (contract-out (struct s ([x integer?]))))
               (struct s (x))))
      (eval '(require 'contract-out2-m))
      (eval '(s-x (s 11))))
   11)
  
  ;; expect the syntax errors in the right order
  (contract-syntax-error-test
   'contract-out3
   '(eval '(module contract-out3-m racket/base
             (require racket/contract)
             (provide (contract-out garbage))
             (λ)))
   #rx"contract-out")
  
  (test/pos-blame
   'contract-struct/c-1
   '(begin
      (eval '(module contract-struct/c-1a racket/base
               (struct s (field))
               (provide s)))
      (eval '(module contract-struct/c-1b racket/base
               (require 'contract-struct/c-1a racket/contract)
               (void (contract (struct/c s boolean?)
                               (s 1)
                               'pos 'neg))))
      (eval '(require 'contract-struct/c-1b))))
  
  (test/spec-passed
   'contract-struct/c-2
   '(begin
      (eval '(module contract-struct/c-2a racket/base
               (struct s (field))
               (provide s)))
      (eval '(module contract-struct/c-2b racket/base
               (require 'contract-struct/c-2a racket/contract)
               (void (contract (struct/c s any/c)
                               (s 1)
                               'pos 'neg))))
      (eval '(require 'contract-struct/c-2b))))
  
  (test/spec-passed/result
   'contract-out-rename1
   '(begin
      (eval '(module contract-out-rename1-a racket/base
               (require racket/contract/base)
               (provide (prefix-out my- (contract-out [f any/c])))
               (define f 11)))
      (eval '(module contract-out-rename1-b racket/base
               (require 'contract-out-rename1-a)
               (define contract-out-rename1-my-f my-f)
               (provide contract-out-rename1-my-f)))
      (eval '(require 'contract-out-rename1-b))
      (eval 'contract-out-rename1-my-f))
   11)
  
  (contract-eval
   '(module contract-test-suite-inferred-name1 racket/base
      (require racket/contract)
      (define contract-inferred-name-test-contract (-> integer? any))
      (define (contract-inferred-name-test1 x) #t)
      (provide/contract (contract-inferred-name-test1 contract-inferred-name-test-contract))
      
      (define (contract-inferred-name-test2 x) x)
      (provide/contract (contract-inferred-name-test2 (-> number? number?)))
      
      (define (contract-inferred-name-test2b x) (values x x))
      (provide/contract (contract-inferred-name-test2b (-> number? (values number? number?))))
      
      (define (contract-inferred-name-test3 x . y) x)
      (provide/contract 
       (contract-inferred-name-test3 (->* (number?) () #:rest (listof number?) number?)))
      
      (define (contract-inferred-name-test4) 7)
      (provide/contract (contract-inferred-name-test4 (->d () () any)))
      
      (define (contract-inferred-name-test5) 7)
      (provide/contract (contract-inferred-name-test5 (->i () () any)))
      ))
  (contract-eval '(require 'contract-test-suite-inferred-name1))
  (test 'contract-inferred-name-test1 object-name (contract-eval 'contract-inferred-name-test1))
  (test 'contract-inferred-name-test2 object-name (contract-eval 'contract-inferred-name-test2))
  (test 'contract-inferred-name-test2b object-name (contract-eval 'contract-inferred-name-test2b))
  (test 'contract-inferred-name-test3 object-name (contract-eval 'contract-inferred-name-test3))
  (test 'contract-inferred-name-test4 object-name (contract-eval 'contract-inferred-name-test4))
  (test 'contract-inferred-name-test5 object-name (contract-eval 'contract-inferred-name-test5)))
