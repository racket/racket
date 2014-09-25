#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/list
                                               'racket/class)])
  
  (contract-eval '(define-contract-struct couple (hd tl)))
  (contract-eval '(define-contract-struct triple (a b c)))
  
  (ctest #t contract-stronger? any/c any/c)
  (ctest #t contract-stronger? (between/c 1 3) (between/c 0 4))
  (ctest #f contract-stronger? (between/c 0 4) (between/c 1 3))
  (ctest #t contract-stronger? (>=/c 3) (>=/c 2))
  (ctest #f contract-stronger? (>=/c 2) (>=/c 3))
  (ctest #f contract-stronger? (<=/c 3) (<=/c 2))
  (ctest #t contract-stronger? (<=/c 2) (<=/c 3))
  (ctest #t contract-stronger? (>/c 3) (>/c 2))
  (ctest #f contract-stronger? (>/c 2) (>/c 3))
  (ctest #f contract-stronger? (</c 3) (</c 2))
  (ctest #t contract-stronger? (</c 2) (</c 3))
  (ctest #t contract-stronger? (</c 2) (>/c 2))
  (ctest #t contract-stronger? (</c 2) (<=/c 2))
  (ctest #f contract-stronger? (</c 2) (>=/c 2))
  (ctest #f contract-stronger? (>/c 2) (<=/c 2))
  (ctest #t contract-stronger? (>/c 2) (>=/c 2))
  (ctest #t contract-stronger? (</c 2) (<=/c 200))
  (ctest #f contract-stronger? (<=/c 2) (</c 2))
  (ctest #t contract-stronger? (<=/c 1) (</c 2))
  (ctest #f contract-stronger? (>=/c 2) (</c 2))
  (ctest #f contract-stronger? (<=/c 2) (>/c 2))
  (ctest #f contract-stronger? (>=/c 2) (>/c 2))
  (ctest #t contract-stronger? (>=/c 3) (>/c 2))
  (ctest #t contract-stronger? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 3)))
  (ctest #f contract-stronger? (recursive-contract (<=/c 3)) (recursive-contract (<=/c 2)))
  (let ([f (contract-eval '(λ (x) (recursive-contract (<=/c x))))])
    (test #t (contract-eval 'contract-stronger?) (contract-eval `(,f 1)) (contract-eval `(,f 1))))
  (ctest #t contract-stronger?
         (letrec ([c (recursive-contract (-> (<=/c 5) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c))
  (ctest #t contract-stronger?
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 1) c))]) c))
  (ctest #t contract-stronger?
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 1) (-> (<=/c 1) c)))]) c))
  (ctest #t contract-stronger?
         (letrec ([c (recursive-contract (-> (<=/c 1) (-> (<=/c 1) c)))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 1) (-> (<=/c 1) (-> (<=/c 1) c))))]) c))
  (ctest #t contract-stronger? (-> integer? integer?) (-> integer? integer?))
  (ctest #f contract-stronger? (-> boolean? boolean?) (-> integer? integer?))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 4) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 4) (>=/c 3)) (-> (>=/c 3) (>=/c 3)))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 3) (>=/c 2)))
  (ctest #f contract-stronger? (-> (>=/c 3) (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? 
         (-> integer? #:x integer? integer?)
         (-> integer? #:y integer? integer?))
  (ctest #f contract-stronger?
         (-> integer? #:y integer? integer?)
         (-> integer? #:x integer? integer?))
  (ctest #t contract-stronger?
         (-> integer? #:x integer? integer?)
         (-> integer? #:x integer? integer?))
  (ctest #t contract-stronger? (-> #:x (>=/c 3) (>=/c 3)) (-> #:x (>=/c 3) (>=/c 2)))

  (let ([c (contract-eval '(->* () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))
  (let ([c (contract-eval '(->d () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))
  (let ([c (contract-eval '(->i () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))

  (ctest #f contract-stronger?
         (->* () #:pre (zero? (random 10)) any) 
         (->* () #:pre (zero? (random 10)) any))
  (ctest #f contract-stronger? 
         (->* () integer? #:post (zero? (random 10))) 
         (->* () integer? #:post (zero? (random 10))))
  
  (ctest #t contract-stronger? (or/c null? any/c) (or/c null? any/c))
  (ctest #f contract-stronger? (or/c null? any/c) (or/c boolean? any/c))
  (ctest #t contract-stronger? (or/c null? boolean?) (or/c null? boolean?))
  (ctest #t contract-stronger? (or/c null? boolean?) (or/c boolean? null?))
  (ctest #t contract-stronger? 
         (or/c null? (-> integer? integer?))
         (or/c null? (-> integer? integer?)))
  (ctest #f contract-stronger?
         (or/c null? (-> boolean? boolean?))
         (or/c null? (-> integer? integer?)))
  (ctest #f contract-stronger? (or/c number? #f) number?)
  (ctest #t contract-stronger? number? (or/c number? #f))
  (ctest #f contract-stronger? (or/c (-> number? number?) #f) (-> number? number?))
  (ctest #t contract-stronger? (-> number? number?) (or/c (-> number? number?) #f))
  (ctest #f contract-stronger? (or/c (-> number? number?) (-> number? number? number?) #f) #f)
  (ctest #t contract-stronger? #f (or/c (-> number? number?) (-> number? number? number?) #f))
  (ctest #t contract-stronger? (or/c real?) (or/c integer? real?))
  (ctest #t contract-stronger? (-> number?) (-> (or/c #f number?)))
  (ctest #t contract-stronger? (-> (or/c #f number?) any/c) (-> number? any/c))
  (ctest #f contract-stronger? (-> (or/c #f number?)) (-> number?))
  (ctest #f contract-stronger? (-> number? any/c) (-> (or/c #f number?) any/c))
  
  (ctest #t contract-stronger? number? number?)
  (ctest #f contract-stronger? boolean? number?)

  (ctest #t contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 1 4)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4)) (parameter/c (between/c 0 5)))

  (ctest #f contract-stronger? (parameter/c (between/c 1 4) (between/c 0 5))
                               (parameter/c (between/c 0 5)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5) (between/c 1 4))
                               (parameter/c (between/c 1 4)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5))
                               (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4))
                               (parameter/c (between/c 0 5) (between/c 0 5)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5) (between/c 1 4))
                               (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4) (between/c 0 5))
                               (parameter/c (between/c 0 5) (between/c 1 4)))

  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'x 'y 'z))
  (ctest #f contract-stronger? (symbols 'x 'y 'z) (symbols 'x 'y))
  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'z 'x 'y))
  (ctest #f contract-stronger? (symbols 'z 'x 'y) (symbols 'x 'y))
  (ctest #t contract-stronger? (one-of/c (expt 2 100)) (one-of/c (expt 2 100) 12))

  (ctest #t contract-stronger?
        (or/c (-> (>=/c 3) (>=/c 3)) (-> string?))
        (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?))
        (or/c (-> string?) (-> any/c integer?)))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> any/c integer?))
        (or/c (-> string?) (-> integer? integer?)))
  (ctest #t contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? boolean?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? char?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #t contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> integer? integer?) integer?))
  
  (ctest #t contract-stronger? (cons/c boolean? integer?) (cons/c boolean? integer?))
  (ctest #f contract-stronger? (cons/c boolean? integer?) (cons/c integer? boolean?))
  (ctest #t contract-stronger? (cons/c number? (listof number?)) (non-empty-listof number?))
  (ctest #t contract-stronger? (non-empty-listof number?) (cons/c number? (listof number?)))
  (ctest #t contract-stronger? (cons/c number? (list/c number? number?)) (non-empty-listof number?))
  (ctest #t contract-stronger? (cons/c number? (cons/c number? (listof number?))) (listof number?))
  (ctest #t contract-stronger? 
         (cons/c (<=/c 1) (cons/c (<=/c 2) (listof (<=/c 3)))) 
         (listof (<=/c 4)))
  (ctest #f contract-stronger? (listof number?) (cons/c number? (cons/c number? (listof any/c))))
    
  (ctest #f contract-stronger? (vectorof (<=/c 3)) (vectorof (<=/c 4)))
  (ctest #f contract-stronger? (vectorof (<=/c 3)) (vectorof (<=/c 4)))
  (ctest #t contract-stronger? (vectorof (<=/c 3) #:immutable #t) (vectorof (<=/c 4) #:immutable #t))
  (ctest #t contract-stronger? (vectorof (<=/c 3) #:immutable #t) (vectorof (<=/c 3)))
  (ctest #f contract-stronger? (vectorof (<=/c 3)) (vectorof (<=/c 3) #:immutable #t))
  (ctest #f contract-stronger? (vectorof (<=/c 3)) (vectorof (<=/c 3) #:immutable #f))
  (ctest #t contract-stronger? (vectorof (<=/c 3) #:immutable #f) (vectorof (<=/c 3)))
  (ctest #t contract-stronger? (vectorof (<=/c 3)) (vectorof (<=/c 3)))
  
  (ctest #t contract-stronger? (vector/c (<=/c 3) (<=/c 3) (<=/c 3)) (vectorof (<=/c 3)))
  (ctest #f contract-stronger? (vector/c (<=/c 3) (<=/c 3) (<=/c 3)) (vectorof (<=/c 4)))
  (ctest #f contract-stronger? (vector/c (<=/c 3) (<=/c 3) (<=/c 3)) (vectorof (<=/c 2)))
  (ctest #t contract-stronger? (vector/c (<=/c 3) (<=/c 3)) (vector/c (<=/c 3) (<=/c 3)))
  (ctest #f contract-stronger? (vector/c (<=/c 3) (<=/c 3)) (vector/c (<=/c 3) (<=/c 2)))
  (ctest #f contract-stronger? (vector/c (<=/c 3) (<=/c 2)) (vector/c (<=/c 3) (<=/c 3)))
  (ctest #t contract-stronger? (vector/c (<=/c 3) #:immutable #t) (vector/c (<=/c 3)))
  (ctest #t contract-stronger? (vector/c (<=/c 3) #:immutable #f) (vector/c (<=/c 3)))
  (ctest #f contract-stronger? (vector/c (<=/c 3)) (vector/c (<=/c 3) #:immutable #t))
  (ctest #f contract-stronger? (vector/c (<=/c 3)) (vector/c (<=/c 3) #:immutable #f))
  (ctest #t contract-stronger? (vector/c (<=/c 2) #:immutable #t) (vector/c (<=/c 3) #:immutable #t))
  (ctest #f contract-stronger? (vector/c (<=/c 3) #:immutable #t) (vector/c (<=/c 2) #:immutable #t))
  
  (ctest #t contract-stronger? (box/c (<=/c 3)) (box/c (<=/c 3)))
  (ctest #f contract-stronger? (box/c (<=/c 3)) (box/c (<=/c 2)))
  (ctest #f contract-stronger? (box/c (<=/c 2)) (box/c (<=/c 3)))
  (ctest #t contract-stronger? (box/c (<=/c 2) #:immutable #t) (box/c (<=/c 3) #:immutable #t))
  (ctest #f contract-stronger? (box/c (<=/c 3) #:immutable #t) (box/c (<=/c 2) #:immutable #t))
  (ctest #t contract-stronger? (box/c (<=/c 3) #:immutable #t) (box/c (<=/c 3)))
  (ctest #t contract-stronger? (box/c (<=/c 3) #:immutable #f) (box/c (<=/c 3)))
  (ctest #f contract-stronger? (box/c (<=/c 3)) (box/c (<=/c 3) #:immutable #t))
  (ctest #f contract-stronger? (box/c (<=/c 3)) (box/c (<=/c 3) #:immutable #f))
  
  (ctest #t contract-stronger? (hash/c integer? symbol?) (hash/c integer? symbol?))
  (ctest #f contract-stronger? (hash/c integer? symbol?) (hash/c symbol? integer?))
  (ctest #f contract-stronger? (hash/c (<=/c 2) symbol?) (hash/c (<=/c 3) symbol?))
  (ctest #t contract-stronger?
         (hash/c (<=/c 2) symbol? #:immutable #t)
         (hash/c (<=/c 3) symbol? #:immutable #t))
  (ctest #f contract-stronger?
         (hash/c (<=/c 3) symbol? #:immutable #t)
         (hash/c (<=/c 2) symbol? #:immutable #t))
  (ctest #t contract-stronger?
         (hash/c (<=/c 2) symbol? #:immutable #f)
         (hash/c (<=/c 2) symbol?))
  (ctest #f contract-stronger?
         (hash/c (<=/c 2) symbol?)
         (hash/c (<=/c 2) symbol? #:immutable #f))
  
  (contract-eval
   `(let ()
      (define x (flat-rec-contract x (or/c (cons/c x '()) '())))
      (,test #t contract-stronger? x (or/c (cons/c x '()) '()))))
  
  (ctest #t contract-stronger? "x" string?)
  (ctest #f contract-stronger? string? "x")

  (ctest #t contract-stronger? 1 real?)
  (ctest #f contract-stronger? real? 1)

  (ctest #t contract-stronger? 'x symbol?)
  (ctest #f contract-stronger? symbol? 'x)
  
  (ctest #t contract-stronger?
         (flat-named-contract 'name1 #f)
         (flat-named-contract 'name2 #f))
  (ctest #t contract-stronger?
         (flat-named-contract 'name1 (flat-named-contract 'name2 #f))
         (flat-named-contract 'name3 (flat-named-contract 'name4 #f)))
  (ctest #t contract-stronger? (flat-named-contract 'name1 1) (flat-named-contract 'name2 1))
  (ctest #t contract-stronger? (flat-named-contract 'name1 "x") (flat-named-contract 'name2 "x"))
  (ctest #t contract-stronger? 
         (flat-named-contract 'name2 (regexp "x"))
         (flat-named-contract 'name2 (regexp "x")))
  (ctest #t contract-stronger? (listof (<=/c 3)) (listof (<=/c 5)))
  (ctest #t contract-stronger? (list/c (<=/c 3) (<=/c 3)) (list/c (<=/c 3) (<=/c 3)))
  (ctest #f contract-stronger? (list/c (<=/c 3) (<=/c 3)) (list/c (<=/c 3) (<=/c 3) (<=/c 3)))
  (ctest #f contract-stronger? (list/c (<=/c 3) (<=/c 3) (<=/c 3)) (list/c (<=/c 3) (<=/c 3)))
  (ctest #t contract-stronger? (list/c (<=/c 3) (<=/c 3)) (listof (<=/c 5)))
  (ctest #t contract-stronger? (list/c (<=/c 3) (<=/c 3)) (non-empty-listof (<=/c 5)))
  (ctest #t contract-stronger? (list/c (<=/c 3)) (non-empty-listof (<=/c 5)))
  (ctest #f contract-stronger? (list/c) (non-empty-listof (<=/c 5)))
  (ctest #t contract-stronger? (list/c) (listof (<=/c 5)))
  (ctest #t contract-stronger? (promise/c (<=/c 2)) (promise/c (<=/c 3)))
  (ctest #f contract-stronger? (promise/c (<=/c 3)) (promise/c (<=/c 2)))
  
  (ctest #t contract-stronger? (syntax/c (<=/c 3)) (syntax/c (<=/c 4)))
  (ctest #f contract-stronger? (syntax/c (<=/c 4)) (syntax/c (<=/c 3)))
  
  (ctest #t contract-stronger? (parametric->/c (x) (-> x x)) (parametric->/c (x) (-> x (or/c x #f))))
  (ctest #f contract-stronger? (parametric->/c (x y) (-> x y)) (parametric->/c (x y) (-> x x y)))
  (contract-eval `(define α (new-∀/c)))
  (ctest #t contract-stronger? (-> α α) (-> α (or/c #f α)))
  (ctest #f contract-stronger? (-> α (or/c #f α)) (-> α α))
  
  (ctest #t contract-stronger?
         (class/c (m (-> any/c (<=/c 3))))
         (class/c (m (-> any/c (<=/c 4)))))
  (ctest #f contract-stronger?
         (class/c (m (-> any/c (<=/c 4))))
         (class/c (m (-> any/c (<=/c 3)))))
  (ctest #t contract-stronger?
         (class/c (field [f integer?]))
         (class/c (field [f integer?])))
  (ctest #f contract-stronger?
         (class/c (field [f (<=/c 3)]))
         (class/c (field [f (<=/c 4)])))
  (ctest #f contract-stronger?
         (class/c (field [f (<=/c 4)]))
         (class/c (field [f (<=/c 3)])))
  (ctest #t contract-stronger?
         (class/c (init [f (<=/c 3)]))
         (class/c (init [f (<=/c 4)])))
  (ctest #f contract-stronger?
         (class/c (init [f (<=/c 4)]))
         (class/c (init [f (<=/c 3)])))
  (ctest #t contract-stronger?
         (class/c (inherit [m (-> any/c (<=/c 3))]))
         (class/c (inherit [m (-> any/c (<=/c 4))])))
  (ctest #f contract-stronger?
         (class/c (inherit [m (-> any/c (<=/c 4))]))
         (class/c (inherit [m (-> any/c (<=/c 3))])))
  (ctest #t contract-stronger?
         (class/c (super [m (-> any/c (<=/c 3))]))
         (class/c (super [m (-> any/c (<=/c 4))])))
  (ctest #f contract-stronger?
         (class/c (super [m (-> any/c (<=/c 4))]))
         (class/c (super [m (-> any/c (<=/c 3))])))
  (ctest #t contract-stronger?
         (class/c (inner [m (-> any/c (<=/c 3))]))
         (class/c (inner [m (-> any/c (<=/c 4))])))
  (ctest #f contract-stronger?
         (class/c (inner [m (-> any/c (<=/c 4))]))
         (class/c (inner [m (-> any/c (<=/c 3))])))
  (ctest #t contract-stronger?
         (class/c (override [m (-> any/c (<=/c 3))]))
         (class/c (override [m (-> any/c (<=/c 4))])))
  (ctest #f contract-stronger?
         (class/c (override [m (-> any/c (<=/c 4))]))
         (class/c (override [m (-> any/c (<=/c 3))])))
  (ctest #t contract-stronger?
         (class/c (augment [m (-> any/c (<=/c 3))]))
         (class/c (augment [m (-> any/c (<=/c 4))])))
  (ctest #f contract-stronger?
         (class/c (augment [m (-> any/c (<=/c 4))]))
         (class/c (augment [m (-> any/c (<=/c 3))])))
  (ctest #t contract-stronger?
         (class/c (augride [m (-> any/c (<=/c 3))]))
         (class/c (augride [m (-> any/c (<=/c 4))])))
  (ctest #f contract-stronger?
         (class/c (augride [m (-> any/c (<=/c 4))]))
         (class/c (augride [m (-> any/c (<=/c 3))])))
  (ctest #t contract-stronger?
         (class/c (absent m n))
         (class/c (absent m)))
  (ctest #f contract-stronger?
         (class/c (absent m))
         (class/c (absent m n)))
  (ctest #t contract-stronger?
         (class/c (absent (field f g)))
         (class/c (absent (field f))))
  (ctest #f contract-stronger?
         (class/c (absent (field f)))
         (class/c (absent (field f g))))
  (ctest #f contract-stronger?
         (class/c (absent (field x)))
         (class/c (absent x)))
  (ctest #f contract-stronger?
         (class/c (absent x))
         (class/c (absent (field x))))
  
  (ctest #t contract-stronger?
         (instanceof/c (class/c (m (-> any/c (<=/c 3)))))
         (instanceof/c (class/c (m (-> any/c (<=/c 4))))))
  (ctest #f contract-stronger?
         (instanceof/c (class/c (m (-> any/c (<=/c 4)))))
         (instanceof/c (class/c (m (-> any/c (<=/c 3))))))
  
  (ctest #t contract-stronger? (is-a?/c object%) (is-a?/c object%))
  (ctest #t contract-stronger? (is-a?/c (class object% (super-new))) (is-a?/c object%))
  (ctest #f contract-stronger? (is-a?/c object%) (is-a?/c (class object% (super-new))))
  (contract-eval `(define one-interface<%> (interface ())))
  (contract-eval `(define another-interface<%> (interface (one-interface<%>))))
  (ctest #t contract-stronger? (is-a?/c another-interface<%>) (is-a?/c one-interface<%>))
  (ctest #f contract-stronger? (is-a?/c one-interface<%>) (is-a?/c another-interface<%>))
  (ctest #t contract-stronger? 
         (is-a?/c (class* object% (one-interface<%>) (super-new)))
         (is-a?/c one-interface<%>))
  (ctest #f contract-stronger?
         (is-a?/c one-interface<%>) 
         (is-a?/c (class* object% (one-interface<%>) (super-new))))
  
  (ctest #t contract-stronger? (subclass?/c (class object% (super-new))) (subclass?/c object%))
  (ctest #f contract-stronger? (subclass?/c object%) (subclass?/c (class object% (super-new))))
  (ctest #t contract-stronger?
         (implementation?/c another-interface<%>)
         (implementation?/c one-interface<%>))
  (ctest #f contract-stronger?
         (implementation?/c one-interface<%>)
         (implementation?/c another-interface<%>))
  
  ;; chances are, this predicate will accept "x", but
  ;; we don't want to consider it stronger, since it 
  ;; will not always accept "x".
  (ctest #f contract-stronger? "x" (λ (x) (not (zero? (random 10000)))))

  (contract-eval
   `(let ()
      (define (non-zero? x) (not (zero? x)))
      (define list-of-numbers
        (or/c null?
              (couple/c number?
                        (recursive-contract list-of-numbers))))
      (define (short-list/less-than n)
        (or/c null?
              (couple/c (<=/c n)
                        (or/c null?
                              (couple/c (<=/c n)
                                        any/c)))))
      (define (short-sorted-list/less-than n)
        (or/c null?
              (couple/dc
               [hd (<=/c n)]
               [tl (hd) (or/c null?
                              (couple/c (<=/c hd)
                                        any/c))])))

      (define (sorted-list/less-than n)
        (or/c null?
              (couple/dc
               [hd (<=/c n)]
               [tl (hd) (sorted-list/less-than hd)])))

      ;; for some reason, the `n' makes it harder to optimize. 
      ;; without it, this test isn't as good a test
      (define (closure-comparison-test n)
        (couple/dc
         [hd any/c]
         [tl (hd) any/c]))

      (,test #t contract-stronger? (couple/c any/c any/c) (couple/c any/c any/c))
      (,test #f contract-stronger? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
      (,test #t contract-stronger? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
      (,test #f contract-stronger? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
        (,test #t contract-stronger? ctc ctc))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
        (,test #t contract-stronger? ctc ctc))
      (,test #t contract-stronger? list-of-numbers list-of-numbers)
      (,test #t contract-stronger? (short-list/less-than 4) (short-list/less-than 5))
      (,test #f contract-stronger? (short-list/less-than 5) (short-list/less-than 4))
      (,test #t contract-stronger? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
      (,test #f contract-stronger? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
      (,test #t contract-stronger? (sorted-list/less-than 4) (sorted-list/less-than 5))
      (,test #f contract-stronger? (sorted-list/less-than 5) (sorted-list/less-than 4))
      (,test #t contract-stronger? (closure-comparison-test 4) (closure-comparison-test 5))

      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #t contract-stronger? (mk-c 1) (mk-c 2)))))


  (contract-eval
   `(let ()

      (struct s (a b))
      (struct t (a b))

      (,test #f contract-stronger?
             (struct/dc s
                        [a (>=/c 1)]
                        [b (>=/c 2)])
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)]))
      (,test #t contract-stronger?
              (struct/dc s
                         [a (>=/c 2)]
                         [b (>=/c 3)])
              (struct/dc s
                         [a (>=/c 1)]
                         [b (>=/c 2)]))

      (,test #f contract-stronger?
             (struct/dc s
                    [a number?]
                    [b number?])
             (struct/dc t
                        [a number?]
                        [b number?]))

      (,test #f contract-stronger?
             (struct/dc t
                        [a number?]
                        [b number?])
             (struct/dc s
                        [a number?]
                        [b number?]))
      
      (,test #f
             contract-stronger?
             (struct/dc s
                        [a integer?]
                        [b integer?])
             (struct/dc s
                        [a integer?]
                        [b integer?]
                        #:inv (a b) #f))
      
      (,test #t
             contract-stronger?
             (struct/dc s
                        [a integer?]
                        [b integer?]
                        #:inv (a b) #f)
             (struct/dc s
                        [a integer?]
                        [b integer?]))
      

      (define (mk c)
        (struct/dc s
                   [a (>=/c c)]
                   [b (a) (>=/c a)]))
      (define one (mk 1))
      (define two (mk 2))
      (,test #f contract-stronger? one two)
      (,test #t contract-stronger? two one))))