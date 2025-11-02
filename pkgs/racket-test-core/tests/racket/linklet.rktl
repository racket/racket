
(load-relative "loadtest.rktl")

(Section 'linklet)

(require racket/linklet
         racket/unsafe/ops)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define l (compile-linklet '(linklet
                                  ()
                                  ()
                                4)))
  (test #t linklet? l)
  (test #t instance? (instantiate-linklet l '()))
  (test 4 instantiate-linklet l '() (make-instance 'l))

  (err/rt-test (instantiate-linklet l) exn:fail:contract:arity?)
  (err/rt-test (instantiate-linklet l '#()))
  (err/rt-test (instantiate-linklet l (list 5)))
  (err/rt-test (instantiate-linklet l '() 5)))

(parameterize ([current-compile-target-machine #f])
  (define c (compile '(module m '#%kernel
                        (#%provide b)
                        (define-values (b) (+ 1 2)))))
  (test #t 'bundle? (linklet-bundle? c))
  (define l (hash-ref (linklet-bundle->hash c) 0))
  (test #t 'linklet? (linklet? l))
  (test '(linklet ((.get-syntax-literal!) (.set-transformer!))
             (b)
           (void)
           (define-values (b) (+ 1 2)) (void))
        'decompile (decompile-linklet l)))

(parameterize ([current-compile-target-machine #f])
  (define c (compile '(module m '#%kernel
                        (#%provide b)
                        (define-values (b) (+ 1 2))
                        (module sub '#%kernel))))
  (test #t 'directory? (linklet-directory? c))
  (define l (hash-ref (linklet-bundle->hash (hash-ref (linklet-directory->hash c) #f)) 0))
  (test #t 'linklet? (linklet? l))
  (test '(linklet ((.get-syntax-literal!) (.set-transformer!))
             (b)
           (void)
           (define-values (b) (+ 1 2)) (void))
        'decompile (decompile-linklet l)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([inst (make-instance 'mutable)])
  (instance-set-variable-value! inst 'x 10)
  (instance-set-variable-value! inst 'y 20)
  (instance-set-variable-value! inst 'x 15)
  (test '(x y) sort (instance-variable-names inst) symbol<?)
  (instance-unset-variable! inst 'y)
  (define names (instance-variable-names inst))
  (test 15 instance-variable-value inst 'x)
  (test 'missing instance-variable-value inst 'y (lambda () 'missing))
  (test '(x) values names))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([c (datum->correlated 'foo)])
  (define bad (gensym))
  (define c2
    (correlated-property
     (correlated-property
      (correlated-property c 123 'number)
      'good
      'GOOD)
     bad
     'BAD))
  (test 'GOOD correlated-property c2 'good)
  (test 'BAD correlated-property c2 bad)
  (test 'number correlated-property c2 123)
  (test '() correlated-property-symbol-keys c)
  (test '(good) correlated-property-symbol-keys c2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([x (datum->correlated 'x)])
  (struct thing (a b) #:prefab)
  (define c (datum->correlated (list x
                                     (vector x 'y)
                                     (box x) (box 'y)
                                     (hash 'x x 'y x 'z 'z)
                                     (hashalw 'x x)
                                     (thing 1 x))))
  (test '(x
          #(x y)
          #&x #&y
          #hash((x . x) (y . x) (z . z))
          #hashalw((x . x))
          #s(thing 1 x))
        correlated->datum
        c)
  (define (check-c->d-eq v)
    (test #t eq? v (correlated->datum (datum->correlated v)))
    (test #t eq? v (correlated->datum v)))
  (check-c->d-eq (cons 'x 'y))
  (check-c->d-eq (box 'x))
  (check-c->d-eq (vector 'x 'y))
  (check-c->d-eq (hash 'x 'y))
  (check-c->d-eq (thing 'x 'y)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
