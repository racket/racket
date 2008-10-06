(module new-fv-tests mzscheme
  (require "test-utils.ss" "planet-requires.ss")
  (require/private type-rep rep-utils type-effect-convenience meet-join subtype union)
  (require-schemeunit)

  (define variance-gen (random-uniform Covariant Contravariant Invariant Constant))
  
  (define alpha-string (random-string (random-char (random-int-between 65 90)) (random-size 1)))
  
  (define (free-gen var) (random-apply make-immutable-hash-table (random-list-of (random-apply cons var variance-gen) (random-size 1))))
  (define free-var-gen (free-gen (random-symbol alpha-string)))
  (define free-idx-gen (free-gen (random-size)))
  
  (define free-vars-gen (free-gen free-var-gen))
  (define free-idxs-gen (free-gen free-idx-gen))
  
  (define type-gen 
    (random-recursive 
      t
      [10 Univ]
      [10 N]
      [10 B]
      [2 (random-apply make-Pair t t)]
      [2 (random-apply make-Vector t)]
      [2 (random-apply -lst t)]
      [2 (random-apply -Promise t)]
      [1 (random-apply apply Un (random-list-of t))]))

  (define values-gen
     (random-weighted 1 type-gen 6 (random-apply -values (random-list-of type-gen (random-weighted 1 0 3 (random-size 2))))))
  
  
  (define (fvars frees) (hash-table-map frees (lambda (k v) k)))
  (define (subset a b) (andmap (lambda (e) (memq e b)) a))
  
  (define (var-below v w)
    (or (eq? v w) (eq? v Invariant) (eq? w Constant)))
  
  (define (free-var-from frees)
    (let ([keys (map car (generate (random-apply hash-table-map frees list)))])
      (apply choose-uniform keys)))

  (define (fv-tests)
    (test-suite "random tests"
    (test-randomly "combine includes all the elements"
                   100
                   ([A free-vars-gen]
                    [B free-vars-gen]
                    [C free-idxs-gen]
                    [D free-idxs-gen])
                   (let ([C1 (combine-frees (list A B))]
                         [C2 (combine-frees (list C D))])
                     (check-not-false (subset (fvars A) (fvars C1)))
                     (check-not-false (subset (fvars B) (fvars C1)))
                     (check-not-false (subset (fvars C) (fvars C2)))
                     (check-not-false (subset (fvars D) (fvars C2)))))
    (test-randomly "combine produces lower variance"
                   100                   
                   ([A free-vars-gen]
                    [B free-vars-gen]
                    [key (free-var-from A)])
                   (let* ([comb (combine-frees (list A B))]
                          [var1 (hash-table-get A key)]
                          [var2 (hash-table-get comb key)])
                     (check-not-false (var-below var2 var1))))))
  
   
  (define (meet-join-tests) 
    (test-suite 
     "meet join"
     (test-randomly "join of two types is above them"
                    10
                    ([A type-gen]
                     [B type-gen]
                     [A+B (join A B)])
                    (check-not-false (subtype A A+B))
                    (check-not-false (subtype B A+B)))
     (test-randomly "meet of two types is below them"
                    10
                    ([A type-gen]
                     [B type-gen]
                     [A+B (meet A B)])
                    (check-not-false (subtype A+B A))
                    (check-not-false (subtype A+B B)))
     (test-randomly "promote/demote"
                    10
                    ([t type-gen]
                     [V (random-list-of (random-symbol alpha-string))]
                     [p (promote t V)]
                     [d (demote t V)]
                     [fv-p (fv p)]
                     [fv-d (fv d)])
                    (check-false (ormap (lambda (e) (memq e V)) fv-p))
                    (check-false (ormap (lambda (e) (memq e V)) fv-d))
                    (check-not-false (subtype t p))
                    (check-not-false (subtype p d)))))
  
  (define-go fv-tests meet-join-tests)


  )
