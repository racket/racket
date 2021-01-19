;;; cptypes-lattice.ss
;;; Copyright 1984-2020 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(module cptypes-lattice
 (predicate-implies?
  predicate-disjoint?
  predicate-intersect
  predicate-union
  make-pred-$record/rtd
  pred-$record/rtd-rtd
  make-pred-$record/ref
  pred-$record/ref-ref)

  (define-record-type pred-$record/rtd
    (fields rtd)
    (nongenerative #{pred-$record/rtd wnquzwrp8wl515lhz2url8sjc-0})
    (sealed #t))

  (define-record-type pred-$record/ref
    (fields ref)
    (nongenerative #{pred-$record/ref zc0e8e4cs8scbwhdj7qpad6k3-0})
    (sealed #t))


  (define (check-constant-is? x pred?)
    (and (Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d) (pred? d)]
           [else #f])))

  (define (check-constant-eqv? x v)
    (and (Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d) (eqv? d v)]
           [else #f])))

  ;only false-rec, boolean and ptr may be '#f
  ;use when the other argument is truthy bur not exactly '#t
  (define (union/true x)
     (cond
       [(or (eq? x 'boolean)
            (check-constant-eqv? x #f))
        'ptr]
       [else
        'true]))

  (define (union/simple x pred? y)
     (cond
       [(or (check-constant-is? x pred?)
            (eq? x y))
        y]
       [else
        (union/true x)]))

  (define (union/symbol x pred? y)
     (cond
       [(or (check-constant-is? x pred?)
            (eq? x y))
        y]
       [(or (eq? x 'gensym)
		    (eq? x 'interned-symbol)
		    (eq? x 'uninterned-symbol)
	    	(eq? x 'symbol)
		    (check-constant-is? x symbol?))
		'symbol]
       [else
        (union/true x)]))

  (define (union/record x)
     (cond
       [(or (pred-$record/rtd? x)
            (pred-$record/ref? x)
	    	(eq? x '$record))
		'$record]
       [else
        (union/true x)]))

  (define (union/fixnum x)
    (cond 
 	  [(check-constant-is? x target-fixnum?)
 	   'fixnum]
	  [(or (eq? x 'bignum)
		   (eq? x 'exact-integer)
		   (check-constant-is? x exact-integer?))
	   'exact-integer]
	  [(or (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x real?))
 	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/bignum x)
    (cond 
 	  [(check-constant-is? x target-bignum?)
	   'bignum]
	  [(or (eq? x 'fixnum)
	 	   (eq? x 'exact-integer)
	 	   (check-constant-is? x exact-integer?))
	   'exact-integer]
	  [(or (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/exact-integer x)
    (cond 
	  [(or (eq? x 'fixnum)
		   (eq? x 'bignum)
		   (check-constant-is? x exact-integer?))
	  'exact-integer]
	  [(or (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/flonum x)
    (cond 
 	  [(or (check-constant-is? x flonum?))
	   'flonum]
	  [(or (eq? x 'real)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/real x)
    (cond 
	  [(or (eq? x 'fixnum)
		   (eq? x 'bignum)
		   (eq? x 'exact-integer)
		   (eq? x 'flonum)
		   (check-constant-is? x real?))
	   'real]
	  [(or (eq? x 'number)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  (define (union/number x)
	(cond 
	  [(or (eq? x 'fixnum)
		   (eq? x 'bignum)
		   (eq? x 'exact-integer)
		   (eq? x 'flonum)
		   (eq? x 'real)
		   (check-constant-is? x number?))
	   'number]
	  [else
	   (union/true x)]))

  ;true when x is an ancestor of y
  (define (rtd-ancestor? x y)
	(or (eqv? x y)
	    (let ([y (record-type-parent y)])
		   (and y
			    (rtd-ancestor? x y)))))

  ; list on ancestors of x, includes x as the last element
  (define (rtd-ancestors x)
	(let loop ([x x] [r '()])
	  (if x 
	     (loop (record-type-parent x) (cons x r))
	     r)))
 
  (define (rdt-last-common-ancestor x y)
    (define xa (rtd-ancestors x))
    (define ya (rtd-ancestors y))
    (let loop ([xa xa] [ya ya] [r #f])
      (if (or (null? xa)
              (null? ya)
              (not (eqv? (car xa) (car ya))))
        r
        (loop (cdr xa) (cdr ya) (car xa)))))

  (define (bottom? x)
    #f)

  (define (exact-integer? x)
    (and (integer? x) (exact? x)))

  (define (interned-symbol? x)
    (and (symbol? x)
         (not (gensym? x))
         (not (uninterned-symbol? x))))

  ;if x and y are equivalent, the result must be eq? to y
  ;so it's easy to test in predicate-implies?
  ;the result may be bigger than the actual union 
  (define (predicate-union/new x y)
    (cond
      [(eq? x y) y]
      [(not x) #f] ;possible a multivalued expression
      [(not y) #f] ;possible a multivalued expression
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(eq? y 'ptr) y]
      [(eq? x 'ptr) x]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             y]
            [(not dy)
             (cond 
               [(or (eq? x 'boolean)
                    (check-constant-eqv? x #t))
                'boolean]
               [else
                'ptr])]
            [(eq? dy #t)
             (cond 
               [(or (eq? x 'boolean)
                    (check-constant-eqv? x #f))
                'boolean]
               [else
                'true])]
            [(null? dy)
             (cond 
               [(or (eq? x 'null-or-pair)
                    (eq? x 'pair))
                'null-or-pair]
               [else
                (union/true x)])]
			[(fixnum? dy)
  			 (union/fixnum x)]
			[(bignum? dy)
  			 (union/bignum x)]
		  	[(exact-integer? dy)
  			 (union/exact-integer x)]
		  	[(flonum? dy)
  			 (union/flonum x)]
			[(real? dy)
  			 (union/real x)]
			[(number? dy)
  			 (union/number x)]
            [(gensym? dy) (union/symbol x gensym? 'gensym)]
            [(uninterned-symbol? dy) (union/symbol x uninterned-symbol? 'uninterned-symbol)]
            [(interned-symbol? dy) (union/symbol x interned-symbol? 'interned-symbol)]
            [(char? dy) (union/simple x char? 'char)]
            [(vector? dy) (union/simple x vector? 'vector)]; i.e. #()
            [(string? dy) (union/simple x string? 'string)]; i.e. ""
            [(bytevector? dy) (union/simple x bytevector? 'bytevector)] ; i.e. '#vu8()
            [(fxvector? dy) (union/simple x fxvector? 'fxvector)] ; i.e. '#vfx()
            [(flvector? dy) (union/simple x flvector? 'flvector)] ; i.e. '#vfl()
            [else
             (union/true x)])])]
      [(pred-$record/rtd? y)
       (cond
         [(pred-$record/rtd? x)
          (let ([x-rtd (pred-$record/rtd-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (cond
              [(eqv? x-rtd y-rtd)
               y]
              [(record-type-sealed? x-rtd)
               (if (rtd-ancestor? y-rtd x-rtd) y '$record)]
              [(record-type-sealed? y-rtd)
               (if (rtd-ancestor? x-rtd y-rtd) x '$record)]
              [else
               (let ([lca-rtd (rdt-last-common-ancestor x-rtd y-rtd)])
                 (cond
                   [(not lca-rtd) '$record]
                   [(eqv? lca-rtd y-rtd) y]
                   [(eqv? lca-rtd x-rtd) x]
                   [else (make-pred-$record/rtd lca-rtd)]))]))]
         [else (union/record x)])]
      [(pred-$record/ref? y)
       (cond
         [(pred-$record/ref? x)
          (if (eq? (pred-$record/ref-ref x)
                   (pred-$record/ref-ref y))
             y
             '$record)]
         [else (union/record x)])]
      [else
       (case y
         [($record)
          (union/record x)] ; y must be the symbol '$record
         [(null-or-pair)
          (cond 
            [(or (eq? x 'pair)
                 (check-constant-eqv? x '()))
             y]
            [else (union/true x)])]
         [(pair)
          (cond 
            [(or (eq? x 'null-or-pair)
                 (check-constant-eqv? x '()))
             'null-or-pair]
            [else
             (union/true x)])]
		 [(fixnum)
  		  (union/fixnum x)]
		 [(bignum)
  		  (union/bignum x)]
		 [(exact-integer)
  		  (union/exact-integer x)]
		 [(flonum)
  		  (union/flonum x)]
		 [(real)
  		  (union/real x)]
		 [(number)
  		  (union/number x)]
         [(gensym)
          (union/symbol x gensym? 'gensym)]
         [(uninterned-symbol)
          (union/symbol x uninterned-symbol? 'uninterned-symbol)]
         [(interned-symbol)
          (union/symbol x interned-symbol? 'interned-symbol)]
         [(symbol)
          (union/symbol x symbol? 'symbol)]
         [(boolean)
          (cond 
            [(check-constant-is? x boolean?)
             y]
            [else
             'ptr])]
         [(char) (union/simple x char? y)]
         [(vector) (union/simple x vector? y)]; i.e. #()
         [(string) (union/simple x string? y)]; i.e. ""
         [(bytevector) (union/simple x bytevector? y)] ; i.e. '#vu8()
         [(fxvector) (union/simple x fxvector? y)] ; i.e. '#vfx()
         [(flvector) (union/simple x flvector? y)] ; i.e. '#vfl()
         [else (union/true x)])]))

  (define (intersect/simple x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(or (eq? x qpred)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/record x y)
    (cond
      [(or (pred-$record/ref? x)
           (pred-$record/rtd? x))
       x]
      [(or (eq? x '$record)
           (eq? x 'true))
       y]
      [else
       'bottom]))

  (define (intersect/symbol x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(or (eq? x qpred)
            (eq? x 'symbol)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/fixnum x check? y)
     (cond
       [(and check? (check-constant-is? x fixnum?))
        x]
       [(or (eq? x 'fixnum)
            (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/bignum x check? y)
     (cond
       [(and check? (check-constant-is? x bignum?))
        x]
       [(or (eq? x 'bignum)
            (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/exact-integer x check? y)
     (cond
       [(and check? (or (check-constant-is? x exact-integer?)
                        (eq? x 'fixnum)
                        (eq? x 'bignum)))
        x]
       [(or (eq? x 'exact-integer)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/flonum x check? y)
     (cond
       [(and check? (check-constant-is? x flonum?))
        x]
       [(or (eq? x 'flonum)
            (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/real x check? y)
     (cond
       [(and check? (or (check-constant-is? x real?)
                        (eq? x 'fixnum)
                        (eq? x 'bignum)
                        (eq? x 'exact-integer)
                        (eq? x 'flonum)))
        x]
       [(or (eq? x 'real)
            (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  (define (intersect/number x check? y)
     (cond
       [(and check? (eq? x 'fixnum))
        x]
       [(and check? (or (check-constant-is? x number?)
                        (eq? x 'fixnum)
                        (eq? x 'bignum)
                        (eq? x 'exact-integer)
                        (eq? x 'flonum)
                        (eq? x 'real)))
        x]
       [(or (eq? x 'number)
            (eq? x 'true))
        y]
       [else
        'bottom]))

  ;the result may be bigger than the actual intersection 
  ;if there is no exact result, it must be at lest included in x
  ;so it's possible to make decreasing sequences
  (define (predicate-intersect/new x y)
    (cond
      [(eq? x y) x]
      [(not y) x]
      [(not x) y]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y 'ptr) x]
      [(eq? x 'ptr) y]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             x]
            [(not dy)
             (cond 
               [(eq? x 'boolean)
                y]
               [else
                'bottom])]
            [(eq? dy #t)
             (cond 
               [(or (eq? x 'boolean)
                    (eq? x 'true))
                y]
               [else
                'bottom])]
            [(null? dy)
             (cond 
               [(or (eq? x 'null-or-pair)
                    (eq? x 'true))
                y]
               [else
                'bottom])]
			[(fixnum? dy)
  			 (intersect/fixnum x #f y)]
			[(bignum? dy)
  			 (intersect/bignum x #f y)]
		  	[(exact-integer? dy)
  			 (intersect/exact-integer x #f y)]
		  	[(flonum? dy)
  			 (intersect/flonum x #f y)]
			[(real? dy)
  			 (intersect/real x #f y)]
			[(number? dy)
  			 (intersect/number x #f y)]
            [(gensym? dy) (intersect/symbol x #f 'gensym y)]
            [(uninterned-symbol? dy) (intersect/symbol x #f 'uninterned-symbol y)]
            [(interned-symbol? dy) (intersect/symbol x #f 'interned-symbol y)]
            [(char? dy) (intersect/simple x #f 'char y)]
            [(vector? dy) (intersect/simple x #f 'vector y)]; i.e. #()
            [(string? dy) (intersect/simple x #f 'string y)]; i.e. ""
            [(bytevector? dy) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
            [(fxvector? dy) (intersect/simple x #f 'fxvector y)] ; i.e. '#vfx()
            [(flvector? dy) (intersect/simple x #f 'flvector y)] ; i.e. '#vfl()
            [else
             (cond 
               [(eq? x 'true)
                y]
               [else
                'bottom])])])]
      [(pred-$record/rtd? y)
       (cond
         [(pred-$record/rtd? x)
          (let ([x-rtd (pred-$record/rtd-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (cond
              [(eqv? x-rtd y-rtd)
               x]
              [(record-type-sealed? x-rtd)
               (if (rtd-ancestor? y-rtd x-rtd) x 'bottom)]
              [(record-type-sealed? y-rtd)
               (if (rtd-ancestor? x-rtd y-rtd) y 'bottom)]
              [else
               (cond
                 [(rtd-ancestor? y-rtd x-rtd) x]
                 [(rtd-ancestor? x-rtd y-rtd) y]
                 [else 'bottom])]))]
         [else
          (intersect/record x y)])]
      [(pred-$record/ref? y)
       (intersect/record x y)]
      [else
       (case y
         [($record)
          (intersect/record x y)]
         [(null-or-pair)
          (cond 
            [(eq? x 'pair)
             'pair]
            [(check-constant-eqv? x '())
             x]
            [(eq? x 'true)
             'null-or-pair]
            [else 'bottom])]
         [(pair)
          (cond 
            [(or (eq? x 'null-or-pair)
                 (eq? x 'true))
             'pair]
            [else
             'bottom])]
		 [(fixnum)
  		  (intersect/fixnum x #t y)]
		 [(bignum)
  		  (intersect/bignum x #t y)]
		 [(exact-integer)
  		  (intersect/exact-integer x #t y)]
		 [(flonum)
  		  (intersect/flonum x #t y)]
		 [(real)
  		  (intersect/real x #t y)]
		 [(number)
  		  (intersect/number x #t y)]
         [(gensym)
          (intersect/symbol x gensym? 'gensym y)]
         [(uninterned-symbol)
          (intersect/symbol x uninterned-symbol? 'uninterned-symbol y)]
         [(interned-symbol)
          (intersect/symbol x interned-symbol? 'interned-symbol y)]
         [(symbol)
          (cond 
            [(or (eq? x 'gensym)
                 (eq? x 'uninterned-symbol)
                 (eq? x 'interned-symbol)
                 (eq? x 'symbol)
                 (eq? x 'true)
                 (check-constant-is? x symbol?))
             x]
            [else
             'bottom])]
         [(boolean)
          (cond 
            [(eq? x 'true)
             true-rec]
            [(check-constant-is? x boolean?)
             x]
            [else
             'bottom])]
         [(true)
          (cond 
            [(eq? x 'boolean)
             true-rec]
            [(check-constant-eqv? x #f)
             'bottom]
            [else
             x])]
         [(char) (intersect/simple x char? 'char y)]
         [(vector) (intersect/simple x vector? 'vector y)]; i.e. #()
         [(string) (intersect/simple x string? 'string y)]; i.e. ""
         [(bytevector) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
         [(fxvector) (intersect/simple x fxvector? 'fxvector y)] ; i.e. '#vfx()
         [(flvector) (intersect/simple x flvector? 'flvector y)] ; i.e. '#vfl()
         [else
          (cond 
            [(eq? x 'true)
             y]
            [else
             'bottom])])]))

  (define (predicate-disjoint?/old x y)
    (and x
         y
         ; a pred-$record/ref may be any other kind or record
         (not (and (pred-$record/ref? x)
                   (predicate-implies? y '$record)))
         (not (and (pred-$record/ref? y)
                   (predicate-implies? x '$record)))
         ; boolean and true may be a #t
         (not (and (eq? x 'boolean)
                   (eq? y 'true)))
         (not (and (eq? y 'boolean)
                   (eq? x 'true)))
         ; the other types are included or disjoint
         (not (predicate-implies? x y))
         (not (predicate-implies? y x))))

  (define (predicate-intersect/old x y)
    (cond
      [(predicate-implies? x y) x]
      [(predicate-implies? y x) y]
      [(predicate-disjoint? y x)
       'bottom]
      [(or (and (eq? x 'boolean) (eq? y 'true))
           (and (eq? y 'boolean) (eq? x 'true)))
       true-rec]
      [else (or x y)])) ; if there is no exact option, at least keep the old value

  ; strange properties of bottom here:
  ; (implies? x bottom): only for x=bottom
  ; (implies? bottom y): always
  ; (implies-not? x bottom): never
  ; (implies-not? bottom y): never
  ; check (implies? x bottom) before (implies? x something)
  (define (predicate-implies?/new x y)
    (eq? (predicate-union/new x y) y))

  (define (predicate-disjoint?/new x y)
    (eq? (predicate-intersect/new x y) 'bottom))

  ; strange properties of bottom here:
  ; (implies? x bottom): only for x=bottom
  ; (implies? bottom y): always
  ; (implies-not? x bottom): never
  ; (implies-not? bottom y): never
  ; check (implies? x bottom) before (implies? x something)
  (define (predicate-implies?/old x y)
    (and x
         y
         (or (eq? x y)
             (eq? x 'bottom)
             (cond
               [(Lsrc? y)
                (and (Lsrc? x)
                     (nanopass-case (Lsrc Expr) y
                       [(quote ,d1)
                        (nanopass-case (Lsrc Expr) x
                          [(quote ,d2) (eqv? d1 d2)]
                          [else #f])]
                       [else #f]))]
               [(pred-$record/rtd? y)
                (and (pred-$record/rtd? x)
                     (let ([x-rtd (pred-$record/rtd-rtd x)]
                           [y-rtd (pred-$record/rtd-rtd y)])
                       (cond
                         [(record-type-sealed? y-rtd)
                          (eqv? x-rtd y-rtd)]
                         [else
                          (let loop ([x-rtd x-rtd])
                            (or (eqv? x-rtd y-rtd)
                                (let ([xp-rtd (record-type-parent x-rtd)])
                                  (and xp-rtd (loop xp-rtd)))))])))]
               [(pred-$record/ref? y)
                (and (pred-$record/ref? x)
                     (eq? (pred-$record/ref-ref x)
                          (pred-$record/ref-ref y)))]
               [(case y
                  [(null-or-pair) (or (eq? x 'pair)
                                      (check-constant-is? x null?))]
                  [(fixnum) (check-constant-is? x target-fixnum?)]
                  [(bignum) (check-constant-is? x target-bignum?)]
                  [(exact-integer)
                   (or (eq? x 'fixnum)
                       (eq? x 'bignum)
                       (check-constant-is? x (lambda (x) (and (integer? x)
                                                              (exact? x)))))]
                  [(flonum) (check-constant-is? x flonum?)]
                  [(real) (or (eq? x 'fixnum)
                              (eq? x 'bignum)
                              (eq? x 'exact-integer)
                              (eq? x 'flonum)
                              (check-constant-is? x real?))]
                  [(number) (or (eq? x 'fixnum)
                                (eq? x 'bignum)
                                (eq? x 'exact-integer)
                                (eq? x 'flonum)
                                (eq? x 'real)
                                (check-constant-is? x number?))]
                  [(gensym) (check-constant-is? x gensym?)]
                  [(uninterned-symbol) (check-constant-is? x uninterned-symbol?)]
                  [(interned-symbol) (check-constant-is? x (lambda (x)
                                                             (and (symbol? x)
                                                                  (not (gensym? x))
                                                                  (not (uninterned-symbol? x)))))]
                  [(symbol) (or (eq? x 'gensym)
                                (eq? x 'uninterned-symbol)
                                (eq? x 'interned-symbol)
                                (check-constant-is? x symbol?))]
                  [(char) (check-constant-is? x char?)]
                  [(boolean) (check-constant-is? x boolean?)]
                  [(true) (and (not (check-constant-is? x not))
                               (not (eq? x 'boolean))
                               (not (eq? x 'ptr)))] ; only false-rec, boolean and ptr may be `#f
                  [($record) (or (pred-$record/rtd? x)
                                 (pred-$record/ref? x)
                                 (check-constant-is? x #3%$record?))]
                  [(vector) (check-constant-is? x vector?)] ; i.e. '#()
                  [(string) (check-constant-is? x string?)] ; i.e. ""
                  [(bytevector) (check-constant-is? x bytevector?)] ; i.e. '#vu8()
                  [(fxvector) (check-constant-is? x fxvector?)] ; i.e. '#vfx()
                  [(flvector) (check-constant-is? x flvector?)] ; i.e. '#vfl()
                  [(ptr) #t]
                  [else #f])]
               [else #f]))))

  (define (predicate-union/old x y)
    (cond
      [(predicate-implies?/old y x) x]
      [(predicate-implies?/old x y) y]
      [(find (lambda (t)
               (and (predicate-implies?/old x t)
                    (predicate-implies?/old y t)))
             '(char null-or-pair $record
               gensym uninterned-symbol interned-symbol symbol
               fixnum bignum exact-integer flonum real number
               boolean true ptr))] ; ensure they are order from more restrictive to less restrictive
      [else #f]))

  (define predicate-union predicate-union/new)
  (define predicate-intersect predicate-intersect/new)

  (define (predicate-implies? x y)
    (define o (predicate-implies?/old x y))
    (define n (predicate-implies?/new x y))
    (define u (predicate-union/new x y))
    (unless (eq? o n)
      (newline)
      (display (list '************ x y o n u))
      (newline)
      (when (and (pred-$record/rtd? x)
                 (pred-$record/rtd? y))
        (display (rtd-ancestors (pred-$record/rtd-rtd x)))
        (newline)
        (display (rtd-ancestors (pred-$record/rtd-rtd y)))
        (newline)
        (display (list (rtd-ancestor? (pred-$record/rtd-rtd y)
                                      (pred-$record/rtd-rtd x))
                       (rtd-ancestor? (pred-$record/rtd-rtd x)
                                      (pred-$record/rtd-rtd y))))
        (newline)))
    n)

  (define (predicate-disjoint? x y)
    (define o (predicate-disjoint?/old x y))
    (define n (predicate-disjoint?/new x y))
    (define u (predicate-intersect/new x y))
    (unless (or (eq? o n) (eq? x 'bottom))
      (newline)
      (display (list '+++++++++++++ x y o n u))
      (newline)
      (when (and (pred-$record/rtd? x)
                 (pred-$record/rtd? y))
        (display (rtd-ancestors (pred-$record/rtd-rtd x)))
        (newline)
        (display (rtd-ancestors (pred-$record/rtd-rtd y)))
        (newline)
        (display (list (rtd-ancestor? (pred-$record/rtd-rtd y)
                                      (pred-$record/rtd-rtd x))
                       (rtd-ancestor? (pred-$record/rtd-rtd x)
                                      (pred-$record/rtd-rtd y))))
        (newline)))
    n)
)
