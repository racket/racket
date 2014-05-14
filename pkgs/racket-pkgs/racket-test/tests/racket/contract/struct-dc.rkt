#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))
  
  (test/spec-passed
   'struct/c1
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                (make-s 1)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/c2
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                (make-s #f)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/c3
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                1
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/c4
   '(let ()
      (define-struct s (a b))
      (contract (struct/c s integer? (struct/c s integer? boolean?))
                (make-s 1 (make-s 2 #t))
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/c5
   '(let ()
      (define-struct s (a b))
      (contract (struct/c s integer? (struct/c s integer? boolean?))
                (make-s 1 (make-s 2 3))
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/c6
   '(let ()
      (define-struct s (f))
      (let ([v (contract (struct/c s (-> number? number?))
                         (make-s values)
                         'pos
                         'neg)])
        ((s-f v) 3))))
  
  (test/neg-blame
   'struct/c7
   '(let ()
      (define-struct s (f))
      (let ([v (contract (struct/c s (-> number? number?))
                         (make-s values)
                         'pos
                         'neg)])
        ((s-f v) #f))))
  
  (test/pos-blame
   'struct/c8
   '(let ()
      (define-struct s (f))
      (let ([v (contract (struct/c s (-> number? number?))
                         (make-s (λ (v) #f))
                         'pos
                         'neg)])
        ((s-f v) 3))))
  
  (test/spec-passed
   'struct/c9
   '(let ()
      (define-struct s (a b) #:mutable)
      (let ([v (contract (struct/c s integer? boolean?)
                         (make-s 3 #t)
                         'pos
                         'neg)])
        (set-s-a! v 4)
        (set-s-b! v #t))))
  
  (test/neg-blame
   'struct/c10
   '(let ()
      (define-struct s (a b) #:mutable)
      (let ([v (contract (struct/c s integer? boolean?)
                         (make-s 3 #t)
                         'pos
                         'neg)])
        (set-s-a! v #f))))
  
  (test/neg-blame
   'struct/c11
   '(let ()
      (define-struct s (a [b #:mutable]))
      (let ([v (contract (struct/c s integer? boolean?)
                         (make-s 3 #t)
                         'pos
                         'neg)])
        (set-s-b! v 5))))
  
  (test/spec-passed/result
   'struct/c12
   '(let ()
      (define-struct s (a) #:mutable)
      (define alpha (new-∃/c 'alpha))
      (define v (make-s 3))
      (let ([v* (contract (struct/c s alpha) v 'pos 'neg)])
        (set-s-a! v* (s-a v*)))
      (s-a v))
   3)
  
  (test/neg-blame
   'struct/c13
   '(let ()
      (define-struct s (a) #:mutable)
      (define alpha (new-∃/c 'alpha))
      (define v (make-s 3))
      (let ([v* (contract (struct/c s alpha) v 'pos 'neg)])
        (set-s-a! v* 4))))
  
  (test/spec-passed/result
   'struct/c14
   '(let ()
      (struct heap (v))
      (struct heap-node heap ())
      
      (heap-v (contract (struct/c heap-node number?) 
                        (heap-node 11)
                        'pos
                        'neg)))
   11)
  
  (test/spec-passed/result
   'struct/c15
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (b-y (contract (struct/c d number? number? number? number?) 
                     (d 11 22 33 44)
                     'pos
                     'neg)))
   22)
  
  (test/spec-passed/result
   'struct/c16
   '(let ()
      (struct doll (contents))
      (list ((flat-contract-predicate (struct/c doll 'center)) (doll 'center))
            ((flat-contract-predicate (struct/c doll 'center)) (doll 'not-center-center))))
   '(#t #f))
  
  (contract-error-test
   'struct/c17
   '(let ()
      (struct foo (v))
      (contract (struct/c foo number?)
                #f
                'pos
                'neg))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"promised: foo?" (exn-message x)))))
  
  
  ;
  ;
  ;
  ;
  ;           ;                         ;    ;    ;;;
  ;         ;;;                       ;;;    ;    ;;;
  ;   ;;;;  ;;;; ;;; ;;;; ;;;   ;;;   ;;;;  ;  ;; ;;;   ;;;
  ;  ;;; ;; ;;;; ;;;;;;;; ;;;  ;;;;;  ;;;;  ; ;;;;;;;  ;;;;;
  ;  ;;;    ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;   ; ;;; ;;; ;;;  ;;
  ;   ;;;;  ;;;  ;;;  ;;; ;;; ;;;     ;;;   ; ;;; ;;; ;;;
  ;     ;;; ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;   ; ;;; ;;; ;;;  ;;
  ;  ;; ;;; ;;;; ;;;  ;;;;;;;  ;;;;;  ;;;; ;  ;;;;;;;  ;;;;;
  ;   ;;;;   ;;; ;;;   ;; ;;;   ;;;    ;;; ;   ;; ;;;   ;;;
  ;
  ;
  ;
  ;
  
  
  (test/spec-passed
   'struct/dc-1
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a () number?]
                           [b (a) boolean?])
                (s 1 #f)
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/dc-1a
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a () number?]
                           [b (a) #:chaperone boolean?])
                (s 1 #f)
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/dc-2
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a () number?]
                           [b (a) (>=/c a)])
                (s 1 2)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/dc-3
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a () number?]
                           [b (a) (>=/c a)])
                (s 2 1)
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/dc-3b
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a () number?]
                           [b (a) (<=/c a)])
                (s 2 1)
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/dc-4
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a number?]
                           [b (a) (>=/c a)])
                (s 1 2)
                'pos
                'neg)))
  
  
  (test/pos-blame
   'struct/dc-5
   '(let ()
      (struct s (a b))
      (s-b (contract (struct/dc s
                                [a () number?]
                                [b (a) (>=/c a)])
                     (s 2 1)
                     'pos
                     'neg))))
  
  (test/spec-passed/result
   'struct/dc-6
   '(let ()
      (struct s (a b))
      (define-opt/c (f z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (f a)]))
      
      (s-a (contract (f 11)
                     (s 12 (s 13 #f))
                     'pos
                     'neg)))
   12)
  
  (test/spec-passed/result
   'struct/dc-7
   '(let ()
      (struct s (a b))
      (define-opt/c (f z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (f a)]))
      
      (s-a (s-b (contract (f 11)
                          (s 12 (s 13 #f))
                          'pos
                          'neg))))
   13)
  
  
  (test/pos-blame
   'struct/dc-8
   '(let ()
      (struct s (a b))
      (define-opt/c (f z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (f a)]))
      (s-b (s-b (contract (f 11)
                          (s 12 (s 13 #f))
                          'pos
                          'neg)))))
  
  
  (test/spec-passed/result
   'struct/dc-9
   '(let ()
      (struct s (a b))
      
      (define-opt/c (g z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (>=/c (+ a 1))]))
      
      (s-a (contract (g 10)
                     (s 12 (s 14 #f))
                     'pos
                     'neg)))
   12)
  
  (test/spec-passed/result
   'struct/dc-10
   '(let ()
      (struct s (a b))
      
      (define-opt/c (g z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (>=/c (+ a 1))]))
      
      (s-b (contract (g 10)
                     (s 12 14)
                     'pos
                     'neg)))
   14)
  
  (test/pos-blame
   'struct/dc-11
   '(let ()
      
      (struct s (a b))
      
      (define-opt/c (g z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (>=/c (+ a 1))]))
      
      (s-b (contract (g 11)
                     (s 12 10)
                     'pos
                     'neg))))
  
  (test/spec-passed/result
   'struct/dc-12
   '(let ()
      (struct kons (hd tl) #:transparent)
      (define (unknown-function a) (=/c a))
      (define-opt/c (f a b)
        (or/c not
              (struct/dc kons
                         [hd (unknown-function a)]
                         [tl () #:lazy (or/c #f (f b a))])))
      (kons-hd (kons-tl (contract (f 1 2)
                                  (kons 1 (kons 2 #f))
                                  'pos
                                  'neg))))
   2)
  
  (test/spec-passed
   'struct/dc-13
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a #:lazy integer?])
                (s #f)
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/dc-14
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a #:lazy (-> integer? integer?)])
                (s #f)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/dc-15
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a integer?])
                (s #f)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/dc-16
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a (-> integer? integer?)])
                (s #f)
                'pos
                'neg)))
  
  (test/spec-passed
   'struct/dc-17
   '(let ()
      (struct s (q a))
      (contract (struct/dc s
                           [q integer?]
                           [a (q) #:lazy (<=/c q)])
                (s 1 #f)
                'pos
                'neg)))
  
  (test/pos-blame
   'struct/dc-18
   '(let ()
      (struct s (q a))
      (contract (struct/dc s
                           [q integer?]
                           [a (q) (<=/c q)])
                (s 1 #f)
                'pos
                'neg)))
  
  (contract-error-test
   'struct/dc-19
   '(let ()
      (struct s (a b))
      (struct/dc s [a (new-∃/c 'α)] [b integer?]))
   exn:fail?)
  
  (contract-error-test
   'struct/dc-20
   '(let ()
      (struct s (a b))
      (contract (struct/dc s [a (b) (new-∃/c 'α)] [b integer?])
                (s 1 2)
                'pos 'neg))
   exn:fail?)
  
  (test/pos-blame
   'struct/dc-new1
   '(let ()
      (struct s (a))
      (contract (struct/dc s [a integer?]) (s #f) 'pos 'neg)))
  
  (test/spec-passed
   'struct/dc-new2
   '(let ()
      (struct s (a))
      (contract (struct/dc s [a #:lazy integer?]) (s #f) 'pos 'neg)))
  
  (test/pos-blame
   'struct/dc-new3
   '(let ()
      (struct s (a))
      (s-a (contract (struct/dc s [a #:lazy integer?]) (s #f) 'pos 'neg))))
  
  (test/spec-passed
   'struct/dc-new4
   '(let ()
      (struct s ([a #:mutable]))
      (contract (struct/dc s [a integer?]) (s #f) 'pos 'neg)))
  
  (test/pos-blame
   'struct/dc-new5
   '(let ()
      (struct s ([a #:mutable]))
      (s-a (contract (struct/dc s [a integer?]) (s #f) 'pos 'neg))))
  
  (test/neg-blame
   'struct/dc-new6
   '(let ()
      (struct s ([a #:mutable]))
      (set-s-a! (contract (struct/dc s [a integer?]) (s 1) 'pos 'neg)
                #f)))
  
  (test/spec-passed
   'struct/dc-new7
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a any/c] [b (a) (non-empty-listof real?)] [c (b) (<=/c (car b))])
                     (s 3 '(2) 1)
                     'pos
                     'neg))))
  
  
  (test/spec-passed
   'struct/dc-new8
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s 
                                [a any/c] 
                                [b (a) (non-empty-listof real?)]
                                [c (a b) (and/c (<=/c a) (<=/c (car b)))])
                     (s 3 '(2) 1)
                     'pos
                     'neg))))
  
  (test/spec-passed
   'struct/dc-new9
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s 
                                [a any/c]
                                [b (a) (non-empty-listof real?)]
                                [c (b a) (and/c (<=/c a) (<=/c (car b)))])
                     (s 3 '(2) 1)
                     'pos
                     'neg))))
  
  
  (test/spec-passed
   'struct/dc-new10
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s
                                [a (b) (<=/c (car b))]
                                [b (c) (non-empty-listof real?)]
                                [c real?])
                     (s 1 '(2) 3)
                     'pos
                     'neg))))
  
  (test/spec-passed
   'struct/dc-new11
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s 
                                [a (b c) (and/c (<=/c (car b)) (<=/c c))]
                                [b (c) (non-empty-listof real?)]
                                [c real?])
                     (s 1 '(2) 3)
                     'pos
                     'neg))))
  
  (test/spec-passed
   'struct/dc-new12
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s
                                [a (c b) (and/c (<=/c (car b)) (<=/c c))]
                                [b (c) (non-empty-listof real?)]
                                [c real?])
                     (s 1 '(2) 3)
                     'pos
                     'neg))))
  
  
  (test/pos-blame
   'struct/dc-new13
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f (-> integer? integer?)] [b (f) (<=/c (f 1))])
                (s (λ (x) #f) 123)
                'pos
                'neg)))
  
  (test/spec-failed
   'struct/dc-new14
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f (-> integer? integer?)] [b (f) (<=/c (f #f))])
                (s (λ (x) 1) 123)
                'pos
                'neg))
   "top-level")
  
  (test/pos-blame
   'struct/dc-new15
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f (-> integer? integer?)] [b (f) #:lazy (<=/c (f 1))])
                (s (λ (x) #f) 123)
                'pos
                'neg)))
  
  (test/spec-failed
   'struct/dc-new16
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f (-> integer? integer?)] [b (f) #:lazy (<=/c (f #f))])
                (s (λ (x) 1) 123)
                'pos
                'neg))
   "top-level")
  
  (test/pos-blame
   'struct/dc-new17
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f #:lazy (-> integer? integer?)] [b (f) #:lazy (<=/c (f 1))])
                (s (λ (x) #f) 123)
                'pos
                'neg)))
  
  (test/spec-failed
   'struct/dc-new18
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f #:lazy (-> integer? integer?)] [b (f) #:lazy (<=/c (f #f))])
                (s (λ (x) 1) 123)
                'pos
                'neg))
   "top-level")
  
  (test/spec-passed
   'struct/dc-new19
   '(let ()
      (struct s (a b c d))
      (contract (struct/dc s
                           [a integer?]
                           [b #:lazy symbol?]
                           [c (a) boolean?]
                           [d (a c) integer?])
                (s 1 'x #t 5)
                'pos 'neg)))
  
  (test/spec-passed
   'struct/dc-new20
   '(let ()
      (struct s (a [b #:mutable] c [d #:mutable]))
      (contract (struct/dc s
                           [a integer?]
                           [b symbol?]
                           [c (a) boolean?]
                           [d (a c) integer?])
                (s 1 'x #t 5)
                'pos 'neg)))
  
  (test/spec-passed
   'struct/dc-new21
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a integer?] [b boolean?])
                             (s 1 #f)
                             'pos 'neg))
      (set-s-a! an-s 2)))
  
  (test/neg-blame
   'struct/dc-new22
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a integer?] [b boolean?])
                             (s 1 #f)
                             'pos 'neg))
      (set-s-a! an-s #f)))
  
  (test/spec-passed
   'struct/dc-new22
   '(let ()
      (struct s ([a #:mutable] b))
      (contract (struct/dc s [a integer?] [b boolean?])
                (s 'one #f)
                'pos 'neg)))
  
  (test/pos-blame
   'struct/dc-new23
   '(let ()
      (struct s ([a #:mutable] b))
      (s-a (contract (struct/dc s [a integer?] [b boolean?])
                     (s 'one #f)
                     'pos 'neg))))
  
  (test/pos-blame
   'struct/dc-new24
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b boolean?])
                             (s (λ (x) #f) #f)
                             'pos 'neg))
      ((s-a an-s) 1)))
  
  (test/neg-blame
   'struct/dc-new25
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b boolean?])
                             (s (λ (x) #f) #f)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) #f))
      ((s-a an-s) 1)))
  
  (test/pos-blame
   'struct/dc-new26
   '(let ()
      (struct s ([a #:mutable] b))
      (contract (struct/dc s [a (-> integer? integer?)] [b (a) (<=/c (a 1))])
                (s (λ (x) #f) #f)
                'pos 'neg)))
  
  (test/pos-blame
   'struct/dc-new27
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -2))
      (s-b an-s)))
  
  (test/neg-blame
   'struct/dc-new28
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) #f))
      (s-b an-s)))
  
  (test/pos-blame
   'struct/dc-new29
   '(let ()
      (struct s ([a #:mutable] b c))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))]
                                        [c (b) (<=/c b)])
                             (s (λ (x) 1) -11 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -2))
      (s-c an-s)))
  
  (test/pos-blame
   'struct/dc-new30
   '(let ()
      (struct s ([a #:mutable] b c))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))]
                                        [c (b) (<=/c b)])
                             (s (λ (x) 1) 1 -2)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -2))
      (s-c an-s)))
  
  (test/neg-blame
   'struct/dc-new31
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-b! an-s 3)))
  
  (test/pos-blame
   'struct/dc-new32
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -1))
      (s-b an-s)))
  
  (test/spec-failed
   'struct/dc-new33
   '(let ()
      (struct s (a [b #:mutable] [c #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b any/c]
                                        [c (a b) (<=/c (a b))])
                             (s (λ (x) 1) 1 1)
                             'pos 'neg))
      (set-s-b! an-s #f)
      (s-c an-s))
   "top-level")
  
  (contract-error-test
   'struct/dc-new-34
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (contract (struct/dc s
                           [a boolean?]
                           [b (a)
                              #:flat
                              (if a
                                  (<=/c 1)
                                  (-> integer? integer?))])
                (s #f 1)
                'pos
                'neg))
   (λ (x) (regexp-match #rx"struct/dc: .*flat" (exn-message x))))
  
  (contract-error-test
   'struct/dc-new-35
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a boolean?]
                                        [b (a)
                                           #:flat
                                           (if a
                                               (<=/c 1)
                                               (-> integer? integer?))])
                             (s #t 1)
                             'pos
                             'neg))
      (set-s-a! an-s #f)
      (s-b an-s))
   (λ (x) (regexp-match #rx"struct/dc: .*flat" (exn-message x))))
  
  (contract-error-test
   'struct/dc-new-36
   '(let ()
      (struct s ([a #:mutable] b))
      (contract (struct/dc s
                           [a boolean?]
                           [b (a)
                              (if a
                                  (<=/c 1)
                                  (new-∃/c 'α))])
                (s #f 1)
                'pos
                'neg))
   (λ (x) (regexp-match #rx"struct/dc: .*chaperone" (exn-message x))))
  
  (contract-error-test
   'struct/dc-new-37
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s
                                        [a boolean?]
                                        [b (a)
                                           (if a
                                               (<=/c 1)
                                               (new-∃/c 'α))])
                             (s #t 1)
                             'pos
                             'neg))
      (set-s-a! an-s #f)
      (s-b an-s))
   (λ (x) (regexp-match #rx"struct/dc: .*chaperone" (exn-message x))))
  
  (contract-error-test
   'struct/dc-new-38
   '(let ()
      (struct s ([a #:mutable] b [c #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a boolean?]
                                        [b (a)
                                           (if a
                                               (<=/c 1)
                                               (new-∃/c 'α))]
                                        [c (b) integer?])
                             (s #t 1 1)
                             'pos
                             'neg))
      (set-s-a! an-s #f)
      (s-c an-s))
   (λ (x) (regexp-match #rx"struct/dc: .*chaperone" (exn-message x))))
  
  (test/spec-passed
   'struct/dc-new-39
   '(let ()
      (struct s (a b))
      (contract (struct/dc s [a integer?] [b integer?]) (s 1 2) 'pos 'neg)))
  
  (test/spec-passed
   'struct/dc-new40
   '(let ()
      (struct s (a b))
      (contract (struct/dc s [a (-> integer? integer?)] [b (-> integer? integer?)])
                (s (λ (x) x) (λ (y) y))
                'pos
                'neg)))
  
  (test/spec-passed/result
   'struct/dc-new41
   '(let ()
      (struct s (a [b #:mutable]))
      (define α (new-∀/c 'α))
      (s-b ((contract (-> α (struct/dc s [b α]))
                      (λ (x) (s 11 x))
                      'pos
                      'neg) 1)))
   1)
  
  (test/spec-passed/result
   'struct/dc-new42
   '(let ()
      (struct s (a [b #:mutable]))
      (define α (new-∀/c 'α))
      (s-b ((contract (-> α (struct/dc s [a integer?] [b (a) #:impersonator α]))
                      (λ (x) (s 11 x))
                      'pos
                      'neg) 1)))
   1)
  
  (test/spec-passed
   'struct/dc-new42
   '(let ()
      (struct s (a [b #:mutable]))
      (contract (struct/dc s [a (-> integer? integer?)] [b (new-∀/c 'α)])
                (s (λ (x) x) 1)
                'pos
                'neg)))
  
  (test/spec-passed/result
   'struct/dc-new43
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (b-y (contract (struct/dc d 
                                [(x #:parent a) boolean?]
                                [(y #:parent b) char?]
                                [(z #:parent c) number?]
                                [w string?])
                     (d #t #\a 3 "x")
                     'pos
                     'neg)))
   #\a)
  
  (test/spec-passed/result
   'struct/dc-new44
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (b-y (contract (struct/dc d 
                                [(x #:parent a) (w) boolean?]
                                [(y #:parent b) ((x #:parent a)) char?]
                                [(z #:parent c) number?]
                                [w string?])
                     (d #t #\a 3 "x")
                     'pos
                     'neg)))
   #\a)
  
  (test/neg-blame
   'struct/dc-new45
   '(begin
      (struct s ([f #:mutable] g) #:transparent)
      (define an-s
        (contract (struct/dc s [f (g) (<=/c g)] [g real?])
                  (s 1 2)
                  'pos
                  'neg))
      (set-s-f! an-s 3)))
  
  (test/spec-passed/result
   'struct/dc-pred1
   '(let ()
      (struct s (a b))
      (define p? (flat-contract-predicate (struct/dc s [a number?] [b (a) #:flat (<=/c a)])))
      (list (p? (s 2 1))
            (p? (s 1 2))))
   '(#t #f))
  
  (test/spec-passed/result
   'struct/dc-pred2
   '(let ()
      (struct s (a b c))
      (define p? (flat-contract-predicate (struct/dc s 
                                                     [a number?]
                                                     [b boolean?]
                                                     [c (a b)
                                                        #:flat
                                                        (if (and (= a 1) b)
                                                            any/c
                                                            none/c)])))
      
      (list (p? (s 1 #t 'whatever))
            (p? (s 11 #f 'whatver))))
   '(#t #f))
  
  (test/spec-passed
   'struct/dc-inv1
   '(let ()
      (struct s (f g))
      (contract (struct/dc s
                           [f real?]
                           [g real?]
                           #:inv (f g) (<= f g))
                (s 1 2)
                'pos
                'neg)))

  (test/pos-blame
   'struct/dc-inv2
   '(let ()
      (struct s (f g))
      (contract (struct/dc s
                           [f real?]
                           [g real?]
                           #:inv (f g) (<= f g))
                (s 2 1)
                'pos
                'neg)))
  
  (test/neg-blame
   'struct/dc-inv3
   '(let ()
      (struct s (f [g #:mutable]))
      (define an-s
        (contract (struct/dc s
                             [f real?]
                             [g real?]
                             #:inv (f g) (<= f g))
                  (s 1 2)
                  'pos
                  'neg))
      (set-s-g! an-s -1)))
  
  (test/spec-passed
   'struct/dc-inv4
   '(let ()
      (struct s (f [g #:mutable]))
      (define an-s
        (contract (struct/dc s
                             [f real?]
                             [g real?]
                             #:inv (f g) (<= f g))
                  (s 1 2)
                  'pos
                  'neg))
      (set-s-g! an-s 3)))
  
  (test/spec-passed
   'struct/dc-inv5
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (contract (struct/dc d 
                           [(x #:parent a) any/c]
                           [(y #:parent b) any/c]
                           [(z #:parent c) any/c]
                           [w any/c]
                           #:inv ((x #:parent a) (y #:parent b) (z #:parent c) w)
                           (and (equal? x #t)
                                (equal? y #\a)
                                (equal? z 3)
                                (equal? w "x")))
                (d #t #\a 3 "x")
                'pos
                'neg)))
  
  (test/spec-passed/result
   'struct/dc-inv6
   '(let ()
      (struct s (f))
      ((struct/dc s [f any/c] #:inv (f) (equal? f 11)) (s 12)))
   #f)
  
  (test/spec-passed/result
   'struct/dc-inv7
   '(let ()
      (struct s (f))
      ((struct/dc s [f any/c] #:inv (f) (equal? f 11)) (s 11)))
   #t)
  
  (test/spec-passed/result
   'struct/dc-inv8
   '(let ()
      (struct node (v l r))
      ((struct/dc node
                  [v any/c]
                  [l any/c]
                  [r any/c]
                  #:inv (l r) #f)
       (node #f #f #f)))
   #f)
  
  (test/spec-passed/result
   'struct/dc-inv9
   '(let ()
      (struct node (v l r))
      ((struct/dc node
                  [v any/c]
                  [l any/c]
                  [r any/c]
                  #:inv (l r) #t)
       (node #f #f #f)))
   #t)
  
  (contract-error-test
   'struct/dc-imp-nondep-runtime-error
   '(let ()
      (struct s (ernie bert))
      (struct/dc s [ernie integer?] [bert (new-∀/c 'α)]))
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"chaperone-contract[?]" (exn-message x)))))
  
  (contract-error-test
   'struct/dc-not-a-field
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a integer?] [y integer?])))
   exn:fail:syntax?)
  
  (contract-error-test
   'struct/dc-circular-dependecies1
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a (a) integer?] [b (a) integer?])))
   exn:fail:syntax?)
  
  (contract-error-test
   'struct/dc-circular-dependecies2
   #'(eval '(let ()
              (struct s (a b c))
              (struct/dc s [a (b) integer?] [b (a) integer?] [c integer?])))
   exn:fail:syntax?)
  
  (contract-error-test
   'struct/dc-dep-on-lazy
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a #:lazy integer?] [b (a) integer?])))
   exn:fail:syntax?)
  
  (contract-error-test
   'struct/dc-lazy-mutable
   #'(eval '(let ()
              (struct s (a [b #:mutable]))
              (struct/dc s [a integer?] [b #:lazy integer?])))
   exn:fail:syntax?)
  
  (contract-error-test
   'struct/dc-immutable-impersonator
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a integer?] [b (a) #:impersonator (<=/c a)])))
   (λ (x) (and (exn:fail:syntax? x) (regexp-match #rx"immutable" (exn-message x)))))
  
  (contract-error-test
   'struct/dc-inv-not-a-field
   #'(eval '(let ()
              (struct s (f g))
              (struct/dc s
                         [f real?]
                         [g real?]
                         #:inv (f g h) (<= f g))))
   (λ (x) (and (exn:fail:syntax? x)
               (regexp-match #rx"field: h is depended" (exn-message x)))))
  
  (contract-error-test
   'struct/dc-inv-dep-on-lazy
   #'(eval '(let ()
              (struct s (f g))
              (struct/dc s
                         [f real?]
                         [g (f) #:lazy real?]
                         #:inv (f g) (<= f g))))
   (λ (x) (and (exn:fail:syntax? x)
               (regexp-match #rx"field: g.*lazy" (exn-message x)))))
  
  (contract-error-test
   'struct/dc-dep-on-present
   #'(eval '(begin
              (struct s (f [g #:mutable]) #:transparent)
              (contract (struct/dc s [f (g) (<=/c g)])
                        (s 1 2)
                        'pos
                        'neg)))
    (λ (x) (and (exn:fail:syntax? x)
                (regexp-match #rx"the field: g is depended on.*no contract"
                              (exn-message x))))))
