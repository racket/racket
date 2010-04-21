#lang scheme/base

(require "../utils/utils.ss")
(require (rename-in (types subtype convenience remove-intersect union utils)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (rep type-rep filter-rep rep-utils) scheme/list
         scheme/contract scheme/match unstable/match
         (for-syntax scheme/base)
         "tc-metafunctions.ss")

;(provide (all-defined-out))

(define-syntax-rule (d/c/p (name . args) c . body)
  (begin (d/c (name . args) c . body)
         (p/c [name c])))

(d/c/p (open-Result r objs)
       (-> Result? (listof Object?) (values Type/c FilterSet? Object?))
       (match r
         [(Result: t fs old-obj)
          (for/fold ([t t] [fs fs] [old-obj old-obj])
            ([(o k) (in-indexed (in-list objs))])
            (values (subst-type t k o #t)
                    (subst-filter-set fs k o #t)
                    (subst-object old-obj k o #t)))]))

(d/c/p (subst-filter-set fs k o polarity)
       (-> FilterSet? integer? Object? boolean? FilterSet?)
  (match fs
    [(FilterSet: f+ f-)
     (combine (subst-filter f+ k o polarity) 
	      (subst-filter f- k o polarity))]))

(d/c (subst-type t k o polarity)
     (-> Type/c integer? Object? boolean? Type/c)
  (define (st t) (subst-type t k o polarity))
  (d/c (sf fs) (FilterSet? . -> . FilterSet?) (subst-filter-set fs k o polarity))
  (type-case (#:Type st 
	      #:Filter sf
	      #:Object (lambda (f) (subst-object f k o polarity)))
	      t
              [#:arr dom rng rest drest kws
                     ;; here we have to increment the count for the domain, where the new bindings are in scope
                     (let* ([arg-count (+ (length dom) (if rest 1 0) (if drest 1 0) (length kws))]
                            [st* (lambda (t) (subst-type t (+ arg-count k) o polarity))])
                       (make-arr (map st dom)
                                 (st* rng)
                                 (and rest (st rest))
                                 (and drest (cons (st (car drest)) (cdr drest)))
                                 (map st kws)))]))

(d/c (subst-object t k o polarity)
     (-> Object? integer? Object? boolean? Object?)
  (match t
    [(NoObject:) t]
    [(Empty:) t]
    [(Path: p i)
     (if (eq? i k)
	 (match o
	   [(Empty:) (make-Empty)]
	   ;; the result is not from an annotation, so it isn't a NoObject
	   [(NoObject:) (make-Empty)]
	   [(Path: p* i*) (make-Path (append p p*) i*)])
	 t)]))

;; this is the substitution metafunction 
(d/c/p (subst-filter f k o polarity)
  (-> Filter/c integer? Object? boolean? Filter/c)
  (define (ap f) (subst-filter f o polarity))
  (define (tf-matcher t p i k o polarity maker)
    (match o
      [(or (Empty:) (NoObject:)) (if polarity -top -bot)]
      [(Path: p* i*)
       (cond [(eq? i k)
	      (maker
	       (subst-type t k o polarity)
	       (append p p*) 
	       i*)]
	     [(index-free-in? k t) (if polarity -top -bot)]
	     [else f])]))
  (match f
    [(ImpFilter: ant consq)
     (make-ImpFilter (subst-filter ant k o (not polarity)) (ap consq))]
    [(AndFilter: fs) (make-AndFilter (map ap fs))]
    [(OrFilter: fs) (make-OrFilter (map ap fs))]
    [(Bot:) -bot]
    [(Top:) -top]
    [(TypeFilter: t p i)
     (tf-matcher t p i k o polarity make-TypeFilter)]
    [(NotTypeFilter: t p i)
     (tf-matcher t p i k o polarity make-NotTypeFilter)]))

(define (index-free-in? k type)
  (let/ec 
   return
   (define (for-object o)
     (object-case (#:Type for-type)
		  o
		  [#:Path p i
			  (if (eq? i k)
			      (return #t)
			      o)]))
   (define (for-filter o)
     (filter-case (#:Type for-type
		   #:Filter for-filter)
		  o
		  [#:NotTypeFilter t p i
				   (if (eq? i k)
				       (return #t)
				       o)]
		  [#:TypeFilter t p i
				(if (eq? i k)
				    (return #t)
				    o)]))
   (define (for-type t)
     (type-case (#:Type for-type
		 #:Filter for-filter
		 #:Object for-object)
		t
                [#:arr dom rng rest drest kws
                       ;; here we have to increment the count for the domain, where the new bindings are in scope
                       (let* ([arg-count (+ (length dom) (if rest 1 0) (if drest 1 0) (length kws))]
                              [st* (lambda (t) (for-type (+ arg-count k) t))])
                         (make-arr (map for-type dom)
                                   (st* rng)
                                   (and rest (for-type rest))
                                   (and drest (cons (for-type (car drest)) (cdr drest)))
                                   (map for-type kws)))]))
   (for-type type)))

;; (or/c Values? ValuesDots?) listof[identifier] -> tc-results?
(d/c/p (values->tc-results tc formals)
  ((or/c Values? ValuesDots?) (listof identifier?) . -> . tc-results?)
  (match tc
    [(ValuesDots: (list rs ...) dty dbound)
     (let-values ([(ts fs os) (for/lists (ts fs os) ([r (in-list rs)]) (open-Result r (map (lambda (i) (make-Path null i)) formals)))])
       (ret ts fs os
            (for/fold ([dty dty]) ([(o k) (in-indexed (in-list formals))])
              (subst-type dty k (make-Path null o) #t))
            dbound))]
    [(Values: (list rs ...))
     (let-values ([(ts fs os) (for/lists (ts fs os) ([r (in-list rs)]) (open-Result r (map (lambda (i) (make-Path null i)) formals)))])
       (ret ts fs os))]))
