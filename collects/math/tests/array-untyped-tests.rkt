#lang racket

(require (for-syntax racket/match)
         rackunit
         math/array)

;; ===================================================================================================
;; Contract tests

(begin-for-syntax
  (define exceptions '(array
                       mutable-array
                       flarray
                       fcarray
                       inline-array-map
                       array+
                       array*
                       array-
                       array/
                       array-min
                       array-max
                       array-scale
                       array-abs
                       array-sqr
                       array-sqrt
                       array-conjugate
                       array-real-part
                       array-imag-part
                       array-make-rectangular
                       array-magnitude
                       array-angle
                       array-make-polar
                       array<
                       array<=
                       array>
                       array>=
                       array=
                       array-not
                       array-and
                       array-or
                       array-if
                       array-axis-sum
                       array-axis-prod
                       array-axis-min
                       array-axis-max
                       array-all-sum
                       array-all-prod
                       array-all-min
                       array-all-max
                       array-count
                       array-andmap
                       array-ormap
                       inline-flarray-map
                       inline-fcarray-map
                       array-strict
                       array-default-strict
                       make-unsafe-array-proc
                       make-unsafe-array-set-proc
                       array/syntax))
  
  (define (looks-like-value? sym)
    (define str (symbol->string sym))
    (and (not (char-upper-case? (string-ref str 0)))
         (not (regexp-match #rx"for/" str))
         (not (regexp-match #rx"for\\*/" str))
         (not (member sym exceptions))))
  
  (define array-exports
    (let ()
      (match-define (list (list #f _ ...)
                          (list 1 _ ...)
                          (list 0 array-exports ...))
        (syntax-local-module-exports #'math/array))
      (filter looks-like-value? array-exports)))
  )

(define-syntax (all-exports stx)
  (with-syntax ([(array-exports ...)  array-exports])
    (syntax/loc stx
      (begin (void array-exports) ...))))

(all-exports)

;; ---------------------------------------------------------------------------------------------------
;; Comprehensions

(check-equal? (for/array #:shape #() () 3)
              (mutable-array 3))
(check-equal? (for/array #:shape #() () 'foo) 
              (mutable-array 'foo))
(check-equal? (for/array #:shape #(2) ([x (in-naturals)]) x) 
              (mutable-array #[0 1]))
(check-equal? (for/array #:shape #(2 3) ([i (in-range 0 6)])
                (vector (quotient i 3) (remainder i 3)))
              (indexes-array #(2 3)))

(check-equal? (for*/array #:shape #() () 3)
              (mutable-array 3))
(check-equal? (for*/array #:shape #() () 'foo)
              (mutable-array 'foo))
(check-equal? (for*/array #:shape #(2) ([x (in-naturals)]) x)
              (mutable-array #[0 1]))
(check-equal? (for*/array #:shape #(2 3) ([i (in-range 0 2)]
                                          [j (in-range 0 3)])
                (vector i j))
              (indexes-array #(2 3)))

(check-equal? (for*/array #:shape #() () 3)
              (for*/array #:shape #() () 3))
(check-equal? (for*/array #:shape #() () 'foo)
              (for*/array #:shape #() () 'foo))
(check-equal? (for*/array #:shape #(2) ([x (in-naturals)]) x)
              (for*/array #:shape #(2) ([x (in-naturals)]) x))
(check-equal? (for*/array #:shape #(2 3) ([i (in-range 0 2)]
                                          [j (in-range 0 3)])
                (list i j))
              (for*/array #:shape #(2 3) ([i (in-range 0 2)]
                                          [j (in-range 0 3)])
                (list i j)))

;; ---------------------------------------------------------------------------------------------------
;; Sequences

(check-equal? (for/list ([x  (in-array (array #[#[1 2 3] #[4 5 6]]))]) x)
              '(1 2 3 4 5 6))

(check-equal? (for/list ([js  (in-array (indexes-array #()))]) js)
              '(#()))
(check-equal? (for/list ([js  (in-array (indexes-array #(0)))]) js)
              '())
(check-equal? (for/list ([js  (in-array (indexes-array #(2 2)))]) js)
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(check-equal? (sequence->list (in-array (indexes-array #())))
              '(#()))
(check-equal? (sequence->list (in-array (indexes-array #(0))))
              '())
(check-equal? (sequence->list (in-array (indexes-array #(2 2))))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(check-equal? (for/list ([js  (in-array-indexes #())]) js)
              '(#()))
(check-equal? (for/list ([js  (in-array-indexes #(0))]) js)
              '())
(check-equal? (for/list ([js  (in-array-indexes #(2 2))]) js)
              '(#(0 0) #(0 1) #(1 0) #(1 1)))
(check-equal? (sequence->list (in-array-indexes #()))
              '(#()))
(check-equal? (sequence->list (in-array-indexes #(0)))
              '())
(check-equal? (sequence->list (in-array-indexes #(2 2)))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(check-equal? (for/list ([js  (in-unsafe-array-indexes #(2 2))]) js)
              '(#(0 0) #(0 0) #(0 0) #(0 0)))
(check-equal? (for/list ([js  (in-unsafe-array-indexes #())]) (vector-copy js))
              '(#()))
(check-equal? (for/list ([js  (in-unsafe-array-indexes #(0))]) (vector-copy js))
              '())
(check-equal? (for/list ([js  (in-unsafe-array-indexes #(2 2))]) (vector-copy js))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))
(check-equal? (sequence->list (in-unsafe-array-indexes #()))
              '(#()))
(check-equal? (sequence->list (in-unsafe-array-indexes #(0)))
              '())
(check-equal? (sequence->list (in-unsafe-array-indexes #(2 2)))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(let ([arr  (indexes-array #(4 5))])
  (check-equal? (for/list ([brr  (in-array-axis arr 0)])
                  (for/list ([js  (in-array brr)])
                    js))
                (array->list* arr))
  (check-equal? (for/list ([brr  (in-array-axis arr 1)])
                  (for/list ([js  (in-array brr)])
                    js))
                (array->list* (array-axis-swap arr 0 1))))

;; ---------------------------------------------------------------------------------------------------
;; Construction macros

(check-equal? (array #[1 2 3])
              (list->array '(1 2 3)))

(check-equal? (array #[#[0 1] #[2 3]])
              (index-array #(2 2)))

(check-equal? (mutable-array #[1 2 3])
              (list->array '(1 2 3)))

(check-equal? (mutable-array #[#[0 1] #[2 3]])
              (index-array #(2 2)))

(check-equal? (flarray #[1 2 3])
              (array->flarray (list->array '(1 2 3))))

(check-equal? (flarray #[#[0 1] #[2 3]])
              (array->flarray (index-array #(2 2))))

(check-equal? (fcarray #[1 2 3])
              (array->fcarray (list->array '(1 2 3))))

(check-equal? (fcarray #[#[0 1] #[2 3]])
              (array->fcarray (index-array #(2 2))))

;; ---------------------------------------------------------------------------------------------------
;; Mapping

(check-equal? (array-map +) (array 0))

(check-equal? (array-map + (array #[1 2 3]))
              (array #[1 2 3]))

(check-equal? (array-map + (array #[1 2 3]) (array #[10 20 30]))
              (array #[11 22 33]))

(check-equal? (array-map make-rectangular (array #[1 2 3]) (array #[10 20 30]))
              (array #[1+10i 2+20i 3+30i]))

(check-exn exn:fail:contract? (λ () (array-map)))
(check-exn exn:fail:contract? (λ () (array-map 5)))
(check-exn exn:fail:contract? (λ () (array-map + 5)))
(check-exn exn:fail:contract? (λ () (array-map -)))
(check-exn exn:fail:contract? (λ () (array-map make-rectangular (array #[1 2 3]))))
(check-exn exn:fail:contract? (λ () (array-map values (array #[1 2 3]) (array #[10 20 30]))))

(check-equal? (array+ (array #[1 2 3]) (array #[10 20 30]))
              (array-map + (array #[1 2 3]) (array #[10 20 30])))

(check-exn exn:fail:contract? (λ () (array+ (array #[1 2 3]) (array 'x))))

(check-equal? (array-scale (array #[1 2 3]) 2)
              (array-map (λ (x) (* x 2)) (array #[1 2 3])))

(check-exn exn:fail:contract? (λ () (array-scale (array #[1 2 3]) 'x)))

(check-equal? (inline-array-map +) (array 0))

(check-equal? (inline-array-map + (array #[1 2 3]))
              (array #[1 2 3]))

(check-equal? (inline-array-map + (array #[1 2 3]) (array #[10 20 30]))
              (array #[11 22 33]))

(check-equal? (inline-array-map make-rectangular (array #[1 2 3]) (array #[10 20 30]))
              (array #[1+10i 2+20i 3+30i]))

(check-exn exn:fail:contract? (λ () (inline-array-map 5)))
(check-exn exn:fail:contract? (λ () (inline-array-map + 5)))
(check-exn exn:fail:contract? (λ () (inline-array-map -)))
(check-exn exn:fail:contract? (λ () (inline-array-map make-rectangular (array #[1 2 3]))))
(check-exn exn:fail:contract? (λ () (inline-array-map values (array #[1 2 3]) (array #[10 20 30]))))

;; ---------------------------------------------------------------------------------------------------
;; Folding

(check-equal? (array-axis-sum (array #[1 2 3]) 0)
              (array 6))


(check-equal? (array-axis-sum (array #[]) 0 0.0)
              (array 0.0))

(check-equal? (array-all-sum (array #[1 2 3]))
              6)

(check-equal? (array-all-sum (array #[]) 0.0)
              0.0)

(check-equal? (array-count even? (array #[1 2 3]))
              1)

(check-equal? (array-count equal? (array #[1 2 3]) (array #[2 1 3]))
              1)

(check-false (array-andmap equal? (array #[1 2 3]) (array #[2 1 3])))
(check-true (array-ormap equal? (array #[1 2 3]) (array #[2 1 3])))

(let ([arr  (parameterize ([array-strictness #f])
              (array-strict
               (build-array #(3 3) (λ (js) (apply + (vector->list js))))))])
  (check-true (array-strict? arr))
  (check-equal? arr (array #[#[0 1 2] #[1 2 3] #[2 3 4]])))

(let ([arr  (parameterize ([array-strictness #f])
              (array-default-strict
               (build-array #(3 3) (λ (js) (apply + (vector->list js))))))])
  (check-false (array-strict? arr))
  (check-equal? arr (array #[#[0 1 2] #[1 2 3] #[2 3 4]]))
  (array-strict! arr)
  (check-true (array-strict? arr))
  (check-equal? arr (array #[#[0 1 2] #[1 2 3] #[2 3 4]])))
