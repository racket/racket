#lang typed/racket/base

(require racket/flonum
         racket/string
         (for-syntax racket/base)
         "../unsafe.rkt")

(provide (rename-out [inline-build-flvector  build-flvector]
                     [inline-flvector-map  flvector-map])
         unsafe-flvector-map
         ;; Pointwise operations
         flvector-scale
         flvector-round
         flvector-floor
         flvector-ceiling
         flvector-truncate
         flvector-abs
         flvector-sqr
         flvector-sqrt
         flvector-log
         flvector-exp
         flvector-sin
         flvector-cos
         flvector-tan
         flvector-asin
         flvector-acos
         flvector-atan
         flvector+
         flvector*
         flvector-
         flvector/
         flvector-expt
         flvector-min
         flvector-max
         flvector=
         flvector<
         flvector<=
         flvector>
         flvector>=)

;; ===================================================================================================
;; build-flvector

(define-syntax (inline-build-flvector stx)
  (syntax-case stx ()
    [(_ size f)
     (syntax/loc stx
       (let: ([n : Integer  size])
         (define xs (make-flvector n))
         (with-asserts ([n index?])
           (let: loop : FlVector ([i : Nonnegative-Fixnum  0])
             (cond [(i . < . n)  (unsafe-flvector-set! xs i ((ann f (Index -> Float)) i))
                                 (loop (+ i 1))]
                   [else  xs])))))]
    [(_ e ...)  (syntax/loc stx (build-flvector e ...))]
    [_  (syntax/loc stx build-flvector)]))

(: build-flvector (Integer (Index -> Float) -> FlVector))
(define (build-flvector size f)
  (cond [(index? size)  (inline-build-flvector size f)]
        [else  (raise-type-error 'build-flvector "Index" 0 size f)]))

;; ===================================================================================================
;; flvector-map

(define-syntax (unsafe-flvector-map stx)
  (syntax-case stx ()
    [(_ f xs-expr)
     (syntax/loc stx
       (let: ([xs : FlVector  xs-expr])
         (define n (flvector-length xs))
         (inline-build-flvector
          n (λ: ([i : Index]) ((ann f (Float -> Float))
                               (unsafe-flvector-ref xs i))))))]
    [(_ f xs-expr xss-expr ...)
     (with-syntax ([(xs xss ...)  (generate-temporaries #'(xs-expr xss-expr ...))]
                   [(Floats ...)  (build-list (length (syntax->list #'(xss-expr ...)))
                                              (λ _ #'Float))])
       (syntax/loc stx
         (let: ([xs : FlVector  xs-expr] [xss : FlVector  xss-expr] ...)
           (define n (flvector-length xs))
           (inline-build-flvector
            n (λ: ([i : Index])
                ((ann f (Float Floats ... -> Float))
                 (unsafe-flvector-ref xs i) (unsafe-flvector-ref xss i) ...))))))]
    [(_ e ...)  (syntax/loc stx (flvector-map e ...))]
    [_  (syntax/loc stx flvector-map)]))

(define-syntax (inline-flvector-map stx)
  (syntax-case stx ()
    [(_ f xs-expr)  (syntax/loc stx (unsafe-flvector-map f xs-expr))]
    [(_ f xs-expr xss-expr ...)
     (with-syntax ([(xs xss ...)  (generate-temporaries #'(xs-expr xss-expr ...))]
                   [(n ns ...)    (generate-temporaries #'(xs-expr xss-expr ...))])
       (syntax/loc stx
         (let: ([xs : FlVector  xs-expr] [xss : FlVector  xss-expr] ...)
           (define n (flvector-length xs))
           (define ns (flvector-length xss)) ...
           (unless (= n ns ...)
             (error 'inline-flvector-map "flvectors must be the same length; given lengths ~a"
                    (string-join (list (number->string n) (number->string ns) ...) ", ")))
           (unsafe-flvector-map f xs-expr xss-expr ...))))]
    [(_ e ...)  (syntax/loc stx (flvector-map e ...))]
    [_  (syntax/loc stx flvector-map)]))

(: flvector-map (case-> ((Float -> Float) FlVector -> FlVector)
                        ((Float Float * -> Float) FlVector FlVector * -> FlVector)))
(define flvector-map
  (case-lambda:
    [([f : (Float -> Float)] [xs : FlVector])
     (inline-flvector-map f xs)]
    [([f : (Float Float * -> Float)] [xs : FlVector] . [yss : FlVector *])
     (define n (flvector-length xs))
     (define ns (map flvector-length yss))
     (unless (or (null? ns) (apply = n ns))
       (error 'flvector-map "flvectors must be the same length; given lengths ~a"
              (string-join (list* (number->string n) (map number->string ns)) ", ")))
     (inline-build-flvector
      n (λ: ([i : Index])
          (apply f (unsafe-flvector-ref xs i)
                 (map (λ: ([zs : FlVector]) (unsafe-flvector-ref zs i)) yss))))])) 

;; ===================================================================================================
;; Pointwise operations

(define-syntax (inline-flvector-lift1 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr) (inline-flvector-map f arr)))]))

(define-syntax (inline-flvector-lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2) (inline-flvector-map f arr1 arr2)))]))

(define-syntax-rule (inline-flvector-lift-compare name comp)
  (λ (xs1 xs2)
    (define n1 (flvector-length xs1))
    (define n2 (flvector-length xs2))
    (unless (= n1 n2) (error name "flvectors must be the same length; given lengths ~e and ~e" n1 n2))
    (build-vector
     n1 (λ: ([j : Index])
          (comp (unsafe-flvector-ref xs1 j)
                (unsafe-flvector-ref xs2 j))))))

(: flvector-scale (FlVector Float -> FlVector))
(define (flvector-scale arr y) (inline-flvector-map (λ (x) (* x y)) arr))

(: flvector-round    (FlVector -> FlVector))
(: flvector-floor    (FlVector -> FlVector))
(: flvector-ceiling  (FlVector -> FlVector))
(: flvector-truncate (FlVector -> FlVector))
(: flvector-abs  (FlVector -> FlVector))
(: flvector-sqr  (FlVector -> FlVector))
(: flvector-sqrt (FlVector -> FlVector))
(: flvector-log  (FlVector -> FlVector))
(: flvector-exp  (FlVector -> FlVector))
(: flvector-sin  (FlVector -> FlVector))
(: flvector-cos  (FlVector -> FlVector))
(: flvector-tan  (FlVector -> FlVector))
(: flvector-asin (FlVector -> FlVector))
(: flvector-acos (FlVector -> FlVector))
(: flvector-atan (FlVector -> FlVector))

(: flvector+ (FlVector FlVector -> FlVector))
(: flvector* (FlVector FlVector -> FlVector))
(: flvector- (case-> (FlVector -> FlVector)
                    (FlVector FlVector -> FlVector)))
(: flvector/ (case-> (FlVector -> FlVector)
                    (FlVector FlVector -> FlVector)))
(: flvector-expt (FlVector FlVector -> FlVector))
(: flvector-min  (FlVector FlVector -> FlVector))
(: flvector-max  (FlVector FlVector -> FlVector))

(: flvector=  (FlVector FlVector -> (Vectorof Boolean)))
(: flvector<  (FlVector FlVector -> (Vectorof Boolean)))
(: flvector<= (FlVector FlVector -> (Vectorof Boolean)))
(: flvector>  (FlVector FlVector -> (Vectorof Boolean)))
(: flvector>= (FlVector FlVector -> (Vectorof Boolean)))

(define flvector-round    (inline-flvector-lift1 flround))
(define flvector-floor    (inline-flvector-lift1 flfloor))
(define flvector-ceiling  (inline-flvector-lift1 flceiling))
(define flvector-truncate (inline-flvector-lift1 fltruncate))
(define flvector-abs  (inline-flvector-lift1 flabs))
(define flvector-sqr  (inline-flvector-lift1 (λ: ([x : Float]) (* x x))))
(define flvector-sqrt (inline-flvector-lift1 flsqrt))
(define flvector-log  (inline-flvector-lift1 fllog))
(define flvector-exp  (inline-flvector-lift1 flexp))
(define flvector-sin  (inline-flvector-lift1 flsin))
(define flvector-cos  (inline-flvector-lift1 flcos))
(define flvector-tan  (inline-flvector-lift1 fltan))
(define flvector-asin (inline-flvector-lift1 flasin))
(define flvector-acos (inline-flvector-lift1 flacos))
(define flvector-atan (inline-flvector-lift1 flatan))

(define flvector+ (inline-flvector-lift2 fl+))
(define flvector* (inline-flvector-lift2 fl*))

(define flvector-
  (case-lambda
    [(arr)  (inline-flvector-map (λ: ([x : Float]) (fl- 0.0 x)) arr)]
    [(arr1 arr2)  (inline-flvector-map fl- arr1 arr2)]))

(define flvector/
  (case-lambda
    [(arr)  (inline-flvector-map (λ: ([x : Float]) (/ 1.0 x)) arr)]
    [(arr1 arr2)  (inline-flvector-map fl/ arr1 arr2)]))

(define flvector-expt (inline-flvector-lift2 flexpt))
(define flvector-min  (inline-flvector-lift2 flmin))
(define flvector-max  (inline-flvector-lift2 flmax))

(define flvector=  (inline-flvector-lift-compare 'flvector=  fl=))
(define flvector<  (inline-flvector-lift-compare 'flvector<  fl<))
(define flvector<= (inline-flvector-lift-compare 'flvector<= fl<=))
(define flvector>  (inline-flvector-lift-compare 'flvector>  fl>))
(define flvector>= (inline-flvector-lift-compare 'flvector>= fl>=))
