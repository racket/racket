#lang typed/racket/base

(require racket/flonum
         racket/string
         (only-in racket/math conjugate)
         (for-syntax racket/base)
         "flvector.rkt"
         "../unsafe.rkt")

(provide (rename-out [-FCVector FCVector]) fcvector?
         fcvector-length fcvector-real-part fcvector-imag-part
         fcvector list->fcvector fcvector->list
         make-fcvector
         (rename-out [inline-unsafe-fcvector-ref   unsafe-fcvector-ref]
                     [inline-unsafe-fcvector-set!  unsafe-fcvector-set!]
                     [inline-fcvector-ref   fcvector-ref]
                     [inline-fcvector-set!  fcvector-set!])
         (rename-out [inline-build-fcvector  build-fcvector]
                     [inline-fcvector-map  fcvector-map])
         unsafe-fcvector-map
         fcvector-copy-all
         ;; Pointwise operations
         fcvector-scale
         fcvector-sqr
         fcvector-sqrt
         fcvector-conjugate
         fcvector-magnitude
         fcvector-angle
         fcvector-log
         fcvector-exp
         fcvector-sin
         fcvector-cos
         fcvector-tan
         fcvector-asin
         fcvector-acos
         fcvector-atan
         fcvector+
         fcvector*
         fcvector-
         fcvector/
         fcvector-expt
         fcvector=
         fcvector-real-part
         fcvector-imag-part
         fcvector-make-rectangular)

;; ===================================================================================================
;; Types and other basics

(: fcvector-guard (Index FlVector FlVector Symbol -> (Values Index FlVector FlVector)))
(define (fcvector-guard n xs ys name)
  (cond [(not (= n (flvector-length xs)))
         (raise-type-error name (format "FlVector of length ~e" n) xs)]
        [(not (= n (flvector-length ys)))
         (raise-type-error name (format "FlVector of length ~e" n) ys)]
        [else  (values n xs ys)]))

(: fcvector-print (FCVector Output-Port (U #t #f 0 1) -> Void))
(define (fcvector-print zs port mode)
  (print `(fcvector ,@(fcvector->list zs)) port 1))

(struct: FCVector ([length : Index] [real-part : FlVector] [imag-part : FlVector])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:guard fcvector-guard
  #:property prop:custom-write fcvector-print)

(define-type -FCVector FCVector)
(define fcvector? FCVector?)
(define fcvector-length FCVector-length)
(define fcvector-real-part FCVector-real-part)
(define fcvector-imag-part FCVector-imag-part)

(: list->fcvector ((Listof Float-Complex) -> FCVector))
(define (list->fcvector zs)
  (define n (length zs))
  (define xs (make-flvector n))
  (define ys (make-flvector n))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [zs zs])
    (cond [(i . < . n)
           (define z (unsafe-car zs))
           (unsafe-flvector-set! xs i (real-part z))
           (unsafe-flvector-set! ys i (imag-part z))
           (loop (+ i 1) (unsafe-cdr zs))]
          [else
           (FCVector n xs ys)])))

(: fcvector (Float-Complex * -> FCVector))
(define (fcvector . zs) (list->fcvector zs))

(: fcvector->list (FCVector -> (Listof Float-Complex)))
(define (fcvector->list zs)
  (define n (fcvector-length zs))
  (define xs (fcvector-real-part zs))
  (define ys (fcvector-imag-part zs))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{acc : (Listof Float-Complex)} null])
    (cond [(i . < . n)
           (loop (+ i 1) (cons (make-rectangular (unsafe-flvector-ref xs i)
                                                 (unsafe-flvector-ref ys i))
                               acc))]
          [else  (reverse acc)])))

(: make-fcvector (case-> (Integer -> FCVector)
                         (Integer Float-Complex -> FCVector)))
(define (make-fcvector n [z 0.0+0.0i])
  (define xs (make-flvector n (real-part z)))
  (define ys (make-flvector n (imag-part z)))
  (FCVector (flvector-length xs) xs ys))

;; ===================================================================================================
;; Indexing

(define-syntax (inline-unsafe-fcvector-ref stx)
  (syntax-case stx ()
    ;; This is split into identifier and non-identifier cases because intervening "let" bindings
    ;; confuse TR's optimizer's Float-Complex unboxer into not unboxing everything
    [(_ zs i)
     (and (identifier? #'zs) (identifier? #'i))
     (syntax/loc stx
       (make-rectangular (unsafe-flvector-ref (fcvector-real-part zs) i)
                         (unsafe-flvector-ref (fcvector-imag-part zs) i)))]
    [(_ zs-expr i-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr] [i : Integer  i-expr])
         (inline-unsafe-fcvector-ref zs i)))]
    [(_ e ...)  (syntax/loc stx (unsafe-fcvector-ref e ...))]
    [_  (syntax/loc stx unsafe-fcvector-ref)]))

(: unsafe-fcvector-ref (FCVector Integer -> Float-Complex))
(define (unsafe-fcvector-ref zs i) (inline-unsafe-fcvector-ref zs i))

(define-syntax (inline-fcvector-ref stx)
  (syntax-case stx ()
    [(_ zs-expr i-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr] [i : Integer  i-expr])
         (define n (fcvector-length zs))
         (cond [(and (0 . <= . i) (i . < . n))  (inline-unsafe-fcvector-ref zs i)]
               [else  (error 'fcvector-ref "expected Index < ~e; given ~e" n i)])))]
    [(_ e ...)  (syntax/loc stx (fcvector-ref e ...))]
    [_  (syntax/loc stx fcvector-ref)]))

(: fcvector-ref (FCVector Integer -> Float-Complex))
(define (fcvector-ref zs i) (inline-fcvector-ref zs i))

(define-syntax (inline-unsafe-fcvector-set! stx)
  (syntax-case stx ()
    [(_ zs i v)
     (and (identifier? #'zs) (identifier? #'i) (identifier? #'v))
     (syntax/loc stx
       (begin (unsafe-flvector-set! (fcvector-real-part zs) i (real-part v))
              (unsafe-flvector-set! (fcvector-imag-part zs) i (imag-part v))))]
    [(_ zs-expr i-expr v-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr] [i : Integer  i-expr] [v : Float-Complex  v-expr])
         (inline-unsafe-fcvector-set! zs i v)))]
    [(_ e ...)  (syntax/loc stx (unsafe-fcvector-set! e ...))]
    [_  (syntax/loc stx unsafe-fcvector-set!)]))

(: unsafe-fcvector-set! (FCVector Integer Float-Complex -> Void))
(define (unsafe-fcvector-set! zs i v) (inline-unsafe-fcvector-set! zs i v))

(define-syntax (inline-fcvector-set! stx)
  (syntax-case stx ()
    [(_ zs-expr i-expr v)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr] [i : Integer  i-expr])
         (define n (fcvector-length zs))
         (cond [(and (0 . <= . i) (i . < . n))  (inline-unsafe-fcvector-set! zs i v)]
               [else  (error 'fcvector-set! "expected Index < ~e; given ~e" n i)])))]
    [(_ e ...)  (syntax/loc stx (fcvector-set! e ...))]
    [_  (syntax/loc stx fcvector-set!)]))

(: fcvector-set! (FCVector Integer Float-Complex -> Void))
(define (fcvector-set! zs i v) (inline-fcvector-set! zs i v))

;; ===================================================================================================
;; build-fcvector

(define-syntax (inline-build-fcvector stx)
  (syntax-case stx ()
    [(_ size f)
     (syntax/loc stx
       (let: ([n : Integer  size])
         (define xs (make-flvector n))
         (define ys (make-flvector n))
         (with-asserts ([n index?])
           (let: loop : FCVector ([i : Nonnegative-Fixnum  0])
             (cond [(i . < . n)
                    (define z (f i))
                    (unsafe-flvector-set! xs i (real-part z))
                    (unsafe-flvector-set! ys i (imag-part z))
                    (loop (+ i 1))]
                   [else  (FCVector n xs ys)])))))]
    [(_ e ...)  (syntax/loc stx (build-fcvector e ...))]
    [_  (syntax/loc stx build-fcvector)]))

(: build-fcvector (Integer (Index -> Float-Complex) -> FCVector))
(define (build-fcvector size f)
  (cond [(index? size)  (inline-build-fcvector size f)]
        [else  (raise-type-error 'build-fcvector "Index" 0 size f)]))

(: fcvector-copy-all (FCVector -> FCVector))
(define (fcvector-copy-all zs)
  (define n (fcvector-length zs))
  (define xs (fcvector-real-part zs))
  (define ys (fcvector-imag-part zs))
  (define new-xs (make-flvector n))
  (define new-ys (make-flvector n))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . n)
           (unsafe-flvector-set! new-xs i (unsafe-flvector-ref xs i))
           (unsafe-flvector-set! new-ys i (unsafe-flvector-ref ys i))
           (loop (+ i 1))]
          [else  (FCVector n new-xs new-ys)])))

;; ===================================================================================================
;; fcvector-map

(define-syntax (unsafe-fcvector-map stx)
  (syntax-case stx ()
    [(_ f zs-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr])
         (define n (fcvector-length zs))
         (define xs (fcvector-real-part zs))
         (define ys (fcvector-imag-part zs))
         (define-syntax-rule (fun i)
           (f (make-rectangular (unsafe-flvector-ref xs i)
                                (unsafe-flvector-ref ys i))))
         (inline-build-fcvector n fun)))]
    [(_ f zs-expr zss-expr ...)
     (with-syntax ([(xs xss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(ys yss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(zs zss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(Ts ...)  (build-list (length (syntax->list #'(zss-expr ...)))
                                          (λ _ #'Float-Complex))])
       (syntax/loc stx
         (let: ([zs : FCVector  zs-expr] [zss : FCVector  zss-expr] ...)
           (define n (fcvector-length zs))
           (define xs (fcvector-real-part zs))
           (define xss (fcvector-real-part zss)) ...
           (define ys (fcvector-imag-part zs))
           (define yss (fcvector-imag-part zss)) ...
           (define-syntax-rule (fun i)
             (f (make-rectangular (unsafe-flvector-ref xs i)
                                  (unsafe-flvector-ref ys i))
                (make-rectangular (unsafe-flvector-ref xss i)
                                  (unsafe-flvector-ref yss i)) ...))
           (inline-build-fcvector n fun))))]))

(define-syntax (inline-fcvector-map stx)
  (syntax-case stx ()
    [(_ f zs-expr)  (syntax/loc stx (unsafe-fcvector-map f zs-expr))]
    [(_ f zs-expr zss-expr ...)
     (with-syntax ([(zs zss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(n ns ...)    (generate-temporaries #'(zs-expr zss-expr ...))])
       (syntax/loc stx
         (let: ([zs : FCVector  zs-expr] [zss : FCVector  zss-expr] ...)
           (define n (fcvector-length zs))
           (define ns (fcvector-length zss)) ...
           (unless (= n ns ...)
             (error 'fcvector-map "fcvectors must be the same length; given lengths ~a"
                    (string-join (list (number->string n) (number->string ns) ...) ", ")))
           (unsafe-fcvector-map f zs-expr zss-expr ...))))]
    [(_ e ...)  (syntax/loc stx (fcvector-map e ...))]
    [_  (syntax/loc stx fcvector-map)]))

(: fcvector-map
   (case-> ((Float-Complex -> Float-Complex) FCVector -> FCVector)
           ((Float-Complex Float-Complex * -> Float-Complex) FCVector FCVector * -> FCVector)))
(define fcvector-map
  (case-lambda:
    [([f : (Float-Complex -> Float-Complex)] [xs : FCVector])
     (inline-fcvector-map f xs)]
    [([f : (Float-Complex Float-Complex * -> Float-Complex)] [xs : FCVector] . [yss : FCVector *])
     (define n (fcvector-length xs))
     (define ns (map fcvector-length yss))
     (unless (or (null? ns) (apply = n ns))
       (error 'fcvector-map "fcvectors must be the same length; given lengths ~a"
              (string-join (list* (number->string n) (map number->string ns)) ", ")))
     (inline-build-fcvector
      n (λ: ([i : Index])
          (apply f (unsafe-fcvector-ref xs i)
                 (map (λ: ([ys : FCVector]) (unsafe-fcvector-ref ys i)) yss))))]))

;; ===================================================================================================
;; Pointwise operations

(define-syntax (inline-fcvector-lift1 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr) (inline-fcvector-map f arr)))]))

(define-syntax (inline-fcvector-lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2) (inline-fcvector-map f arr1 arr2)))]))

(: fcvector-scale (FCVector (U Float Float-Complex) -> FCVector))
(define (fcvector-scale arr y)
  (define-syntax-rule (scale x) (* x y))
  (cond [(flonum? y)  (inline-fcvector-map scale arr)]
        [else  (inline-fcvector-map scale arr)]))

(: fcvector-sqr (FCVector -> FCVector))
(: fcvector-sqrt (FCVector -> FCVector))
(: fcvector-conjugate (FCVector -> FCVector))
(: fcvector-magnitude (FCVector -> FlVector))
(: fcvector-angle (FCVector -> FlVector))
(: fcvector-log (FCVector -> FCVector))
(: fcvector-exp (FCVector -> FCVector))
(: fcvector-sin (FCVector -> FCVector))
(: fcvector-cos (FCVector -> FCVector))
(: fcvector-tan (FCVector -> FCVector))
(: fcvector-asin (FCVector -> FCVector))
(: fcvector-acos (FCVector -> FCVector))
(: fcvector-atan (FCVector -> FCVector))

(: fcvector+ (FCVector FCVector -> FCVector))
(: fcvector* (FCVector FCVector -> FCVector))
(: fcvector- (case-> (FCVector -> FCVector)
                    (FCVector FCVector -> FCVector)))
(: fcvector/ (case-> (FCVector -> FCVector)
                    (FCVector FCVector -> FCVector)))
(: fcvector-expt (FCVector FCVector -> FCVector))
(: fcvector= (FCVector FCVector -> (Vectorof Boolean)))

(: fcvector-make-rectangular (FlVector FlVector -> FCVector))

(define-syntax-rule (sqr-syntax z) (* z z))

(define fcvector-sqr (inline-fcvector-lift1 sqr-syntax))
(define fcvector-sqrt (inline-fcvector-lift1 sqrt))
(define fcvector-conjugate (inline-fcvector-lift1 conjugate))

(define (fcvector-magnitude zs)
  (unsafe-flvector-map (λ: ([x : Float] [y : Float])
                         (magnitude (make-rectangular x y)))
                       (fcvector-real-part zs)
                       (fcvector-imag-part zs)))

(define (fcvector-angle zs)
  (unsafe-flvector-map (λ: ([x : Float] [y : Float])
                         (angle (make-rectangular x y)))
                       (fcvector-real-part zs)
                       (fcvector-imag-part zs)))

(define fcvector-log (inline-fcvector-lift1 log))
(define fcvector-exp (inline-fcvector-lift1 exp))
(define fcvector-sin (inline-fcvector-lift1 sin))
(define fcvector-cos (inline-fcvector-lift1 cos))
(define fcvector-tan (inline-fcvector-lift1 tan))
(define fcvector-asin (inline-fcvector-lift1 asin))
(define fcvector-acos (inline-fcvector-lift1 acos))
(define fcvector-atan (inline-fcvector-lift1 atan))

(define fcvector+ (inline-fcvector-lift2 +))
(define fcvector* (inline-fcvector-lift2 *))

(define fcvector-
  (case-lambda
    [(arr)
     (define-syntax-rule (negate z) (- 0.0+0.0i z))
     (inline-fcvector-map negate arr)]
    [(arr1 arr2)
     (inline-fcvector-map - arr1 arr2)]))

(define fcvector/
  (case-lambda
    [(arr)
     (define-syntax-rule (invert z) (/ 1.0+0.0i z))
     (inline-fcvector-map invert arr)]
    [(arr1 arr2)
     (inline-fcvector-map / arr1 arr2)]))

(define fcvector-expt (inline-fcvector-lift2 expt))

(define (fcvector= zs1 zs2)
  (define n1 (fcvector-length zs1))
  (define n2 (fcvector-length zs2))
  (unless (= n1 n2) (error 'fcvector= "fcvectors must be the same length; given lengths ~e and ~e"
                           n1 n2))
  (define xs1 (fcvector-real-part zs1))
  (define ys1 (fcvector-imag-part zs1))
  (define xs2 (fcvector-real-part zs2))
  (define ys2 (fcvector-imag-part zs2))
  (build-vector
   n1 (λ: ([j : Index])
        (and (= (unsafe-flvector-ref xs1 j)
                (unsafe-flvector-ref xs2 j))
             (= (unsafe-flvector-ref ys1 j)
                (unsafe-flvector-ref ys2 j))))))

(define (fcvector-make-rectangular xs ys)
  (FCVector (flvector-length xs) xs ys))
