#lang typed/racket/base

(require racket/flonum
         racket/string
         (only-in racket/math conjugate)
         (for-syntax racket/base syntax/parse)
         "flvector.rkt"
         "../../flonum.rkt"
         "../unsafe.rkt"
         "../exception.rkt")

(provide
 (rename-out [-FCVector FCVector]) fcvector?
 fcvector-length fcvector-real-part fcvector-imag-part
 ;; Construction
 fcvector
 make-fcvector
 (rename-out [inline-unsafe-fcvector-ref   unsafe-fcvector-ref]
             [inline-unsafe-fcvector-set!  unsafe-fcvector-set!]
             [inline-fcvector-ref   fcvector-ref]
             [inline-fcvector-set!  fcvector-set!])
 (rename-out [inline-build-fcvector  build-fcvector])
 unsafe-fcvector-copy!
 fcvector-copy!
 fcvector-copy
 (rename-out [inline-fcvector-map  fcvector-map])
 unsafe-fcvector-map
 ;; Loops
 for/fcvector:
 for*/fcvector:
 (rename-out [in-fcvector-clause in-fcvector])
 ;; Conversion
 list->fcvector
 fcvector->list
 vector->fcvector
 fcvector->vector
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
         (raise-argument-error name (format "FlVector of length ~e" n) xs)]
        [(not (= n (flvector-length ys)))
         (raise-argument-error name (format "FlVector of length ~e" n) ys)]
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

(: fcvector (Float-Complex * -> FCVector))
(define (fcvector . zs) (list->fcvector zs))

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
    [(_ zs-expr i-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr] [i : Integer  i-expr])
         (make-rectangular (unsafe-flvector-ref (fcvector-real-part zs) i)
                           (unsafe-flvector-ref (fcvector-imag-part zs) i))))]
    [(_ e ...)  (syntax/loc stx (unsafe-fcvector-ref e ...))]
    [_  (syntax/loc stx unsafe-fcvector-ref)]))

(: unsafe-fcvector-ref (FCVector Integer -> Float-Complex))
(define (unsafe-fcvector-ref zs i) (inline-unsafe-fcvector-ref zs i))

(define-syntax (inline-fcvector-ref stx)
  (syntax-case stx ()
    [(_ zs-expr i-expr)
     (syntax/loc stx
       (let*: ([zs : FCVector  zs-expr]
               [i : Integer  i-expr]
               [n : Index  (fcvector-length zs)])
         (if (and (0 . <= . i) (i . < . n))
             (inline-unsafe-fcvector-ref zs i)
             (error 'fcvector-ref "expected Index < ~e; given ~e" n i))))]
    [(_ e ...)  (syntax/loc stx (fcvector-ref e ...))]
    [_  (syntax/loc stx fcvector-ref)]))

(: fcvector-ref (FCVector Integer -> Float-Complex))
(define (fcvector-ref zs i) (inline-fcvector-ref zs i))

(define-syntax (inline-unsafe-fcvector-set! stx)
  (syntax-case stx ()
    [(_ zs-expr i-expr v-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr] [i : Integer  i-expr] [v : Float-Complex  v-expr])
         (unsafe-flvector-set! (fcvector-real-part zs) i (real-part v))
         (unsafe-flvector-set! (fcvector-imag-part zs) i (imag-part v))))]
    [(_ e ...)  (syntax/loc stx (unsafe-fcvector-set! e ...))]
    [_  (syntax/loc stx unsafe-fcvector-set!)]))

(: unsafe-fcvector-set! (FCVector Integer Float-Complex -> Void))
(define (unsafe-fcvector-set! zs i v) (inline-unsafe-fcvector-set! zs i v))

(define-syntax (inline-fcvector-set! stx)
  (syntax-case stx ()
    [(_ zs-expr i-expr v)
     (syntax/loc stx
       (let*: ([zs : FCVector  zs-expr]
               [i : Integer  i-expr]
               [n : Index  (fcvector-length zs)])
         (if (and (0 . <= . i) (i . < . n))
             (inline-unsafe-fcvector-set! zs i v)
             (error 'fcvector-set! "expected Index < ~e; given ~e" n i))))]
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
       (let*: ([n : Integer  size]
               [xs : FlVector  (make-flvector n)]
               [ys : FlVector  (make-flvector n)])
         (with-asserts ([n index?])
           (let: loop : FCVector ([i : Nonnegative-Fixnum  0])
             (cond [(i . < . n)
                    (let ([z  (f i)])
                      (unsafe-flvector-set! xs i (real-part z))
                      (unsafe-flvector-set! ys i (imag-part z)))
                    (loop (+ i 1))]
                   [else  (FCVector n xs ys)])))))]
    [(_ e ...)  (syntax/loc stx (build-fcvector e ...))]
    [_  (syntax/loc stx build-fcvector)]))

(: build-fcvector (Integer (Index -> Float-Complex) -> FCVector))
(define (build-fcvector size f)
  (cond [(index? size)  (inline-build-fcvector size f)]
        [else  (raise-argument-error 'build-fcvector "Index" 0 size f)]))

;; ===================================================================================================
;; fcvector-copy

(: unsafe-fcvector-copy! (FCVector Integer FCVector Integer Integer -> Void))
(define (unsafe-fcvector-copy! dest dest-start src src-start src-end)
  (define dest-xs (fcvector-real-part dest))
  (define dest-ys (fcvector-imag-part dest))
  (define src-xs (fcvector-real-part src))
  (define src-ys (fcvector-imag-part src))
  (let loop ([i dest-start] [j src-start])
    (when (j . unsafe-fx< . src-end)
      (unsafe-flvector-set! dest-xs i (unsafe-flvector-ref src-xs j))
      (unsafe-flvector-set! dest-ys i (unsafe-flvector-ref src-ys j))
      (loop (unsafe-fx+ i 1) (unsafe-fx+ j 1)))))

(: fcvector-copy! (case-> (FCVector Integer FCVector -> Void)
                          (FCVector Integer FCVector Integer -> Void)
                          (FCVector Integer FCVector Integer Integer -> Void)))
(define fcvector-copy!
  (case-lambda
    [(dest dest-start src)
     (fcvector-copy! dest dest-start src 0 (fcvector-length src))]
    [(dest dest-start src src-start)
     (fcvector-copy! dest dest-start src src-start (fcvector-length src))]
    [(dest dest-start src src-start src-end)
     (define dest-len (fcvector-length dest))
     (define src-len (fcvector-length src))
     (cond [(or (dest-start . < . 0) (dest-start . > . dest-len))
            (raise-argument-error 'fcvector-copy! (format "Index <= ~e" dest-len) 1
                                  dest dest-start src src-start src-end)]
           [(or (src-start . < . 0) (src-start . > . src-len))
            (raise-argument-error 'fcvector-copy! (format "Index <= ~e" src-len) 3
                                  dest dest-start src src-start src-end)]
           [(or (src-end . < . 0) (src-end . > . src-len))
            (raise-argument-error 'fcvector-copy! (format "Index <= ~e" src-len) 4
                                  dest dest-start src src-start src-end)]
           [(src-end . < . src-start)
            (error 'fcvector-copy! "ending index is smaller than starting index")]
           [((- dest-len dest-start) . < . (- src-end src-start))
            (error 'fcvector-copy! "not enough room in target vector")]
           [else
            (unsafe-fcvector-copy! dest dest-start src src-start src-end)])]))

(: fcvector-copy (case-> (FCVector -> FCVector)
                         (FCVector Integer -> FCVector)
                         (FCVector Integer Integer -> FCVector)))
(define (fcvector-copy zs [start 0] [end (fcvector-length zs)])
  (define n (fcvector-length zs))
  (define new-zs (FCVector n (make-flvector n) (make-flvector n)))
  (fcvector-copy! new-zs 0 zs start end)
  new-zs)

;; ===================================================================================================
;; fcvector-map

(define-syntax (unsafe-fcvector-map stx)
  (syntax-case stx ()
    [(_ f zs-expr)
     (syntax/loc stx
       (let: ([zs : FCVector  zs-expr])
         (let ([n  (fcvector-length zs)]
               [xs  (fcvector-real-part zs)]
               [ys  (fcvector-imag-part zs)])
           (define-syntax-rule (fun i)
             (f (make-rectangular (unsafe-flvector-ref xs i)
                                  (unsafe-flvector-ref ys i))))
           (inline-build-fcvector n fun))))]
    [(_ f zs-expr zss-expr ...)
     (with-syntax ([(xs xss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(ys yss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(zs zss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(Ts ...)  (build-list (length (syntax->list #'(zss-expr ...)))
                                          (λ _ #'Float-Complex))])
       (syntax/loc stx
         (let: ([zs : FCVector  zs-expr] [zss : FCVector  zss-expr] ...)
           (let ([n  (fcvector-length zs)]
                 [xs  (fcvector-real-part zs)]
                 [ys  (fcvector-imag-part zs)]
                 [xss  (fcvector-real-part zss)] ...
                 [yss  (fcvector-imag-part zss)] ...)
             (define-syntax-rule (fun i)
               (f (make-rectangular (unsafe-flvector-ref xs i)
                                    (unsafe-flvector-ref ys i))
                  (make-rectangular (unsafe-flvector-ref xss i)
                                    (unsafe-flvector-ref yss i)) ...))
             (inline-build-fcvector n fun)))))]))

(define-syntax (inline-fcvector-map stx)
  (syntax-case stx ()
    [(_ f zs-expr)  (syntax/loc stx (unsafe-fcvector-map f zs-expr))]
    [(_ f zs-expr zss-expr ...)
     (with-syntax ([(zs zss ...)  (generate-temporaries #'(zs-expr zss-expr ...))]
                   [(n ns ...)    (generate-temporaries #'(zs-expr zss-expr ...))])
       (syntax/loc stx
         (let: ([zs : FCVector  zs-expr] [zss : FCVector  zss-expr] ...)
           (let ([n  (fcvector-length zs)]
                 [ns  (fcvector-length zss)] ...)
             (unless (= n ns ...)
               (error 'fcvector-map "fcvectors must be the same length; given lengths ~a"
                      (string-join (list (number->string n) (number->string ns) ...) ", ")))
             (unsafe-fcvector-map f zs-expr zss-expr ...)))))]
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
;; Loops

(define-syntax (base-for/fcvector: stx)
  (syntax-parse stx
    [(_ for: #:length n-expr:expr (clauses ...) body ...+)
     (quasisyntax/loc stx
       (let: ([n : Integer  n-expr])
         (cond [(n . > . 0)
                (let ([xs  (make-flvector n)]
                      [ys  (make-flvector n)]
                      [i  0])
                  (let/ec: break : Void
                    (for: (clauses ...)
                      (let: ([z : Float-Complex  (let () body ...)])
                        (unsafe-flvector-set! xs i (real-part z))
                        (unsafe-flvector-set! ys i (imag-part z)))
                      (set! i (unsafe-fx+ i 1))
                      (when (i . unsafe-fx>= . n) (break (void))))
                    (void))
                  (with-asserts ([n index?])
                    (FCVector n xs ys)))]
               [else
                (FCVector 0 (flvector) (flvector))])))]
    [(_ for: (clauses ...) body ...+)
     (quasisyntax/loc stx
       (let ([n   4]
             [xs  (make-flvector 4)]
             [ys  (make-flvector 4)]
             [i   0])
         (for: (clauses ...)
           (let: ([z : Float-Complex  (let () body ...)])
             (let ([x  (real-part z)]
                   [y  (imag-part z)])
               (cond [(unsafe-fx= i n)
                      (let ([new-n  (unsafe-fx* 2 n)])
                        (let ([new-xs  (make-flvector new-n x)]
                              [new-ys  (make-flvector new-n y)])
                          (unsafe-flvector-copy! new-xs 0 xs 0 n)
                          (unsafe-flvector-copy! new-ys 0 ys 0 n)
                          (set! n new-n)
                          (set! xs new-xs)
                          (set! ys new-ys)))]
                     [else
                      (unsafe-flvector-set! xs i x)
                      (unsafe-flvector-set! ys i y)])))
           (set! i (unsafe-fx+ i 1)))
         (void)
         (let ([new-xs  (if (unsafe-fx= i n) xs (flvector-copy xs 0 i))]
               [new-ys  (if (unsafe-fx= i n) ys (flvector-copy ys 0 i))]
               [i i])
           (with-asserts ([i index?])
             (FCVector i new-xs new-ys)))))]))

(define-syntax-rule (for/fcvector: e ...)
  (base-for/fcvector: for: e ...))

(define-syntax-rule (for*/fcvector: e ...)
  (base-for/fcvector: for*: e ...))

;; ===================================================================================================
;; Sequences

(: in-fcvector : (FCVector -> (Sequenceof Float-Complex)))
(define (in-fcvector zs)
  (define n (fcvector-length zs))
  (define xs (fcvector-real-part zs))
  (define ys (fcvector-imag-part zs))
  (make-do-sequence
   (λ () (values (λ: ([i : Fixnum]) (make-rectangular (unsafe-flvector-ref xs i)
                                                      (unsafe-flvector-ref ys i)))
                 (λ: ([i : Fixnum]) (unsafe-fx+ i 1))
                 0
                 (λ: ([i : Fixnum]) (i . < . n))
                 #f
                 #f))))

(define-sequence-syntax in-fcvector-clause
  (λ () #'in-fcvector)
  (λ (stx)
    (syntax-case stx ()
      [[(z) (_ zs-expr)]
       (syntax/loc stx
         [(z)
          (:do-in
           ([(n xs ys)
             (let: ([zs : FCVector  zs-expr])
               (values (fcvector-length zs)
                       (fcvector-real-part zs)
                       (fcvector-imag-part zs)))])
           (void)
           ([#{i : Nonnegative-Fixnum} 0])
           (< i n)
           ([(z)  (make-rectangular (unsafe-flvector-ref xs i)
                                    (unsafe-flvector-ref ys i))])
           #true
           #true
           [(+ i 1)])])]
      [[_ clause] (raise-syntax-error 'in-array "expected (in-fcvector <FCVector>)"
                                      #'clause #'clause)])))

;; ===================================================================================================
;; Conversion

(: list->fcvector ((Listof Number) -> FCVector))
(define (list->fcvector zs)
  (define n (length zs))
  (define xs (make-flvector n))
  (define ys (make-flvector n))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [zs zs])
    (cond [(i . < . n)
           (define z (unsafe-car zs))
           (unsafe-flvector-set! xs i (real->double-flonum (real-part z)))
           (unsafe-flvector-set! ys i (real->double-flonum (imag-part z)))
           (loop (+ i 1) (unsafe-cdr zs))]
          [else
           (FCVector n xs ys)])))

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

(: vector->fcvector ((Vectorof Number) -> FCVector))
(define (vector->fcvector vs)
  (define n (vector-length vs))
  (define xs (make-flvector n))
  (define ys (make-flvector n))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . n)
           (define z (unsafe-vector-ref vs i))
           (unsafe-flvector-set! xs i (real->double-flonum (real-part z)))
           (unsafe-flvector-set! ys i (real->double-flonum (imag-part z)))
           (loop (+ i 1))]
          [else
           (FCVector n xs ys)])))

(: fcvector->vector (FCVector -> (Vectorof Float-Complex)))
(define (fcvector->vector zs)
  (define n (fcvector-length zs))
  (define xs (fcvector-real-part zs))
  (define ys (fcvector-imag-part zs))
  (define vs (make-vector n 0.0+0.0i))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . n)
           (unsafe-vector-set! vs i (make-rectangular (unsafe-flvector-ref xs i)
                                                      (unsafe-flvector-ref ys i)))
           (loop (+ i 1))]
          [else  vs])))

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
