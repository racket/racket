#lang typed/racket/base

(require racket/sequence
         racket/list
         "../../flonum.rkt"
         "../../base.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(: lte?->lt? (All (A) ((A A -> Any) -> (A A -> Boolean))))
(define ((lte?->lt? lte?) x1 x2)
  (and (lte? x1 x2) (not (lte? x2 x1))))

(: find-near-pow2 (Real -> Nonnegative-Exact-Rational))
(define (find-near-pow2 x)
  (expt 2 (max -1073 (min 1023 (exact-round (/ (log (abs x)) (fllog 2.0)))))))

(: weights->list (Symbol (Sequenceof Real) -> (Listof Nonnegative-Real)))
(define (weights->list name w-seq)
  (for/list: : (Listof Nonnegative-Real) ([w w-seq])
    (cond [(w . >= . 0)  w]
          [else  (raise-argument-error name "(Sequenceof Nonnegative-Real)" w-seq)])))

(: weights->normalized-weights (Symbol (Sequenceof Real) -> (Listof Nonnegative-Flonum)))
(define (weights->normalized-weights name ws)
  (let ([ws  (weights->list name ws)])
    (when (empty? ws) (raise-argument-error name "nonempty (Sequenceof Real)" ws))
    (define max-w (find-near-pow2 (apply max ws)))
    (let ([ws  (map (λ: ([w : Real]) (/ w max-w)) ws)])
      (define total-w (sum ws))
      (map (λ: ([w : Real]) (assert (fl (/ w total-w)) nonnegative?)) ws))))

;; ===================================================================================================

(: check-lengths! (All (A B) (Symbol String A B Index Index -> Void)))
(define (check-lengths! name what xs ys m n)
  (unless (= m n) (error name "~a must be the same length; given ~e (length ~a) and ~e (length ~a)"
                         what xs m ys n)))

(: sequences->weighted-samples
   (All (A) (Symbol (Sequenceof A) (Sequenceof Real)
                    -> (Values (Listof A) (Listof Nonnegative-Real)))))
(define (sequences->weighted-samples name x-seq w-seq)
  (define xs (sequence->list x-seq))
  (define ws
    (for/list: : (Listof Nonnegative-Real) ([w w-seq])
      (cond [(w . >= . 0)  w]
            [else  (raise-argument-error name "(Sequenceof Nonnegative-Real)" 1 x-seq w-seq)])))
  (check-lengths! name "values and weights" xs ws (length xs) (length ws))
  (values xs ws))

(define nonnegative? (λ: ([x : Real]) (not (negative? x))))

(: sequences->normalized-weighted-samples
   (All (A) (Symbol (Sequenceof A) (Sequenceof Real)
                    -> (Values (Listof A) (Listof Positive-Flonum)))))
(define (sequences->normalized-weighted-samples name xs ws)
  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)])
    (when (empty? xs) (raise-argument-error name "nonempty (Sequenceof A)" 0 xs ws))
    (define max-w (find-near-pow2 (assert (apply max ws) nonnegative?)))
    (let ([ws  (map (λ: ([w : Nonnegative-Real]) (/ w max-w)) ws)])
      (define total-w (sum ws))
      (let loop ([xs  xs]
                 [ws  ws]
                 [#{new-xs : (Listof A)}  empty]
                 [#{new-ws : (Listof Positive-Flonum)}  empty])
        (cond [(or (empty? xs) (empty? ws))  (values (reverse new-xs) (reverse new-ws))]
              [else
               (define w (fl (/ (first ws) total-w)))
               (cond [(w . > . 0.0)
                      (loop (rest xs) (rest ws) (cons (first xs) new-xs) (cons w new-ws))]
                     [else
                      (loop (rest xs) (rest ws) new-xs new-ws)])])))))

(: sequence->normalized-weighted-samples
   (All (A) (Symbol (Sequenceof A) -> (Values (Listof A) (Listof Positive-Flonum)))))
(define (sequence->normalized-weighted-samples name xs)
  (let ([xs  (sequence->list xs)])
    (when (empty? xs) (raise-argument-error name "nonempty (Sequenceof A)" xs))
    (define n (length xs))
    (define w (assert (fl/ 1.0 (fl n)) positive?))
    (values xs (build-list n (λ (i) w)))))

(: sequence->vector (All (A) ((Sequenceof A) -> (Vectorof A))))
(define (sequence->vector vs)
  (for/vector: ([v vs]) : A v))

(: sequences->weighted-sample-vectors
   (All (A) (Symbol (Sequenceof A) (Sequenceof Real)
                    -> (Values (Vectorof A) (Vectorof Nonnegative-Real)))))
(define (sequences->weighted-sample-vectors name x-seq w-seq)
  (define xs (sequence->vector x-seq))
  (define ws
    (for/vector: ([w w-seq]) : Nonnegative-Real
      (cond [(w . >= . 0)  w]
            [else  (raise-argument-error name "(Sequenceof Nonnegative-Real)" 1 x-seq w-seq)])))
  (check-lengths! name "values and weights" xs ws (vector-length xs) (vector-length ws))
  (values xs ws))

;; ===================================================================================================

;; bias = #f   Return the central moment
;; bias = #t   Assume sum of weights is the count and correct for bias normally
;; bias = n    Assume n actual samples; correct for bias

(: get-bias-adjustment (Nonnegative-Real (U #t Real) Positive-Real -> Positive-Real))
(define (get-bias-adjustment c bias mn)
  (define n (if (real? bias) bias c))
  (if (n . > . mn) n +nan.0))

(: adjust-variance (Nonnegative-Real Nonnegative-Real (U #t #f Real) -> Nonnegative-Real))
(define (adjust-variance m2 n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 1)])
           (define c (max 0 (/ n (- n 1))))  ; max proves c >= 0
           (* m2 c))]
        [else  m2]))

(: adjust-covariance (Real Nonnegative-Real (U #t #f Real) -> Real))
(define (adjust-covariance m2 n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 1)])
           (* m2 (/ n (- n 1))))]
        [else  m2]))

(: adjust-skewness (Real Nonnegative-Real (U #t #f Real) -> Real))
(define (adjust-skewness g n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 2)])
           (fl (* g (/ (sqrt (max 0 (* n (- n 1)))) (- n 2)))))]
        [else  (fl g)]))

(: adjust-kurtosis (Nonnegative-Real Nonnegative-Real (U #t #f Real) -> Nonnegative-Real))
(define (adjust-kurtosis g n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 3)])
           (define c (max 0 (/ (- n 1) (* (- n 2) (- n 3)))))  ; max proves c >= 0
           (* (+ (* (+ n 1) g) 6) c))]
        [else  g]))
