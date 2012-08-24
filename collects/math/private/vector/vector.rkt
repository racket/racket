#lang typed/racket/base

(require racket/vector
         (for-syntax racket/base syntax/parse)
         racket/unsafe/ops
         "../exception.rkt")

(provide for/vector: for*/vector:)

(define-syntax (base-for/vector stx)
  (syntax-case stx ()
    [(name for ann T K #:length n-expr #:fill fill-expr (clauses ...) body-expr)
     (syntax/loc stx
       (call/ec
        (ann (λ (break)
               (define n n-expr)
               (define vs (ann (make-vector n fill-expr) T))
               (define i 0)
               (for (clauses ...)
                 (unsafe-vector-set! vs i body-expr)
                 (set! i (unsafe-fx+ i 1))
                 (when (i . unsafe-fx>= . n) (break vs)))
               vs)
             K)))]
    [(name for ann T K #:length n-expr (clauses ...) body-expr)
     (syntax/loc stx
       (let ([n n-expr])
         (define vs
           (call/ec
            (ann (λ (break)
                   (define vs (ann (vector) T))
                   (define i 0)
                   (for (clauses ...)
                     (define v body-expr)
                     (cond [(unsafe-fx= i 0)  (define new-vs (ann (make-vector n v) T))
                                              (set! vs new-vs)]
                           [else  (unsafe-vector-set! vs i v)])
                     (set! i (unsafe-fx+ i 1))
                     (when (i . unsafe-fx>= . n) (break vs)))
                   vs)
                 K)))
         (cond [(= (vector-length vs) n)  vs]
               [else
                ;; Only happens when n > 0 and vs = (vector)
                (raise-result-error 'name (format "~e-element vector" n) vs)])))]
    [(_ for ann T K (clauses ...) body-expr)
     (syntax/loc stx
       (let ()
         (define n 0)
         (define vs (ann (vector) T))
         (define i 0)
         (for (clauses ...)
           (define v body-expr)
           (cond [(unsafe-fx= i n)  (define new-n (max 4 (unsafe-fx* 2 n)))
                                    (define new-vs (ann (make-vector new-n v) T))
                                    (vector-copy! new-vs 0 vs)
                                    (set! n new-n)
                                    (set! vs new-vs)]
                 [else  (unsafe-vector-set! vs i v)])
           (set! i (unsafe-fx+ i 1)))
         (vector-copy vs 0 i)))]))

(define-for-syntax (base-for/vector: stx for:)
  (syntax-parse stx #:literals (:)
    [(name (~optional (~seq : T:expr))
           (~optional (~seq #:length n-expr:expr))
           (~optional (~seq #:fill fill-expr:expr))
           (clauses ...)
           (~optional (~seq : A:expr))
           body ...+)
     (let ([T  (attribute T)]
           [A  (attribute A)])
       (with-syntax ([(maybe-length ...)  (if (attribute n-expr) #'(#:length n-expr) #'())]
                     [(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())]
                     [body-expr  (if A #`(ann (let () body ...) #,A) #'(let () body ...))]
                     [T  (cond [(and T A)  #`(U #,T (Vectorof #,A))]
                               [T  T]
                               [A  #`(Vectorof #,A)]
                               [else  #'(Vectorof Any)])])
         (quasisyntax/loc stx
           (base-for/vector #,for: ann T ((T -> Nothing) -> T)
                            maybe-length ... maybe-fill ... (clauses ...) body-expr))))]))

(define-syntax (for/vector: stx)
  (base-for/vector: stx #'for:))

(define-syntax (for*/vector: stx)
  (base-for/vector: stx #'for*:))
