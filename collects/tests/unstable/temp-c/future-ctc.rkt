#lang racket
(require unstable/temp-c/dsl
         tests/eli-tester)

(define manual-strange/c
  (make-contract
   #:name "amazing contract"
   #:first-order procedure?
   #:projection
   (λ (b)
     (λ (x)
       (λ (f)
         (define ready? #f)
         (letrec-values
             ([(? o) 
               (x 
                (λ (y)
                  (cond
                    [(not ready?)
                     (raise-blame-error b x "cannot call until return")]
                    [(not (? y))
                     (raise-blame-error b x "expected a value of ~a" ?)]
                    [else
                     (f y)])))])
           (set! ready? #t)
           (values ? o)))))))

(define hot-strange/c
  (with-monitor
      (label 'strange
             (-> (label 'strategy
                        (-> any/c ; The monitor will make it tighter
                            any/c))
                 (values (-> any/c boolean?)
                         any/c)))
    (complement
     (union
      ; You can't call strategy until strange returns
      (seq (star _) (call 'strange _)
           (star (not (ret 'strange _ _)))
           (call 'strategy _))
      ; You can't call strategy with something that violates the predicate
      (seq (star _) (call 'strange _) (star _)
           (dseq (ret 'strange predicate? _)
                 (seq (star _)
                      (call 'strategy (not (? predicate?))))))))))

(define (try-it-out strange/c)
  (define strange-fun/ctc
    (contract strange/c 
              (λ (f)
                (values number? f))
              'pos 'neg))
  
  (define bad-strange-fun/ctc
    (contract strange/c 
              (λ (f)
                (f 4)
                (values number? f))
              'pos 'neg))
  
  (define-values (? o) (strange-fun/ctc (λ (x) x)))
  (test
   (o 4) => 4
   (o "string") =error> "broke"
   
   (bad-strange-fun/ctc (λ (x) x)) =error> "broke"))

(test
 ; ->i doesn't work
 (->i ([strategy (predicate?) (-> predicate? any/c)])
      (values [predicate? (-> any/c boolean?)]
              [object any/c]))
 =error>
 "an argument cannot depend on a result"
 
 ; but the manual version does
 (try-it-out manual-strange/c)
 ; and so does the temporal version
 (try-it-out hot-strange/c))
