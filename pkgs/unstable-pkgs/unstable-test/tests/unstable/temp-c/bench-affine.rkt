#lang racket

; The code
(define (raw f)
  (f (λ () (void))))

(define (client seed use-affine)
  (random-seed seed)
  (use-affine
   (λ (f)
     (define used? #f)
     (for ([i (in-range 10000)])
       (sync (alarm-evt (random 100)))
       (unless (or used? (zero? (random 3)))
         (set! used? #t)
         (f))))))

(define (bad-client use-affine)
  (use-affine
   (λ (f)
     (f)
     (f))))

; The benchmarks
(define ctc
  (contract
   (-> (-> (-> any/c) any/c) any/c)
   raw 'pos 'neg))

(define aff->
  (make-contract
   #:name 'affine
   #:first-order procedure?
   #:projection
   (λ (b)
     (λ (f)
       (define called? #f)
       (λ ()
         (when called?
           (raise-blame-error b f "called twice!"))
         (set! called? #t)
         (define x (f))
         x)))))
(define ad-hoc
  (contract
   (-> (-> aff-> any/c) any/c)
   raw 'pos 'neg))

(require unstable/temp-c/dsl)
(define (rgx)
  (contract
   (with-monitor 
    (-> (-> (label 'affine (-> any/c)) any/c) any/c)
    (complement
     (seq (star _)
          (call 'affine)
          (star _)
          (call 'affine))))
   raw 'pos 'neg))

; The runner
(require tests/stress)
(define seed (+ 1 (random (expt 2 30))))
(define-syntax-rule (STRESS version ...)
  (begin
    (with-handlers ([exn? (λ (x) (void))])
      (bad-client version)
      (printf "~a does not fail when it should\n" 'version))
    ...
    (with-handlers ([exn? (λ (x) 
                            (printf "~a fails when it should not\n" 'version))])
      (client seed version))
    ...
    (newline)
    (stress 4 
            [(format "~a" 'version)
             (client seed version)]
            ...)))

(STRESS raw ctc ad-hoc (rgx))


(module+ test
  (module config info
    (define random? #t)))
