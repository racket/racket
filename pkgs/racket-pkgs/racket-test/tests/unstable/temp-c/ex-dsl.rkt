#lang racket
(require unstable/temp-c/dsl
         tests/eli-tester)

(define (test-spec spec f)
  (define i 0)
  (define MallocFreeImpl
    (cons (λ () (begin0 i (set! i (add1 i))))
          (λ (a) (void))))
  (define MallocFreeProt
    (contract spec MallocFreeImpl
              'pos 'neg))
  
  (match-define (cons malloc free) MallocFreeProt)
  (f malloc free))

(define (good malloc free)
  (define a (malloc))
  (free a)
  (define e (malloc))
  (define f (malloc))
  (free e)
  (free f)
  (define c (malloc))
  (define d (malloc))
  (free d)
  (free c))

(define (bad malloc free)
  (define b (malloc))
  (free b)
  (free b))

(define addr? number?)

(define NoFreeSpec
  (with-monitor 
      (cons/c (label 'malloc (-> addr?))
              (label 'free (-> addr? void?)))
    ; It is okay as long as you never call free
    (complement (seq (star _) (call 'free _) (star _)))))
(test (test-spec NoFreeSpec good) =error> "disallowed"
      (test-spec NoFreeSpec bad) =error> "disallowed")

(define NoFreeTwiceSpec
  (with-monitor 
      (cons/c (label 'malloc (-> addr?))
              (label 'free (-> addr? void?)))
    (complement (seq (star _) (call 'free _) (star _) (call 'free _) (star _)))))
(test (test-spec NoFreeTwiceSpec good) =error> "disallowed"
      (test-spec NoFreeTwiceSpec bad) =error> "disallowed")

(define MallocFreeBalancedSpec
  (with-monitor 
      (cons/c (label 'malloc (-> addr?))
              (label 'free (-> addr? void?)))
    (star 
     (seq (call 'malloc)
          (ret 'malloc _)
          (call 'free _)
          (ret 'free _)))))
(test (test-spec MallocFreeBalancedSpec good) =error> "disallowed" 
      (test-spec MallocFreeBalancedSpec bad) =error> "disallowed")

(define MallocFreeSpec
  (with-monitor 
      (cons/c (label 'malloc (-> addr?))
              (label 'free (-> addr? void?)))
    (complement (seq (star _)
                     (call 'free _)
                     (star (not (ret 'malloc _)))
                     (call 'free _)))))
(test (test-spec MallocFreeSpec good) =error> "disallowed" 
      (test-spec MallocFreeSpec bad) =error> "disallowed")

(define MallocFreeSpecNQ
  (with-monitor 
      (cons/c (label 'malloc (-> addr?))
              (label 'free (-> addr? void?)))
    (complement 
     (seq (star _)
          (call 'free x)
          (star (not (ret 'malloc x)))
          (call 'free x)))))
(test (test-spec MallocFreeSpecNQ good) =error> "disallowed" 
      (test-spec MallocFreeSpecNQ bad) =error> "disallowed")

(require unstable/match)
(define MallocFreeSpecQ
  (with-monitor 
      (cons/c (label 'malloc (-> addr?))
              (label 'free (-> addr? void?)))
    (complement 
     (seq (star _)
          (dseq (call 'free addr)
                (seq
                 (star (not (ret 'malloc (== addr))))
                 (call 'free (== addr))))))))
(test (test-spec MallocFreeSpecQ good)
      (test-spec MallocFreeSpecQ bad) =error> "disallowed")
