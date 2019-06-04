#lang racket/base

;; Version 1, using DOS
#;(require dos)

;; Version 2, going direct
(begin
  (define 0x80 (make-continuation-prompt-tag 'dos))

  (define (run-process-until-syscall p st)
    (call-with-continuation-barrier
     (λ ()
       (call-with-continuation-prompt
        (λ () (p st))
        0x80
        (λ (x) x)))))

  (define (dos-syscall k->syscall)
    ;; First we capture our context back to the OS
    (call-with-current-continuation
     (λ (k)
       ;; Then we abort, give it to the OS, along with a syscall
       ;; specification
       (abort-current-continuation 0x80 (k->syscall k)))
     0x80))

  (define USE-OBSCENE-MEMORY? #t)
  (define (map-reduce f + a l)
    (if USE-OBSCENE-MEMORY?
      (cond
        [(null? l) a]
        [else
         (+ (f (car l)) (map-reduce f + a (cdr l)))])
      (cond
        [(null? l)
         a]
        [(pair? l)
         (+ (map-reduce f + a (car l))
            (map-reduce f + a (cdr l)))]
        [else
         (f l)])))

  (define USE-LOTS-OF-MEMORY? #t)
  (define (dos-boot merge-effects last-state ps empty-effects)
    (if USE-LOTS-OF-MEMORY?
      (map-reduce (λ (p) (run-process-until-syscall p last-state))
                  merge-effects
                  empty-effects
                  ps)
      (map (λ (p) (run-process-until-syscall p last-state)) ps))))

(define nothing-threads
  (for/list ([i (in-range 1900)])
    (λ (init-st)
      (let lp ()
        (dos-syscall (λ (k) k))
        (lp)))))

;; prevent inlining
(set! dos-boot dos-boot)

(begin
  (let lp ([ps nothing-threads] [max-use 0] [n 0] [grow-n 0])
    (printf "~s ~s ~s\n" max-use n grow-n)
    (unless (= n 200)
      (when (= grow-n 25)
        (error "memory use grew too many times"))
      (collect-garbage)
      (define use (current-memory-use))
      (lp (dos-boot cons #f ps null)
          (max use max-use)
          (add1 n)
          (if (use . > . max-use)
              (add1 grow-n)
              grow-n)))))
