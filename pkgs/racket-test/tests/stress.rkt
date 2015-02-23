#lang racket
(provide stress
         fit)

; fit : string number (number -> any) #:slices number -> void
; Run f with slices different numbers between 0 and max, then print the summary
; timing with label
(define (fit label max f #:slices [slices 20])
  (fit-display
   label
   (for/list ([slice-n (in-range 1 (add1 slices))])
     (define i (round (* slice-n (/ max slices))))
     (define-values (cpu real gc)
       (isolate slice-n (λ () (f i))))
     (collect-garbage) (collect-garbage)
     (vector i cpu))))

(define (fit-display label l)
  (define baseline (vector-ref (findf (λ (v) (not (zero? (vector-ref v 1)))) l) 1))
  (printf "~a: baseline = ~a\n" label baseline)
  (for ([v (in-list l)])
    (match-define (vector n val) v)
    (printf "\t~a: ~ax\n" n (exact->inexact (/ val baseline)))))

(define (isolate trial-n thunk)
  (define exp-cust (make-custodian))
  (define-values (_ cpu real gc) 
    (parameterize ([current-custodian exp-cust])
      (time-apply thunk empty)))
  (custodian-shutdown-all exp-cust)
  (values cpu real gc))

; stress : number [string expr ...] ...
; Runs trials-expr different instances of each (begin expr ...) averaging the 
; timing and displaying them sorted by cpu time
(define-syntax-rule (stress trials-expr [label body ...] ...)
  (stress* trials-expr
           (cons label (λ () body ...))
           ...))

(define (cumulative-average ca x i)
  (+ ca (/ (- x ca) (add1 i))))

(define (run-experiment how-many exp)
  (match-define (cons label thunk) exp)
  (define-values
    (cpu real gc)
    (for/fold ([cpu0 0.0]
               [real0 0.0]
               [gc0 0.0])
      ([trial-n (in-range how-many)])
      (define-values (cpu1 real1 gc1)
        (isolate trial-n thunk))
      (when (zero? (modulo trial-n 5))
        (collect-garbage) (collect-garbage))
      (values (cumulative-average cpu0 cpu1 trial-n)
              (cumulative-average real0 real1 trial-n)
              (cumulative-average gc0 gc1 trial-n))))
  (vector label cpu real gc))

(define (stress* how-many . experiments)
  (stress-display
   how-many
   (sort-experiments 
    (for/list ([exp (in-list experiments)])
      (run-experiment how-many exp)))))

(define (sort-experiments l)
  (sort l <=
        #:key (λ (v) (vector-ref v 1))))

(define (stress-display how-many res)
  (define-values
    (min-cpu min-real min-gc)
  (for/fold ([min-cpu +inf.0]
             [min-real +inf.0]
             [min-gc +inf.0])
    ([v (in-list res)])
    (match-define (vector label cpu real gc) v)
    (printf "~a: cpu: ~a real: ~a gc: ~a (averaged over ~a runs)\n"
            label cpu real gc how-many)
    (values (min min-cpu cpu)
            (min min-real real)
            (min min-gc gc))))
  (define (norm min x)
    (if (zero? min)
        "inf"
        (real->decimal-string (/ x min))))
  (printf "Normalized:\n")
  (for ([v (in-list res)])
    (match-define (vector label cpu real gc) v)
    (printf "~a: cpu: ~a real: ~a gc: ~a (averaged over ~a runs)\n"
            label (norm min-cpu cpu) (norm min-real real) (norm min-gc gc) how-many))
  (newline))
