#lang racket/base
(require tests/stress
         unstable/temp-c/dsl
         racket/match
         racket/contract)

(define (id x) x)

(define raw 
  id)
(define ctc 
  (contract (-> integer? integer?) id
            'pos 'neg))
(define monitor-ctc
  (contract (monitor/c (λ (x) #t) 'f
                       (-> integer? integer?))
            id 'pos 'neg))
(define monitor-ctc+atomic
  (contract (monitor/c
             (let ([called? #f])
               (match-lambda
                 [(? monitor:proj? x)
                  #t]
                 [(? monitor:call? x)
                  (begin0 (not called?)
                          (set! called? #t))]
                 [(? monitor:return? x)
                  (begin0 called?
                          (set! called? #f))]))
             'f
             (-> integer? integer?))
            id 'pos 'neg))
(define dsl-ctc
  (contract (with-monitor (-> integer? integer?))
            id 'pos 'neg))
(define dsl-ctc+atomic
  (contract (with-monitor (label 'f (-> integer? integer?))
              (seq (? monitor:proj?)
                   (star 
                    (seq (call 'f _)
                         (ret 'f _)))
                   ; We need this to preserve prefix-closure
                   (opt (call 'f _))))
            id 'pos 'neg))

(define-syntax-rule (stress-it x ver ...)
  (let ([x* x])
    (printf "Running ~a iterations\n" x*)
    (stress 1
            [(symbol->string 'ver)
             (printf "Running ~a\n" 'ver)
             (for ([i (in-range x*)])
               (ver 1))]
            ...)))

(stress-it 
 (expt 10 4)
 raw
 ctc
 monitor-ctc
 monitor-ctc+atomic
 dsl-ctc
 dsl-ctc+atomic)
