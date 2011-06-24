#lang racket/base
(require tests/stress
         racket/system
         racket/runtime-path)

(define racket-pth
  (find-executable-path "racket"))

(define-runtime-path ttt:raw "ttt-bench-raw.rkt")
(define-runtime-path ttt:ctc "ttt-bench-ctc.rkt")

racket-pth

(define (bench p)
  (system* racket-pth "-t" p))

(define-syntax-rule (stress-it ver ...)
  (let ([x* 1])
    (printf "Running ~a iterations\n" x*)
    (stress 100
            [(symbol->string 'ver)
             (printf "Running ~a\n" 'ver)
             (for ([i (in-range x*)])
               (bench ver))]
            ...)))

(stress-it 
 ttt:raw
 ttt:ctc)
