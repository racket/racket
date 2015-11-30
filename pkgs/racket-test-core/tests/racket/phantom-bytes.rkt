#lang racket/base

;; An extra test of phantom bytes.

(define (make-one)
  (make-phantom-bytes (expt 2 29)))

(define pbs (list (make-one)))

(define (check)
  (unless (> (current-memory-use) (* (length pbs)
                                     (expt 2 29)))
    (error "failed"))
  (for ([pb (in-list pbs)])
    (set-phantom-bytes! pb 0))
  (unless (< (current-memory-use) (expt 2 29))
    (error "failed after zeros:" (current-memory-use)))
  (for ([pb (in-list pbs)])
    (set-phantom-bytes! pb (expt 2 29)))
  (unless (> (current-memory-use) (* (length pbs)
                                     (expt 2 29)))
    (error "failed after restore:" (current-memory-use))))

(check)
(collect-garbage)
(check)

(define mem (make-bytes (* 250 1024 1024)))
(check)
(collect-garbage)
(check)
(set! pbs (cons (make-one) pbs))
(check)
(collect-garbage)
(check)
(collect-garbage)
(check)

(void (bytes-length mem))

'ok

(module test racket/base
  (require compiler/find-exe
           racket/system)
  
  (define exe (find-exe))
  (unless (system* exe "-l" "tests/racket/phantom-bytes")
    (error "run failed"))
  
  ;; Also try in incremental mode
  (void (putenv "PLT_INCREMENTAL_GC" "yes"))
  (unless (system* exe "-l" "tests/racket/phantom-bytes")
    (error "run failed")))
