#lang racket/base
(provide cut-stack-at-checkpoint with-stack-checkpoint)

;; run a thunk, and if an exception is raised, make it possible to cut the
;; stack so that the surrounding context is hidden
(define checkpoints (make-weak-hasheq))
(define (call-with-stack-checkpoint thunk)
  (define checkpoint #f)
  (call-with-exception-handler
   (λ (exn)
     (when (and checkpoint ; just in case there's an exception before it's set
                (not (hash-has-key? checkpoints exn)))
       (hash-set! checkpoints exn checkpoint))
     exn)
   (lambda ()
     (set! checkpoint (current-continuation-marks))
     (thunk))))
;; returns the stack of the input exception, cutting off any tail that was
;; registered as a checkpoint
(define (cut-stack-at-checkpoint exn)
  (define stack (continuation-mark-set->context (exn-continuation-marks exn)))
  (define checkpoint
    (cond [(hash-ref checkpoints exn #f) => continuation-mark-set->context]
          [else #f]))
  (if (not checkpoint)
    stack
    (let loop ([st stack]
               [sl (length stack)]
               [cp checkpoint]
               [cl (length checkpoint)])
      (cond [(sl . > . cl) (cons (car st) (loop (cdr st) (sub1 sl) cp cl))]
            [(sl . < . cl) (loop st sl (cdr cp) (sub1 cl))]
            [(equal? st cp) '()]
            [else (loop st sl (cdr cp) (sub1 cl))]))))
  

(define-syntax-rule (with-stack-checkpoint expr)
  (call-with-stack-checkpoint (lambda () expr)))

