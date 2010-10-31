#lang racket

(require racket/serialize
         racket/runtime-path
         compiler/zo-marshal)

(provide (all-defined-out))

(serializable-struct impl-timeout ())
(serializable-struct impl-rejected ())
(serializable-struct impl-exception (value))
(serializable-struct impl-answer (value))
(serializable-struct impl-clos-val ())
(serializable-struct impl-undefined-val ())

(define (eval-impl expr timeout)
  (let* ([p (zo-marshal expr)]
         [c (make-channel)]
         [t (thread
             (λ ()
               (parameterize ([read-accept-compiled #t])
                 (channel-put c (with-handlers ([exn:fail? values])
                                  (let ([val (eval (read (open-input-bytes p)))])
                                    (impl-answer
                                     (cond [(procedure? val) (impl-clos-val)]
                                           [(eq? (letrec ([x x]) x) val) (impl-undefined-val)]
                                           [else val]))))))))])
    
    (match (sync/timeout timeout c)
      [(and (? exn:fail?) (exn (regexp #rx"ill-formed code") _))
       (impl-rejected)]
      [(exn msg _) (impl-exception msg)]
      [#f (begin (kill-thread t) (impl-timeout))]
      [x x])))

(define-runtime-path impl-exec-path "impl-exec.rkt")

(define (eval-impl-external expr timeout)
  (let-values ([(in-in in-out) (make-pipe)]
               [(out-in out-out) (make-pipe)])
    (parameterize ([current-input-port in-in]
                   [current-output-port out-out])
      (write timeout in-out)
      (write (serialize expr) in-out)
      (if (system (format "racket -X ~a ~a"
                          (find-executable-path 
                           (find-system-path 'exec-file)
                           (find-system-path 'collects-dir))
                          impl-exec-path))
          (deserialize (read out-in))
          (error 'eval-impl-external "failed to evaluate ~a" expr)))))
