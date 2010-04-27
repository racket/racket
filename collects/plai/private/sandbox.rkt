#lang scheme
(require scheme/sandbox)

(define timeout/c (and/c integer? positive?))
(define memory-limit/c (and/c integer? positive?))
(define sandbox-result/c (or/c exn? (listof any/c)))
(define eval-expression/c any/c)

(define memory-accounting?
  (custodian-memory-accounting-available?))

(provide exn:fail:cpu-resource? exn:fail:cpu-resource-resource)
(define-struct (exn:fail:cpu-resource exn:fail) (resource))


(provide/contract
 (call-with-limits/cpu-time
  (timeout/c (-> any) . -> . any)))
(define (call-with-limits/cpu-time sec thunk)
  (let ([ch   (make-channel)]
        ;; use this to copy parameter changes from the sub-thread
        [p    current-preserved-thread-cell-values])
    (let* ([start-cpu-time (current-process-milliseconds)]
           ; cpu-time is modulo fixnum, so we may never reach end-cpu-time
           [end-cpu-time (+ start-cpu-time (* 1000 sec))]
           [work 
            (thread (lambda ()
                      (channel-put ch
                                   (with-handlers ([void (lambda (e)
                                                           (list (p) raise e))])
                                     (call-with-values thunk
                                                       (lambda vs (list* (p) values vs)))))))]
           [watch (thread 
                   (λ ()
                     (channel-put 
                      ch (let loop ([wait-sec 
                                     (quotient 
                                      (- end-cpu-time (current-process-milliseconds))
                                      1000)])
                           ; Wait for sec.  The process would have got < sec cpu-time.
                           (sync/timeout wait-sec work)
                           (if (>= (current-process-milliseconds) end-cpu-time)
                               'time
                               (loop (quotient 
                                      (- end-cpu-time (current-process-milliseconds))
                                      1000)))))))]
           [r (channel-get ch)])
      (kill-thread watch)
      (if (list? r)
          ;; apply parameter changes first
          (begin (p (car r)) (apply (cadr r) (cddr r)))
          (raise (make-exn:fail:cpu-resource "out of cpu time"
                                             (current-continuation-marks)
                                             r))))))


(provide evaluate/limits/cpu-time)
(define (evaluate/limits/cpu-time  evaluator memory-limit cpu-time-limit expr)
  (parameterize ([sandbox-eval-limits `(#f ,memory-limit)])
    (call-with-limits/cpu-time
     cpu-time-limit
     (λ () (evaluator expr)))))



#|(provide/contract 
   (sandbox-execution (timeout/c memory-limit/c eval-expression/c
                                 . -> . sandbox-result/c)))|#

(provide sandbox-execution)

(define-struct (exn:sandbox:unknown exn:fail) (value))

(define (sandbox-execution timeout memory-limit language requires body to-evaluate)
  (with-handlers ([exn? (λ (exn) exn)]
                  [(λ (x) #t) 
                   (λ (v) 
                     (make-exn:sandbox:unknown 
                      v "not a subclass of exn:fail"
                      (current-continuation-marks)))])
    
    (call-with-values
     (λ ()
       (parameterize ([sandbox-eval-limits `(,timeout ,memory-limit)])
         (let ([evaluator (make-evaluator language requires body)])
           (evaluator to-evaluate))))
     (λ results results))))

