#lang racket/base
(require racket/list
         racket/match
         racket/local
         racket/contract
         racket/async-channel)

(define current-worker (make-parameter #f))

(define-struct job-queue (async-channel))
(define-struct job (paramz thunk))
(define-struct done ())

(define (make-queue how-many)
  (define jobs-ch (make-async-channel))
  (define work-ch (make-async-channel))
  (define done-ch (make-async-channel))
  (define (working-manager spaces accept-new? jobs continues)
    (if (and (not accept-new?)
             (empty? jobs)
             (empty? continues))
        (killing-manager how-many)
        (apply
         sync
         (if (and accept-new?
                  (not (zero? spaces)))
             (handle-evt
              jobs-ch
              (match-lambda
                [(? job? the-job)
                 (working-manager (sub1 spaces) accept-new?
                                  (list* the-job jobs) continues)]
                [(? done?)
                 (working-manager spaces #f jobs continues)]))
             never-evt)
         (handle-evt
          done-ch
          (lambda (reply-ch)
            (working-manager spaces accept-new? 
                             jobs (list* reply-ch continues))))
         (if (empty? jobs)
             never-evt
             (handle-evt
              (async-channel-put-evt work-ch (first jobs))
              (lambda (_)
                (working-manager spaces accept-new?
                                 (rest jobs) continues))))
         (map
          (lambda (reply-ch)
            (handle-evt
             (async-channel-put-evt reply-ch 'continue)
             (lambda (_)
               (working-manager (add1 spaces) accept-new? 
                                jobs (remq reply-ch continues)))))
          continues))))
  (define (killing-manager left)
    (unless (zero? left)
      (sync
       (handle-evt
        done-ch
        (lambda (reply-ch)
          (async-channel-put reply-ch 'stop)
          (killing-manager (sub1 left)))))))
  (define (worker i)
    (match (async-channel-get work-ch)
      [(struct job (paramz thunk))
       (call-with-parameterization 
        paramz 
        (lambda () 
          (parameterize ([current-worker i])
            (thunk))))
       (local [(define reply-ch (make-async-channel))]
         (async-channel-put done-ch reply-ch)
         (local [(define reply-v (async-channel-get reply-ch))]
           (case reply-v
             [(continue) (worker i)]
             [(stop) (void)]
             [else
              (error 'worker "Unknown reply command")])))]))
  (define the-workers
    (for/list ([i (in-range 0 how-many)])
      (thread (lambda ()
                (worker i)))))
  (define the-manager
    (thread (lambda () (working-manager how-many #t empty empty))))
  (make-job-queue jobs-ch))

(define (submit-job! jobq thunk)
  (async-channel-put
   (job-queue-async-channel jobq)
   (make-job (current-parameterization)
             thunk)))

(define (stop-job-queue! jobq)
  (async-channel-put
   (job-queue-async-channel jobq)
   (make-done)))

(provide/contract
 [current-worker (parameter/c (or/c false/c exact-nonnegative-integer?))]
 [job-queue? (any/c . -> . boolean?)]
 [rename make-queue make-job-queue 
         (exact-nonnegative-integer? . -> . job-queue?)]
 [submit-job! (job-queue? (-> any) . -> . void)]
 [stop-job-queue! (job-queue? . -> . void)])
