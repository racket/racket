#lang racket/base

#|

This file is shared between the original
namespace that drracket first starts with
any other namespaces that it loads,
so it keeps the requirements low (it could
be in the '#%kernel language, but 
drracket already shares mred/mred, so there
seems little point to that).

|#
  
(provide planet-terse-register
         planet-terse-log
         planet-terse-set-key) 

(define terse-log-message-chan (make-channel))
(define terse-log-proc-chan (make-channel))
(define log-key-tc (make-thread-cell (gensym) #t))
  
(define thd
  (thread
   (lambda ()
     (let ([procs (make-weak-hasheq)])
       (let loop ()
         (sync
          (handle-evt
           terse-log-message-chan
           (lambda (msg)
             (let ([registry (list-ref msg 0)]
                   [id (list-ref msg 1)]
                   [str (list-ref msg 2)])
               (for ([eph (in-list (hash-ref procs registry '()))])
                 (let ([proc (ephemeron-value eph)])
                   (when proc
                     (proc id str)))))
             (loop)))
          (handle-evt
           terse-log-proc-chan
           (lambda (rp)
             (let* ([registry (list-ref rp 0)]
                    [proc (list-ref rp 1)])
               (hash-update! procs
                             registry 
                             (lambda (x) (cons (make-ephemeron registry proc) x)) 
                             '())
               (loop))))))))))

(define (planet-terse-log id str)
  (unless (thread-dead? thd)
    (sync (channel-put-evt terse-log-message-chan (list (thread-cell-ref log-key-tc) id str))))
  (void))

(define (planet-terse-register proc)
  (sync (channel-put-evt terse-log-proc-chan (list (thread-cell-ref log-key-tc) proc)))
  (void))

(define (planet-terse-set-key new-key)
  (thread-cell-set! log-key-tc new-key))
