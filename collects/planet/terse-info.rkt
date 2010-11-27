#lang scheme/base

#|

This file is shared between the original
namespace that drscheme first starts with
any other namespaces that it loads,
so it keeps the requirements low (it could
be in the '#%kernel language, but 
drscheme already shares mred/mred, so there
seems little point to that).

|#
  
(provide planet-terse-register
         planet-terse-log
         planet-terse-set-key
         planet-terse-log-key-param) 

(define terse-log-message-chan (make-channel))
(define terse-log-proc-chan (make-channel))
(define planet-terse-log-key-param (make-parameter (gensym)))
  
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
               (for-each (lambda (eph) 
                           (let ([proc (weak-box-value eph)])
                             (when proc
                               (proc id str))))
                         (hash-ref procs registry '())))
             (loop)))
          (handle-evt
           terse-log-proc-chan
           (lambda (rp)
             (let ([registry (list-ref rp 0)]
                   [proc (list-ref rp 1)])
               (hash-update! procs
                             registry 
                             (lambda (x) (cons (make-weak-box proc) x)) 
                             '())
               (loop))))))))))

(define (planet-terse-log id str [key (planet-terse-log-key-param)])
  (sync (channel-put-evt terse-log-message-chan (list key id str)))
  (void))
  
(define (planet-terse-register proc [key (planet-terse-log-key-param)])
  (sync (channel-put-evt terse-log-proc-chan (list key proc)))
  (void))

(define (planet-terse-set-key new-key)
  (planet-terse-log-key-param new-key))
