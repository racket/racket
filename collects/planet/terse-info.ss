#lang scheme/base

#|

This file is shared between the original
namespace that drscheme first starts with
and other namespaces that it loads,
so it keeps the requirements low (it could
be in the '#%kernel language, but 
drscheme already shares mred/mred, so there
seems little point to that.

|#
  
(provide planet-terse-register planet-terse-log) 

(define-values (terse-log-message-chan) (make-channel))
(define-values (terse-log-proc-chan) (make-channel))
  
(define thd
  (thread
   (lambda ()
     (let ([procs (make-weak-hash)])
       (let loop ()
         (sync
          (handle-evt
           terse-log-message-chan
           (lambda (msg)
             (let ([namespace (list-ref msg 0)]
                   [id (list-ref msg 1)]
                   [str (list-ref msg 2)])
               (for-each (lambda (eph) 
                           (let ([proc (ephemeron-value eph)])
                             (when proc
                               (proc id str))))
                         (hash-ref procs namespace '())))
             (loop)))
          (handle-evt
           terse-log-proc-chan
           (lambda (pn)
             (let ([proc (list-ref pn 0)]
                   [namespace (list-ref pn 1)])
               (hash-update! procs
                             namespace 
                             (lambda (x) (cons (make-ephemeron namespace proc) x)) 
                             '())
               (loop))))))))))
  
(define (planet-terse-log id str [namespace (current-namespace)])
  (sync (channel-put-evt terse-log-message-chan (list namespace id str))))
  
(define (planet-terse-register proc [namespace (current-namespace)])
  (sync (channel-put-evt terse-log-proc-chan (list proc namespace))))
