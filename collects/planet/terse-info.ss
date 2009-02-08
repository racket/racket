(module terse-info '#%kernel

  ;; This file is dynamically loaded by drscheme in a (possibly) 
  ;; empty namespace (ie possibly no scheme/base module yet)
  ;; and it is dynamically loaded by the planet resolver.
  
  (#%provide planet-terse-register planet-terse-log) 
  (define-values (terse-log-message-chan) (make-channel))
  (define-values (terse-log-proc-chan) (make-channel))
  
  (thread
   (lambda ()
     (letrec-values ([(loop) 
                      (lambda (procs)
                        (sync
                         (handle-evt
                          terse-log-message-chan
                          (lambda (msg)
                            (for-each (lambda (proc) (proc (car msg) (cdr msg))) procs)
                            (loop procs)))
                         (handle-evt
                          terse-log-proc-chan
                          (lambda (proc) 
                            (loop (cons proc procs))))))])
       (loop '()))))
  
  (define-values (planet-terse-log)
    (lambda (id str)
      (sync (channel-put-evt terse-log-message-chan (cons id str)))))
  
  (define-values (planet-terse-register)
    (lambda (proc)
      (sync (channel-put-evt terse-log-proc-chan proc)))))

