#lang scheme

(provide plai-complete-program)
(define (plai-complete-program port settings reader language-module)
  (let ([state 'init])
        ;; state : 'init => 'require => 'done
    (lambda ()
      (case state
        [(init)
         (set! state 'require)
         (let ([body-exps 
                (let loop ()
                  (let ([result (reader (object-name port) port)])
                    (if (eof-object? result)
                        null
                        (cons result (loop)))))])
           (expand
            (datum->syntax
             #f
             `(,#'module #%plai ,language-module
                         ,@body-exps))))]
        [(require) 
         (set! state 'done)
         (syntax
          (let ([done-already? #f])
            (dynamic-wind
             void
             (lambda ()
               (dynamic-require ''#%plai #f))
             (lambda () 
               (unless done-already?
                 (set! done-already? #t)
                 (current-namespace (module->namespace ''#%plai)))))))]
        [(done) eof]))))
