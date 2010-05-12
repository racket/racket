#lang racket/base

(provide debug)

;; printf debugging convenience
(define-syntax debug
  (syntax-rules ()
    [(_ (f . args))
     (begin (printf "starting ~a (~a)~n" 'f f)
            (let ([l (list . args)])
              (printf "arguments are:~n")
              (for/list ([arg 'args]
                         [val l])
                (printf "\t~a: ~a~n" arg val))
              (let ([e (with-handlers ([values (lambda (exn)
                                                 (printf "~a raised exception ~a~n" 'f exn)
                                                 (raise exn))])
                         (call-with-values (lambda () (apply f l)) list))])
                (if (and (pair? e) (null? (cdr e)))
                    (printf "~a result was ~a~n" 'f (car e))
                    (printf "~a results were ~a~n" 'f e))
                (apply values e))))]
    [(_ f . args) (debug (f . args))]))