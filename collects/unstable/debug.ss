#lang scheme/base

(provide debug)

;; printf debugging convenience
(define-syntax debug
  (syntax-rules ()
    [(_ (f . args))
     (begin (printf "starting ~a~n" 'f)
            (let ([l (list . args)])
              (printf "arguments are:~n")
              (for/list ([arg 'args]
                         [val l])
                (printf "\t~a: ~a~n" arg val))
              (let ([e (with-handlers ([values (lambda (exn)
                                                 (printf "~a raised exception ~a~n" 'f exn)
                                                 (raise exn))])
                         (apply f l))])
                (printf "~a result was ~a~n" 'f e)
                e)))]
    [(_ f . args) (debug (f . args))]))