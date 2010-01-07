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
              (let ([e (apply f l)])
                (printf "result was ~a~n" e)
                e)))]
    [(_ f . args) (debug (f . args))]))