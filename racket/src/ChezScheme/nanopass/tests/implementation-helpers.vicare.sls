;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (tests implementation-helpers)
  (export time printf system interpret pretty-print format)
  (import (vicare))
  
  (library 
    (nanopass testing-environment)
    (export not < <= = boolean? char? eq? integer? null? pair? procedure?
            vector? zero? * + - add1 car cdr char->integer cons make-vector
            quotient remainder sub1 vector vector-length vector-ref void
            set-car! set-cdr! vector-set! quote set! if begin lambda let
            letrec)
    (import (rename (rnrs) (set! vicare:set!) (if vicare:if))
            (rnrs mutable-pairs)
            (rename (only (vicare) void sub1 add1 remainder quotient) (void vicare:void)))
    (define-syntax set!
      (syntax-rules ()
        [(_ x v) (call-with-values (lambda () (vicare:set! x v)) (case-lambda [() #!void] [(x) x]))]))
    (define-syntax if
      (syntax-rules ()
        [(_ t c) (call-with-values (lambda () (vicare:if t c)) (case-lambda [() #!void] [(x) x]))]
        [(_ t c a) (vicare:if t c a)]))
    (define-syntax void
      (syntax-rules ()
        [(_) (call-with-values (lambda () (vicare:void)) (case-lambda [() #!void] [(x) x]))])))

  (define interpret
    (lambda (src)
      ;; work around for vicare's strange handling of the return value of primitives like set!,
      ;; which apparently returns no values.
      (call-with-values (lambda () (eval src (environment '(nanopass testing-environment))))
        (case-lambda
          [() #!void]
          [(x) x]))))

  (define system
    (lambda (arg)
      (foreign-call "system" arg))))
