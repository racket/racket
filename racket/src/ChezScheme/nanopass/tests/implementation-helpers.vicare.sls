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
    (import (rnrs) (rnrs mutable-pairs) (only (vicare) void sub1 add1 remainder quotient)))

  (define interpret
    (lambda (src)
      (eval src (environment '(nanopass testing-environment)))))
  (define system
    (lambda (arg)
      (foreign-call "system" arg))))
