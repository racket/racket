;;; Copyright (c) 2000-2015 Andrew W. Keep, R. Kent Dybvig
;;; See the accompanying file Copyright for details

(library (tests new-compiler)
  (export L0 parse-L0 unparse-L0)
  (import (rnrs) (nanopass) (tests helpers))

#|
  (compiler-passes '(
    parse-scheme                   ;; conversion? simplification? verification.
    convert-complex-datum          ;; conversion/simplification
    uncover-assigned               ;; analysis
    purify-letrec                  ;; conversion/simplification
    convert-assignments            ;; conversion
    optimize-direct-call           ;; optimization
    remove-anonymous-lambda        ;; conversion
    sanitize-binding-forms         ;; conversion/simplification
    uncover-free                   ;; analysis
    convert-closures               ;; conversion
    optimize-known-call            ;; optimization
    analyze-closure-size           ;; analysis
    uncover-well-known             ;; analysis (for optimization)
    optimize-free                  ;; optimization
    optimize-self-reference        ;; optimization
    analyze-closure-size           ;; analysis
    introduce-procedure-primitives ;; conversion
    lift-letrec                    ;; conversion
    normalize-context              ;; conversion
    specify-representation         ;; conversion
    uncover-locals                 ;; analysis
    remove-let                     ;; conversion
    verify-uil                     ;; verification
    remove-complex-opera*          ;; conversion
    flatten-set!                   ;; conversion
    impose-calling-conventions     ;; conversion
    expose-allocation-pointer      ;; conversion
    uncover-frame-conflict         ;; conversion
    pre-assign-frame               ;; 
    assign-new-frame
    (iterate
      finalize-frame-locations
      select-instructions
      uncover-register-conflict
      assign-registers
      (break when everybody-home?)
      assign-frame)
    discard-call-live
    finalize-locations
    expose-frame-var
    expose-memory-operands
    expose-basic-blocks
    #;optimize-jumps
    flatten-program
    generate-x86-64
    ))
|#

  (define vector-for-all
    (lambda (p? x)
      (let loop ([n (fx- (vector-length x) 1)])
        (cond
          [(fx<? n 0) #t]
          [(not (p? (vector-ref x n))) #f]
          [else (loop (fx- n 1))]))))

  (define target-fixnum?
    (lambda (x)
      (and (integer? x) (exact? x)
           (<= (- (ash 1 60)) x (- (ash 1 60) 1)))))

  (define constant?
    (lambda (x)
      (or (eq? x #t) (eq? x #f) (eq? x '()) (target-fixnum? x))))

  (define scheme-object?
    (lambda (x)
      (or (constant? x)
          (and (pair? x) (scheme-object? (car x)) (scheme-object? (cdr x)))
          (and (vector? x) (vector-for-all scheme-object? x)))))

  (define-language L0
    (terminals
      (constant (c))
      (scheme-object (d))
      (variable (x))
      (primitive (pr)))
    (Expr (e body)
      c
      x
      (quote d)
      (if e0 e1)
      (if e0 e1 e2)
      (and e* ...)
      (or e* ...)
      (begin e* ... e)
      (lambda (x* ...) body body* ...)
      (let ([x* e*] ...) body body* ...)
      (letrec ([x* e*] ...) body body* ...)
      (set! x e)
      (pr e* ...)
      (e0 e* ...)))
  )
