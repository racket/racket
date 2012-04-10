#lang scribble/base
@(require racket/sandbox
          scribble/eval)
@(define my-eval
  (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [print-as-expression #f])
        (make-evaluator "eval-datum-lang.rkt")))))

@interaction[#:eval my-eval
"hello world"
#"hi again"]
