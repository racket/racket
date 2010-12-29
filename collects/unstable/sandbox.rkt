#lang racket/base

(require racket/promise
         racket/sandbox)

(provide make-trusted-evaluator
         make-trusted-module-evaluator
         make-scribble-evaluator
         make-scribble-module-evaluator
         make-sandbox-namespace-specs)

(define make-trusted-evaluator
  (make-keyword-procedure
   (lambda (keys vals . args)
     (call-with-trusted-sandbox-configuration
      (lambda ()
        (keyword-apply make-evaluator keys vals args))))))

(define make-trusted-module-evaluator
  (make-keyword-procedure
   (lambda (keys vals . args)
     (call-with-trusted-sandbox-configuration
      (lambda ()
        (keyword-apply make-module-evaluator keys vals args))))))

(define make-scribble-evaluator
  (make-keyword-procedure
   (lambda (keys vals . args)
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string])
       (keyword-apply make-trusted-evaluator keys vals args)))))

(define make-scribble-module-evaluator
  (make-keyword-procedure
   (lambda (keys vals . args)
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string])
       (keyword-apply make-trusted-module-evaluator keys vals args)))))

(define (make-sandbox-namespace-specs make-ns . paths)

  (define parent
    (delay
      (let* ([ns (make-ns)])
        (parameterize ([current-namespace ns])
          (for ([path (in-list paths)])
            (dynamic-require path #f)))
        ns)))

  (define (make-child)
    (let* ([ns (make-ns)])
      (parameterize ([current-namespace ns])
        (for ([path (in-list paths)])
          (namespace-attach-module (force parent) path)))
      ns))

  (list make-child))
