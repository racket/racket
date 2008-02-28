#lang scheme/base

(provide define-require-syntax)

(require (for-syntax scheme/base
                     scheme/require-transform))

(define-for-syntax (make-require-macro cert proc)
  (make-require-transformer
   (lambda (stx)
     (let* ([i (make-syntax-introducer)]
            [new-stx (cert (i (proc (i stx))) i)])
       (expand-import new-stx)))))

(define-syntax (define-require-syntax stx)
  (syntax-case stx ()
    [(_ id proc)
     (symbol? (syntax-e #'id))
     #'(define-syntax id
         (let ([cert (syntax-local-require-certifier)])
           (make-require-macro cert proc)))]))
