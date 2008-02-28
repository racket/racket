#lang scheme/base

(provide define-provide-syntax )

(require (for-syntax scheme/base
                     scheme/provide-transform))

(define-for-syntax (make-provide-macro cert proc)
  (make-provide-transformer
   (lambda (stx modes)
     (let* ([i (make-syntax-introducer)]
            [new-stx (cert (i (proc (i stx))) i)])
       (expand-export new-stx modes)))))

(define-syntax (define-provide-syntax stx)
  (syntax-case stx ()
    [(_ id proc)
     (symbol? (syntax-e #'id))
     #'(define-syntax id
         (let ([cert (syntax-local-provide-certifier)])
           (make-provide-macro cert proc)))]))
