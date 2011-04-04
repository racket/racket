#lang racket/base
(require scribble/core
         (for-syntax racket/base))

(provide cond-element
         cond-block)

(define-for-syntax (render-cond stx mk check-result no-matching-case)
  (syntax-case stx ()
    [(_ [test body0 body ...] ...)
     (let ([tests (syntax->list #'(test ...))])
       (with-syntax ([(test-expr ...)
                      (for/list ([test (in-list tests)]
                                 [pos (in-naturals)])
                        (let loop ([test test])
                          (syntax-case test (else and or not)
                            [else
                             (unless (= pos (sub1 (length tests)))
                               (raise-syntax-error
                                #f
                                "found `else' not in last clause"
                                stx
                                test))
                             #'#t]
                            [(and test ...)
                             #`(and . #,(map loop (syntax->list #'(test ...))))]
                            [(or test ...)
                             #`(or . #,(map loop (syntax->list #'(test ...))))]
                            [(not test)
                             #`(not #,(loop #'test))]
                            [id
                             (identifier? #'id)
                             #'(memq 'id mode)])))]
                     [mk mk]
                     [check-result check-result]
                     [no-matching-case no-matching-case])
         #'(mk
            (lambda (get put)
              (let ([mode (get 'scribble:current-render-mode 'text)])
                (cond
                 [test-expr (check-result (let () body0 body ...))]
                 ...
                 [else (no-matching-case)]))))))]))

(define-syntax (cond-block stx)
  (render-cond stx #'traverse-block #'check-block #'no-block-case))
                           
(define-syntax (cond-element stx)
  (render-cond stx #'traverse-element #'check-content #'no-element-case))

(define (check-block v)
  (unless (block? v)
    (raise-mismatch-error
     'cond-block
     "clause result is not a block: "
     v))
  v)

(define (check-content v)
  (unless (content? v)
    (raise-mismatch-error
     'cond-element
     "clause result is not content: "
     v))
  v)

(define (no-block-case)
  (raise (make-exn:fail:contract
          "cond-element: no clause matched"
          (current-continuation-marks))))

(define (no-element-case)
  (raise (make-exn:fail:contract
          "cond-element: no clause matched"
          (current-continuation-marks))))
