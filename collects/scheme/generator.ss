#lang scheme/base

(require (for-syntax scheme/base)
         scheme/control
         scheme/stxparam scheme/splicing)

(provide yield generator in-generator infinite)

;; (define-syntax-parameter yield
;;   (lambda (stx)
;;     (raise-syntax-error
;;      #f "yield is only bound inside a sequence generator")))

;; (define (procedure->generator proc)
;;   (define tag (make-continuation-prompt-tag))
;;   (define (cont)
;;     (reset-at tag
;;       (let ([r (proc (lambda (r) (shift-at tag k (set! cont k) r)))])
;;         ;; normal return:
;;         (set! cont (lambda () r))
;;         r)))
;;   (lambda () (cont)))

;; not using parameterization
#;
(define-syntax-rule (generator body0 body ...)
  (let ([tag (make-continuation-prompt-tag)])
    (define yielder
      (let ([yield (lambda (value) (shift-at tag k (set! cont k) value))])
        yield))
    (splicing-syntax-parameterize ([yield (make-rename-transformer #'yielder)])
      (define (cont)
        (reset-at tag
          (let ([retval (begin body0 body ...)])
            ;; normal return:
            (set! cont (lambda () retval))
            retval))))
    (define (generator) (cont))
    generator))

(define current-yielder
  (make-parameter
   (lambda (v)
     (error 'yield "must be called in the context of a generator"))))

(define (yield value)
  ((current-yielder) value))

(define yield-tag (make-continuation-prompt-tag))

(define-syntax-rule (generator body0 body ...)
  (let ()
    (define (cont)
      (define (yielder value)
        (shift-at yield-tag k (set! cont k) value))
      (reset-at yield-tag
        (parameterize ([current-yielder yielder])
          (let ([retval (begin body0 body ...)])
            ;; normal return:
            (set! cont (lambda () retval))
            retval))))
    (define (generator) (cont))
    generator))

(define stop-value (gensym))

(define-sequence-syntax in-generator
  (syntax-rules ()
    [(_ body0 body ...)
     (in-producer (generator body0 body ... stop-value) stop-value)])
  (lambda (stx)
    (syntax-case stx ()
      [((id ...) (_ body0 body ...))
       #'[(id ...)
          (in-producer (generator body0 body ... stop-value) stop-value)]])))

(define (infinite sequence)
  (generator
    (for ([i (in-cycle sequence)])
         (yield i))))

#|
;; examples
(for/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]) i)
(for*/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]
            [j (in-generator (yield 'X) (yield '-))])
  (list i j))
|#
