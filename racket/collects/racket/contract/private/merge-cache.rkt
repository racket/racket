#lang racket/base

(provide define/merge-cache)

(require (for-syntax racket/base))

;; weak hashtable never cleared
(define MERGE-CACHE (make-thread-cell (make-weak-hasheq)))

(require (for-syntax racket/base))
(define-syntax (define/merge-cache stx)
  (syntax-case stx ()
    [(_ (merge-name new-se new-neg old-se old-neg) body ...)
     #'(define (merge-name new-se new-neg old-se old-neg)
         (call-with-merge-cache new-se new-neg old-se old-neg
           (let ([merge-name (λ () body ...)])
             merge-name)))]))

(define (call-with-merge-cache new-se new-neg old-se old-neg body-thunk)
  (define the-cache (thread-cell-ref MERGE-CACHE))
  (define h1 (hash-ref the-cache new-se #f))
  (cond
    [(and h1 (ephemeron-value h1))
     =>
     (λ (h1)
       (define h2 (hash-ref h1 new-neg #f))
       (cond
         [(and h2 (ephemeron-value h2))
          =>
          (λ (h2)
            (define h3 (hash-ref h2 old-se #f))
            (cond
              [(and h3 (ephemeron-value h3))
               =>
               (λ (h3)
                 (define cached-result (hash-ref h3 old-neg #f))
                 (cond
                   [(and cached-result (ephemeron-value cached-result)) => values]
                   [else
                    (define result (body-thunk))
                    (hash-set! h3 old-neg (make-ephemeron old-neg result))
                    result]))]
              [else
               (define result (body-thunk))
               (define h3 (make-hasheq (list (cons old-neg (make-ephemeron old-neg result)))))
               (hash-set! h2 old-se (make-ephemeron old-se h3))
               result]))]
         [else
          (define result (body-thunk))
          (define h3 (make-hasheq (list (cons old-neg (make-ephemeron old-neg result)))))
          (define h2 (make-hasheq (list (cons old-se (make-ephemeron old-se h3)))))
          (hash-set! h1 new-neg (make-ephemeron new-neg h2))
          result]))]
    [else
     (define result (body-thunk))
     (define h3 (make-hasheq (list (cons old-neg (make-ephemeron old-neg result)))))
     (define h2 (make-hasheq (list (cons old-se (make-ephemeron old-se h3)))))
     (define h1 (make-hasheq (list (cons new-neg (make-ephemeron new-neg h2)))))
     (hash-set! the-cache new-se (make-ephemeron new-se h1))
     result]))
