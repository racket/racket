#lang racket/base
(require '#%futures
         (for-syntax racket/base
                     syntax/for-body))

(provide future?
         future
         touch
         processor-count
         current-future
         fsemaphore?
         make-fsemaphore
         fsemaphore-count
         fsemaphore-post
         fsemaphore-wait
         fsemaphore-try-wait?
         would-be-future
         futures-enabled?
         for/async
         for*/async)

;; Note: order of touches not guaranteed.

(define-syntaxes (for/async for*/async)
  (let ()
    (define ((transformer for/fold/derived-id) stx)
      (syntax-case stx ()
        [(_ clauses body ...)
         (with-syntax ([this-syntax stx]
                       [for/fold/derived for/fold/derived-id]
                       [((pre-body ...)
                         (post-body ...))
                        (split-for-body stx #'(body ...))])
           (syntax/loc stx
             (let ([futures
                    (for/fold/derived this-syntax
                      ([fs null])
                      clauses
                      pre-body ...
                      (cons (future (lambda () post-body ...)) fs))])
               ;; touches futures in original order
               (let loop ([fs futures])
                 (when (pair? fs)
                   (loop (cdr fs))
                   (touch (car fs))))
               (void))))]))
    (values (transformer #'for/fold/derived)
            (transformer #'for*/fold/derived))))
