#lang racket/base
(require '#%futures
         (for-syntax racket/base))

(provide  future?
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
        [(_ (clause ...) . body)
         (quasisyntax/loc stx
           (let ([futures
                  (#,for/fold/derived-id #,stx ([fs null]) (clause ...)
                                         (cons (future (lambda () . body)) fs))])
             ;; touches futures in original order
             (let loop ([fs futures])
               (cond [(pair? fs) (begin (loop (cdr fs)) (touch (car fs)))]
                     [else (void)]))))]))
    (values (transformer #'for/fold/derived)
            (transformer #'for*/fold/derived))))
