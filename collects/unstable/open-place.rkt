#lang racket/base

(require (for-syntax syntax/parse racket/base syntax/free-vars racket/syntax)
         racket/place)

(provide open-place)

(define-syntax (open-place stx)
  (syntax-parse stx
    [(_ ch:id body:expr ...)
     (define b #'(let () body ...))
     (define/with-syntax b* (local-expand b 'expression null))
     (define/with-syntax (fvs ...) (free-vars #'b*))
     (define/with-syntax (i ...) (for/list ([(v i) (in-indexed (syntax->list #'(fvs ...)))]) i))
     (define/with-syntax (v p) (generate-temporaries '(v p)))
     #'(let ()
         (define p (place ch (let* ([v (place-channel-get ch)]
                                    [fvs (vector-ref v i)] ...)
                               b*)))
         (define vec (vector fvs ...))
         (for ([e (in-vector vec)]
               [n (in-list (syntax->list (quote-syntax (fvs ...))))])
           (unless (place-message-allowed? e)
             (raise-arguments-error 'open-place 
                                    "free variable values must be allowable as place messages"
                                    (symbol->string (syntax-e n)) e)))
         (place-channel-put p vec)
         p)]))
