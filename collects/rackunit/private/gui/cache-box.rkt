#lang racket/base
(require racket/contract/base)

;; Add a new kind of promise instead?

;; FIXME: handle exceptions like promises?

(define (make-cache* thunk)
  (make-cache thunk #f))

(define (cache-ref cb)
  (let ([result (cache-result cb)])
    (if result
        (apply values result)
        (call-with-values (cache-thunk cb)
          (lambda result
            (set-cache-result! cb result)
            (apply values result))))))

(define (cache-invalidate! cb)
  (set-cache-result! cb #f))

(define (cache-printer cb port write?)
  (let ([result (cache-result cb)])
    (if result
        (fprintf port
                 (if write? "#<cache!~s>" "#<cache!~a>")
                 (if (and (pair? result) (null? (cdr result)))
                     (car result)
                     (cons 'values result)))
        (fprintf port "#<cache>"))))

(define-struct cache (thunk [result #:mutable])
  #:property prop:custom-write cache-printer)

(define-syntax-rule (cache* expr)
  (make-cache* (lambda () expr)))

(provide (rename-out [cache* cache]))
(provide/contract
 [rename make-cache* make-cache
  (-> (-> any) cache?)]
 [cache?
  (-> any/c boolean?)]
 [cache-ref
  (-> cache? any)]
 [cache-invalidate!
  (-> cache? any)])
