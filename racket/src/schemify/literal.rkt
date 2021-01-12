#lang racket/base
(require racket/unsafe/undefined
         racket/extflonum
         "wrap.rkt")

(provide literal?
         unwrap-literal
         wrap-literal
         register-literal-serialization)

(define (literal? v)
  (define u (unwrap v))
  (or (number? u)
      (boolean? u)
      (eq? u 'eof)
      (and (pair? u)
           (let ([a (unwrap (car u))])
             (cond
               [(eq? a 'quote)
                (let ([u (unwrap (cadr u))])
                  (or (and (symbol? u)
                           (or (symbol-interned? u)
                               (symbol-unreadable? u)))
                      (null? u)
                      (char? u)
                      (void? u)))]
               [(and (eq? a 'void)
                     (null? (cdr u)))
                #t]
               [else #f])))))

;; Unwrap a literal so that it can be serialized
;; or constant-folded
(define (unwrap-literal v)
  (define u (unwrap v))
  (cond
    [(pair? u)
     (let ([a (unwrap (car u))])
       (cond
         [(eq? a 'quote) (unwrap (cadr u))]
         [(eq? a 'void) (void)]))]
    [(eq? u 'eof) eof]
    [else u]))

(define (wrap-literal x)
  (cond
    [(or (string? x) (bytes? x) (boolean? x) (number? x))
     x]
    [(void? x) `(quote ,(void))]
    [(eof-object? x) 'eof]
    [else
     `(quote ,x)]))

(define (register-literal-serialization q serializable?-box datum-intern?)
  (let check-register ([q q] [seen #hasheq()])
    (define-syntax-rule (check-cycle new-seen e0 e ...)
      (cond
        [(hash-ref seen q #f)
         (raise-arguments-error 'compile "cannot compile cyclic value"
                                "value" q)]
        [else
         (let ([new-seen (hash-set seen q #t)])
           e0 e ...)]))
    (define (register! q)
      (unless (unbox serializable?-box)
        (set-box! serializable?-box (make-hasheq)))
      (hash-set! (unbox serializable?-box) q #t))
    (cond
      [(symbol? q)
       ;; gensyms need to be exposed to the whole linklet directory:
       (unless (or (symbol-interned? q)
                   (symbol-unreadable? q))
         (register! q))]
      [(or (null? q)
           (number? q)
           (char? q)
           (boolean? q)
           (eof-object? q)
           (void? q)
           (eq? q unsafe-undefined))
       (void)]
      [(or (string? q)
           (bytes? q))
       (when datum-intern?
         (register! q))]
      [(pair? q)
       (check-cycle
        seen
        (check-register (car q) seen)
        (check-register (cdr q) seen))]
      [(vector? q)
       (check-cycle
        seen
        (for ([e (in-vector q)])
          (check-register e seen)))]
      [(hash? q)
       (register! q)
       (check-cycle
        seen
        (for ([(k v) (in-hash q)])
          (check-register k seen)
          (check-register v seen)))]
      [(box? q)
       (check-cycle
        seen
        (check-register (unbox q) seen))]
      [(srcloc? q)
       (register! q)
       (srcloc-source q)]
      [(prefab-struct-key q)
       (register! q)
       (check-cycle
        seen
        (check-register (struct->vector q) seen))]
      [else
       (register! q)])))
