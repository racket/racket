#lang racket/base

(require racket/promise (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax ~ (make-rename-transformer #'lazy))
(define ! force)
(define ~? promise?)

;; force a top-level list structure; works with improper lists (will force the
;; dotted item when it checks if it's a pair); does not handle cycles
(define (!list x)
  (let ([x (! x)])
    (if (list? x) ; cheap check,
      x           ; and big savings on this case
      (let loop ([x x])
        (if (pair? x)
          ;; avoid allocating when possible
          (let ([r (loop (! (cdr x)))]) (if (eq? r (cdr x)) x (cons (car x) r)))
          x)))))

;; similar to !list, but also force the values in the list
(define (!!list x)
  (let ([x (! x)])
    (if (list? x)                   ; cheap check,
      (if (ormap ~? x) (map ! x) x) ; and big savings on these cases
      (let loop ([x x])
        (if (pair? x)
          ;; avoid allocating when possible
          (if (~? (car x))
            (cons (! (car x)) (loop (! (cdr x))))
            (let ([r (loop (! (cdr x)))])
              (if (eq? r (cdr x)) x (cons (car x) r))))
          x)))))

(define (!! x)
  ;; Recursively force the input value, preserving sharing (usually indirectly
  ;; specified through self-referential promises).  The result is a copy of the
  ;; input structure, where the scan goes down the structure that
  ;; `make-reader-graph' handles.
  (define t (make-weak-hasheq))
  (define placeholders? #f)
  (define (loop x)
    (let ([x (! x)])
      ;; * Save on placeholder allocation (which will hopefully save work
      ;;   recopying values again when passed through `make-reader-graph') --
      ;;   basic idea: scan the value recursively, marking values as visited
      ;;   *before* we go inside; when we get to a value that was marked,
      ;;   create a placeholder and use it as the mark (or use the mark value
      ;;   if it's already a placeholder); finally, if after we finished
      ;;   scanning a value -- if we see that its mark was changed to a
      ;;   placeholder, then put the value in it.
      ;; * Looks like we could modify the structure if it's mutable instead of
      ;;   copying it, but that might leave the original copy with a
      ;;   placeholder in it.
      (define-syntax-rule (do-value expr)
        (let ([y (hash-ref t x #f)])
          (cond ;; first visit to this value
                [(not y) (hash-set! t x #t)
                         (let* ([r expr] [y (hash-ref t x #f)])
                           (when (placeholder? y)
                             (placeholder-set! y r)
                             (set! placeholders? #t))
                           r)]
                ;; already visited it twice => share the placeholder
                [(placeholder? y) y]
                ;; second visit => create a placeholder request
                [else (let ([p (make-placeholder #f)]) (hash-set! t x p) p)])))
      ;; deal with only with values that `make-reader-graph' can handle (for
      ;; example, no mpairs) -- otherwise we can get back placeholder values
      ;; (TODO: hash tables)
      (cond [(pair? x)
             (do-value (cons (loop (car x)) (loop (cdr x))))]
            [(vector? x)
             (do-value (let* ([len (vector-length x)] [v (make-vector len)])
                         (for ([i (in-range len)])
                           (vector-set! v i (loop (vector-ref x i))))
                         (if (immutable? x) (vector->immutable-vector v) v)))]
            [(box? x)
             (do-value ((if (immutable? x) box-immutable box)
                        (loop (unbox x))))]
            [else
             (let ([k (prefab-struct-key x)])
               (if k
                 (do-value (let ([v (struct->vector x)])
                             (for ([i (in-range 1 (vector-length v))])
                               (vector-set! v i (loop (vector-ref v i))))
                             (apply make-prefab-struct k
                                    (cdr (vector->list v)))))
                 x))])))
  (let ([x (loop x)]) (if placeholders? (make-reader-graph x) x)))
