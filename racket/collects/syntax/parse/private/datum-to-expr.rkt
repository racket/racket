#lang racket/base
(provide datum->expression)

;; datum->expression : Datum -> Syntax[Expr]
;; Produces code that evaluates (at same phase!) to an equivalent value.
;; (Note: to produce phase-0 expr from phase-1 value, this module would
;; need to require racket/base for-template.)
(define (datum->expression v)
  (define (const v) `(quote ,v))
  (define (const? e) (and (pair? e) (eq? (car e) 'quote)))
  (define (loop v)
    (cond [(syntax? v)
           `(quote-syntax ,v)]
          [(pair? v)
           (cond [(and (list? v) (andmap syntax? v))
                  `(syntax->list (quote-syntax ,(datum->syntax #f v)))]
                 [else
                  (define outer-v v)
                  (let pairloop ([v v] [acc null])
                    (cond [(pair? v)
                           (pairloop (cdr v) (cons (loop (car v)) acc))]
                          [(null? v)
                           (cond [(andmap const? acc) (const outer-v)]
                                 [else `(list ,@(reverse acc))])]
                          [else
                           (let ([acc (cons (loop v) acc)])
                             (cond [(andmap const? acc) (const outer-v)]
                                   [else `(list* ,@(reverse acc))]))]))])]
          [(vector? v)
           (let ([elem-es (map loop (vector->list v))])
             (cond [(andmap const? elem-es) (const v)]
                   [else `(vector ,@elem-es)]))]
          [(prefab-struct-key v)
           => (lambda (key)
                (define elem-es (map loop (cdr (vector->list (struct->vector v)))))
                (cond [(andmap const? elem-es) (const v)]
                      [else `(make-prefab-struct (quote ,key) ,@elem-es)]))]
          ;; FIXME: boxes, hashes?
          [else
           (const v)]))
  (datum->syntax #'here (loop v)))
