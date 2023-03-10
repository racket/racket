#lang racket/base
(require (for-template racket/base))
(provide datum-expr?
         datum-expr-value
         string-expr-value
         false/false-expr?
         allow-intern)

;; Recognize constant datum expressions (quote, self-quoting)

(define (datum-expr? e [ok? void])
  (define-values (quotable? v) (classify-expr e))
  (and quotable? (if ok? (ok? v) #t)))
(define (datum-expr-value e [ok? #f])
  (define-values (quotable? v) (classify-expr e))
  (and quotable? (if ok? (ok? v) #t) v))

(define (string-expr-value e)
  (datum-expr-value e string?))

(define (false/false-expr? e)
  (or (eq? e #f)
      (and (datum-expr? e) (eq? (datum-expr-value e) #f))))

;; ----------------------------------------

;; classify-expr : Syntax[Expr] -> (values #t Any) or (values #f #f)
(define (classify-expr e)
  (syntax-case e (quote quote-syntax #%datum list list* null)
    [(quote d)
     (values #t (syntax->datum #'d))]
    [(#%datum . d)
     (values #t (syntax->datum #'d))]
    [(list elem ...)
     (and (free-identifier=? (datum->syntax e '#%app) #'#%app)
          (syntax-property e 'allow-intern?))
     (classify-exprs (syntax->list #'(elem ...)))]
    [(list* elem ... rest)
     (and (free-identifier=? (datum->syntax e '#%app) #'#%app)
          (syntax-property e 'allow-intern?))
     (let ()
       (define-values (elems-quotable? elems-vs) (classify-exprs (syntax->list #'(elem ...))))
       (define-values (rest-quotable? rest-v) (classify-expr #'rest))
       (if (and elems-quotable? rest-quotable?)
           (values #t (append elems-vs rest-v))
           (values #f #f)))]
    [null
     (values #t null)]
    [d
     (and (syntax? e) (self-quoting-datum? (syntax-e #'d))
          (free-identifier=? (datum->syntax e '#%datum) #'#%datum))
     (values #t (syntax->datum #'d))]
    [_
     (cond [(eq? e #f) ;; FIXME: fix these cases at the source
            (values #t #f)]
           [else (values #f #f)])]))

;; classify-exprs : (Listof Syntax[Expr]) -> (values Boolean (U #f List))
(define (classify-exprs es)
  (define (loop es accvs)
    (cond [(pair? es)
           (define-values (quotable? v) (classify-expr (car es)))
           (if quotable?
               (loop (cdr es) (cons v accvs))
               (values #f #f))]
          [(null? es)
           (values #t (reverse accvs))]))
  (loop es null))

;; self-quoting-datum? : Any -> Boolean
(define (self-quoting-datum? v)
  (or (boolean? v) (number? v) (string? v) (bytes? v) (char? v)
      (regexp? v) (byte-regexp? v)))

;; allow-intern : Syntax[Expr] -> Syntax[Expr]
;; Mark an expression as internable (if the expression itself is suitable).
(define (allow-intern e)
  (syntax-property e 'allow-intern? #t))
