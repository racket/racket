#lang racket/base
(require (for-syntax racket/base)
         syntax/stx)

(provide (all-defined-out)
         (all-from-out syntax/stx))

(define (d->so template datum)
  (if (syntax? template)
      (datum->syntax template datum template template)
      datum))

(define (stx->datum x)
  (syntax->datum (datum->syntax #f x)))

(define-syntax (syntax-copier stx)
  (syntax-case stx ()
    [(syntax-copier hole expr pattern)
     #'(let ([expr-var expr])
         (lambda (in-the-hole)
           (with-syntax ([pattern expr-var])
             (with-syntax ([hole in-the-hole])
               (syntax/restamp pattern #'pattern expr-var)))))]))

(define-syntax syntax/skeleton
  (syntax-rules ()
    [(syntax/skeleton old-expr pattern)
     (syntax/restamp pattern #'pattern old-expr)]))


;; FIXME: Need to avoid turning syntax lists into syntax pairs
(define-syntax (syntax/restamp stx)
  (syntax-case stx (...)
    [(syntax/restamp (pa (... ...)) new-expr old-expr)
     #`(let ([new-parts (stx->list new-expr)]
             [old-parts (stx->list old-expr)])
         ;; FIXME 
         (unless (= (length new-parts) (length old-parts))
           (printf "** syntax/restamp\n~s\n" (quote-syntax #,stx))
           (printf "pattern : ~s\n" (syntax->datum #'(pa (... ...))))
           (printf "old parts: ~s\n" (map syntax->datum old-parts))
           (printf "new parts: ~s\n" (map syntax->datum new-parts)))
         (d->so
          old-expr
          (map (lambda (new old) (syntax/restamp pa new old))
               new-parts
               old-parts)))]
    [(syntax/restamp (pa . pb) new-expr old-expr)
     ;; FIXME 
     #'(begin
         (unless (and (stx-pair? new-expr) (stx-pair? old-expr))
           (printf "** syntax/restamp\n~s\n" (quote-syntax #,stx))
           (printf "pattern : ~s\n" (syntax->datum (quote-syntax (pa . pb))))
           (printf "old parts: ~s\n" old-expr)
           (printf "new parts: ~s\n" new-expr))
         (let ([na (stx-car new-expr)]
               [nb (stx-cdr new-expr)]
               [oa (stx-car old-expr)]
               [ob (stx-cdr old-expr)])
           (d->so old-expr
                  (cons (syntax/restamp pa na oa)
                        (syntax/restamp pb nb ob)))))]
    [(syntax/restamp pvar new-expr old-expr)
     #'new-expr]))

(define (iota n)
  (let loop ([i 0])
    (if (< i n)
        (cons i (loop (add1 i)))
        null)))

;; stx-take : syntax-list number -> (list-of syntax)
(define (stx-take items n)
  (cond [(zero? n) null]
        [else (cons (stx-car items) (stx-take (stx-cdr items) (sub1 n)))]))

(define (take-if-possible items n)
  (unless (number? n)
    (raise-type-error 'take-if-possible "number" n))
  (if (and (pair? items) (positive? n))
      (cons (car items) (take-if-possible (cdr items) (sub1 n)))
      null))

(define (reverse-take-if-possible items n)
  (define (loop items n acc)
    (if (and (pair? items) (positive? n))
        (loop (cdr items) (sub1 n) (cons (car items) acc))
        acc))
  (loop items n null))

(define (reverse-take-until items tail)
  (define (loop items acc)
    (if (and (pair? items) (not (eq? items tail)))
        (loop (cdr items) (cons (car items) acc))
        null))
  (loop items null))

;; stx-improper-length : syntax -> number
(define (stx-improper-length stx)
  (let loop ([stx stx] [n 0])
    (if (stx-pair? stx)
        (loop (stx-cdr stx) (add1 n))
        n)))

(define (stx->list* stx)
  (cond [(pair? stx)
         (cons (car stx) (stx->list* (cdr stx)))]
        [(null? stx)
         null]
        [(syntax? stx)
         (let ([x (syntax-e stx)])
           (if (pair? x)
               (cons (car x) (stx->list* (cdr x)))
               (list stx)))]
        [else null]))


(define (syntaxish? x)
  (or (syntax? x)
      (null? x)
      (and (pair? x)
           (syntaxish? (car x))
           (syntaxish? (cdr x)))))
